{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

-- | This module implements the main interpretation function
module Grace.Interpret
    ( -- * Interpret
      Input(..)
    , ImportCallback
    , interpret
    , interpretWith
    , interpretExprWith
    , parseInput

      -- * Errors related to interpretation
    , InterpretError(..)
    ) where

import Control.Exception.Safe (Exception(..), catchAny)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (Bifunctor(..))
import Data.Generics.Product (the)
import Grace.Import (Import)
import Data.Text (Text)
import Grace.Import (ImportCallback)
import Grace.Location (Location(..))
import Grace.Syntax (Node(..), Syntax(..))
import Grace.Type (Type)
import Grace.Value (Value)
import System.FilePath ((</>))

import qualified Control.Lens         as Lens
import qualified Control.Monad.Except as Except
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified Grace.Context        as Context
import qualified Grace.Import         as Import
import qualified Grace.Infer          as Infer
import qualified Grace.Monotype       as Monotype
import qualified Grace.Normalize      as Normalize
import qualified Grace.Parser         as Parser
import qualified Grace.Syntax         as Syntax
import qualified Grace.Type           as Type
import qualified System.FilePath      as FilePath
import qualified Text.URI             as URI

{-| Input to the `interpret` function

    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath
    -- ^ The path to the code
    | Code String Text
    -- ^ Source code: @Code name content@

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret
    :: (MonadError InterpretError m, MonadIO m)
    => Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Input
    -> m (Type Location, Value)
interpret = interpretWith (Import.resolverToCallback mempty) []

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (MonadError InterpretError m, MonadIO m)
    => ImportCallback
    -- ^ How to resolve URI imports
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Input
    -> m (Type Location, Value)
interpretWith resolveImport bindings maybeAnnotation input = do
    let directory = case input of
            Path file -> FilePath.takeDirectory file
            Code _ _  -> "."

    expression <- parseInput input

    interpretExprWith resolveImport bindings maybeAnnotation directory expression

{- | Like `interpretWith`, but takes a pre-parsed expression instead of some
     source code
-}
interpretExprWith
    :: (MonadError InterpretError m, MonadIO m)
    => ImportCallback
    -- ^ How to resolve URI imports
    -> [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> FilePath
    -- ^ The base directory used when resolving filepath imports of relative paths
    -> Syntax Location Import
    -> m (Type Location, Value)
interpretExprWith resolveImport bindings maybeAnnotation directory expression = do
    resolvedExpression <- flip traverse (annotate expression) \(maybeAnnotation', import_) -> case import_ of
        Import.File file ->
            interpretWith resolveImport bindings maybeAnnotation' input
                where
                    input = Path (FilePath.normalise (directory </> file))

        Import.URI uri Nothing -> do
            eitherResult <- liftIO do
                (Right <$> resolveImport Nothing uri) `catchAny` (return . Left)

            importExpression <- case eitherResult of
                Left e -> throwError (ImportError uri (displayException e))
                Right result -> return result

            let relocate location = location { name = Text.unpack (URI.render uri) }

            interpretExprWith resolveImport bindings maybeAnnotation' directory (first relocate importExpression)

        Import.URI uri (Just metadata) -> do
            let locate offset = (Syntax.location expression) { offset = offset }

            let locatedMetadata = first locate metadata

            let annotation = Just (Type.Type
                    { location = Syntax.location locatedMetadata
                    , node = Type.Scalar Monotype.JSON
                    })

            (_, metadata') <- interpretExprWith resolveImport bindings annotation directory locatedMetadata

            eitherResult <- liftIO do
                (Right <$> resolveImport (Just metadata') uri) `catchAny` (return . Left)

            importExpression <- case eitherResult of
                Left e -> throwError (ImportError uri (displayException e))
                Right result -> return result

            let relocate location = location { name = Text.unpack (URI.render uri) }

            interpretExprWith resolveImport bindings maybeAnnotation' directory (first relocate importExpression)

    let annotatedExpression =
            case maybeAnnotation of
                Nothing         -> resolvedExpression
                Just annotation ->
                    Syntax
                        { node = Annotation resolvedExpression annotation
                        , location = Syntax.location resolvedExpression
                        }

    let typeContext = do
            (variable, type_, _) <- bindings

            return (Context.Annotation variable type_)

    case Infer.typeWith typeContext annotatedExpression of
        Left message -> do
            Except.throwError (TypeInferenceError message)

        Right inferred -> do
            let evaluationContext = do
                    (variable, _, value) <- bindings

                    return (variable, value)

            return (inferred, Normalize.evaluate evaluationContext resolvedExpression)

-- | Like `Parser.parse`, but expects an `Input` and returns a located expression
parseInput
    :: (MonadError InterpretError m, MonadIO m)
    => Input
    -> m (Syntax Location Import)
parseInput input = do
    code <- case input of
        Path file   -> liftIO (Text.IO.readFile file)
        Code _ text -> return text

    let name = case input of
            Path file -> file
            Code n _  -> n

    case Parser.parse name code of
        Left message -> do
            Except.throwError (ParseError message)

        Right expression -> do
            let locate offset = Location{..}

            return (first locate expression)

{-| We use this utility so that when we resolve an import of the form:

    > ./someImport.ffg : SomeType

    … then the type-annotation is used when type-checking the import.  This
    allows the user to supply an expected type to fix imports that would
    otherwise not type-check in isolation.  You can think of this function as
    \"pushing\" the type annotation into the imported expression.

    This is particularly useful when importing JSON.  For example, suppose
    that we had the following JSON expression:

    > [ 1, true ]

    We can't interpret that directly because it is a type error, and we also
    can't import that without a type annotation for the same reason.  However,
    we can import the JSON like this:

    > ./example.json : List (exists (a : Type) . a)

    … and the expression will succeed since the type annotation is used when
    type-checking @./example.json@.  We wouldn't be able to add that same type
    annotation directly to @./example.json@ because then it would no longer be
    valid JSON.
-}
annotate :: Syntax s a -> Syntax s (Maybe (Type s), a)
annotate = Lens.transform transformSyntax . fmap ((,) Nothing)
  where
    transformSyntax = Lens.over (the @"node") transformNode

    transformNode (Annotation Syntax{ node = Embed (_, a) } annotation) =
        Embed (Just annotation, a)
    transformNode node =
        node

-- | Errors related to interpretation of an expression
data InterpretError
    = ImportError URI.URI String
    | ParseError Parser.ParseError
    | TypeInferenceError Infer.TypeInferenceError
    deriving stock (Eq, Show)

instance Exception InterpretError where
    displayException (ImportError uri e) =
        "The import of " <> Text.unpack (URI.render uri) <> " failed with: " <>
        e
    displayException (ParseError e) = displayException e
    displayException (TypeInferenceError e) = displayException e

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- | This module contains the functions and types that power to URI-base imports
-}

module Grace.Import
    ( Import(..)
    , ImportCallback
    , Resolver(..)
    , resolverToCallback
    , ImportError(..)
    ) where

import Control.Exception.Safe (Exception(..), throw)
import Grace.Location (Location, Offset)
import Grace.Pretty (Pretty(..))
import Grace.Syntax (Syntax)
import Grace.Value (Value)
import Prettyprinter ((<+>))
import Text.URI (URI)

{- | A reference to external source code that will be imported by the
     interpreter.
-}
data Import
    = File FilePath
    | URI URI (Maybe (Syntax Offset Import))

instance Pretty Import where
    pretty (File file) = pretty file
    pretty (URI uri Nothing) = pretty uri
    pretty (URI uri (Just metadata)) = pretty uri <+> "with" <+> pretty metadata

-- | Type of the callback function used to resolve URI imports
type ImportCallback = Maybe Value -> URI -> IO (Syntax Location Import)

{- | A resolver for an URI.

     When the interpreter tries to resolve an URI pointing to some source code
     it will try multiple resolvers sequentially and stops if one returns a
     @Just code@ value where @code@ is the source code of an expression.
     It will then try to parse and interpret that expression.

     Here are some good practices for the development of resolvers:

     * A resolver should handle exactly one URI scheme.

     * If a resolver encounters an URI which it cannot process (e.g. a
       @file://@ URI is passed to a HTTP resolver) it should return @Nothing@
       as fast as possible.

     * Exceptions thrown in resolvers will be caught and rethrown as an
       `ImportError` by the interpreter. The error will contain the URI so there
       is no need to include in your exception.
-}
newtype Resolver = Resolver { runResolver :: Maybe Value -> URI -> IO (Maybe (Syntax Location Import)) }

instance Semigroup Resolver where
    x <> y = Resolver \metadata uri -> do
        maybeResult <- runResolver x metadata uri
        case maybeResult of
            Nothing -> runResolver y metadata uri
            _ -> return maybeResult

instance Monoid Resolver where
    mempty = Resolver (\_ _ -> return Nothing)

-- | Convert a resolver to a callback function
resolverToCallback :: Resolver -> ImportCallback
resolverToCallback resolver metadata uri = do
    maybeResult <- runResolver resolver metadata uri
    case maybeResult of
        Nothing -> throw UnsupportedURI
        Just result -> return result

-- | Errors that might be raised during import resolution.
data ImportError
    = UnsupportedURI
    deriving stock Show

instance Exception ImportError where
    displayException UnsupportedURI = "Resolving this URI is not supported"

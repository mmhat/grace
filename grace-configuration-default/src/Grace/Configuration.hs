module Grace.Configuration
    ( resolveImport
    ) where

import Grace.Import (ImportCallback)
import Grace.Resolver.Builtin (defaultResolver)

import qualified Grace.Import as Import

resolveImport :: ImportCallback
resolveImport = Import.resolverToCallback defaultResolver

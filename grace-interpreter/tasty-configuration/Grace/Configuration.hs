module Grace.Configuration
    ( resolveImport
    ) where

import Grace.Import (ImportCallback)

import qualified Grace.Import as Import

resolveImport :: ImportCallback
resolveImport = Import.resolverToCallback mempty

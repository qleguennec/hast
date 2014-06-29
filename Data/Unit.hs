module Data.Unit (
    Unit(..)
    , getPath
) where

import Data.Workspace

data Unit = Unit WP String
 deriving (Eq)

instance Show Unit where
    show = getPath

getPath :: Unit -> FilePath
getPath (Unit (WP path) name) = path ++ "/" ++ name

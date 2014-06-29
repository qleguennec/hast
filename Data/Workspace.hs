module Data.Workspace (
    WP(..)
) where

data WP = WP FilePath
 deriving (Eq, Show)

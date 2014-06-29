module Config.Workspace (
    workspace
) where

import Config.Reader
import Data.ConfigFile
import Data.Workspace

workspace :: ConfigParser -> WP
workspace cp = WP $ getString cp "workspace" "root"

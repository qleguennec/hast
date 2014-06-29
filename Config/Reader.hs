module Config.Reader (
    readConf
    , getString
) where

import Data.ConfigFile
import Data.Either.Utils
import Control.Monad.Except

confFile = "/home/moka/workspace/hast/hast.config"

readConf :: IO (ConfigParser)
readConf = readfile emptyCP confFile >>= runExceptT >>= \(Right a) -> return a

getString :: ConfigParser -> SectionSpec -> OptionSpec -> String
getString cp sec opt = forceEither $ get cp sec opt

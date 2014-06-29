import System.Environment
import System.Exit
import System.Process

import Data.Workspace ()
import Data.Unit
import Data.List

import Config.Reader
import Config.Workspace

version :: [Char]
version = "0.2.0.0"

help :: IO ()
help = putStrLn . init . foldl (++) "Usage: \n" . map (\(n, (_, h)) -> n ++ " -> " ++ h ++ "\n") $ args

type ArgDef = (String, (IO (), String))

args :: [ArgDef]
args = [
      ("-v", (putStrLn version, "Print version and exit"))
    , ("-h", (help            , "Print help and exit"   ))]

searchArg :: String -> Maybe (ArgDef)
searchArg s = case filter (\(n, _) -> n == s) args of
    [] -> Nothing
    xs -> Just . head $ xs

getCommand :: Maybe (ArgDef) -> IO ()
getCommand a = case a of
    Nothing          -> putStrLn ("Unknown command") >> die 1
    Just (_, (c, _)) -> c >> exit

main :: IO ()
main = getArgs >>= mapM (parse) >>= process

exit :: IO ()
exit = exitWith ExitSuccess

die :: Int -> IO ()
die = exitWith . ExitFailure

data Arg    = Arg String | Action String
 deriving (Eq, Show)
data Parser = ArgParser Arg | UnitParser Unit
 deriving (Eq, Show)

parse :: String -> IO (Parser)
parse s = case s of
    arg@('-':_)  -> return . ArgParser . Arg $ arg
    ('@':unit) -> do
        wp <- readConf >>= return . workspace
        return . UnitParser $ Unit wp unit
    fs -> return . ArgParser . Action $ fs

process :: [Parser] -> IO ()
process [] = exit

process ((UnitParser u):xs) = let
    unitArgs = takeWhile (\case
        (UnitParser _) -> False
        (ArgParser a)  -> case a of
            (Arg _)    -> False
            (Action _) -> True) xs
    in processWithUnit u unitArgs >> process (xs \\ unitArgs)

process ((ArgParser arg):xs)   = case arg of 
    (Arg a)    -> (getCommand . searchArg $ a)>> process xs
    (Action a) -> putStrLn ("No unit specified on action " ++ a) >> die 2

processWithUnit :: Unit -> [Parser] -> IO ()
processWithUnit u []      = (putStrLn . getPath) u >> rawSystem "cd" [getPath u] >>= exitWith
processWithUnit u (x:xs)  = print x

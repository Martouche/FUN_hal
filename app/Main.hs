module Main where

import           Lib
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

end :: String -> Bool
end str = all (\x -> elem x ['\n', ' ', '\t']) str

doHAL :: String -> (Maybe String, String)
doHAL str =
  case parse str parseExpr of
    Nothing          -> (Nothing, str)
    Just (res, rest) -> (fmap toString (hal res), rest)

main :: IO ()
main = do
  args <- getArgs
  foldl (\_ filename -> readAllAndInterp filename) (return ()) (filter (\x -> (head x) /= '-') args)
  if elem "-i" args
    then commandPromt
    else return ()

commandPromt :: IO ()
commandPromt = do
  putStr "> "
  hFlush stdout
  eof <- hIsEOF stdin
  if eof
    then putChar '\n'
    else do
      line <- getLine
      cmdLineInterp line
      commandPromt

cmdLineInterp :: String -> IO ()
cmdLineInterp str =
  case doHAL str of
    (Nothing, _) -> putErrLn "Error"
    (Just res, rest) -> do
      putStrLn res
      if end rest
        then return ()
        else cmdLineInterp rest

readAllAndInterp :: String -> IO ()
readAllAndInterp filename =
  catchIOError
    (withFile
       filename
       ReadMode
       (\handle -> do
          content <- hGetContents handle
          fileInterp content))
    (\err -> do
       putStrLn $ show err
       exitWith $ ExitFailure 84)

fileInterp :: String -> IO ()
fileInterp str =
  case doHAL str of
    (Nothing, _) -> do
      putErrLn "Error"
      exitWith $ ExitFailure 84
    (Just res, rest) -> do
      if end rest
        then putStrLn res
        else fileInterp rest

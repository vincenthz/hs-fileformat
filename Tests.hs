import Data.FileFormat
import System.Environment
import Control.Monad
import qualified Control.Exception as E

printType file = getAndPrint `E.catch`
    (\e -> putStrLn (show (e :: E.SomeException)))
    where getAndPrint = getFileformat file >>= \ty -> putStrLn (file ++ ": " ++ show ty)

main = getArgs >>= mapM_ printType
{-
	let file = args !! 0
	ft <- getFiletype file
	putStrLn $ show ft
-}

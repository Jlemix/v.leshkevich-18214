import Control.Monad.State
import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import System.Environment

main :: IO ()
main = do
   inHandle <- openFile "input.txt" ReadMode
  
   putStrLn "input consequence"
   line <- getLine
  
   contents <- hGetContents inHandle

   let transit tr = ((read (words tr !! 0) :: Int , head (words tr !! 1)) , read (words tr !! 2) :: Int)

   let listState = map (\w -> read w :: Int) (words (head (lines contents)))
       ts = tail $ lines contents
       prv = foldl (\prevPrv ts -> (transit ts):prevPrv) [] ts

   let processInt prv c handle = do
         cur <- get
         when (isNothing $ lookup (cur, c) prv) $ lift $ do
           putStrLn "shish"
           hClose handle

   element <- foldl (\cur c -> cur >>= execStateT (processInt prv c inHandle)) (return 0) line
  
   if (elem element listState) then putStrLn "norm tema ksta" else putStrLn "meehhhhh"
   hClose inHandle

import Control.Monad
import Data.List
import System.IO

main = do
   input <- openFile "input.txt" ReadMode
   output <- openFile "output.txt" WriteMode
   contents <- hGetContents input
   let sorted = sort contents
   let symb str = [(smb, num) | smb <- (nub str), let num = length(filter (== smb) str)]	  
   let counter (smb, num) = [smb] ++ " - " ++ (show num) ++ "\n"
   mapM (hPutStr output) (foldr (\acc str -> (counter acc):str) [] (symb sorted))     
   hClose input
   hClose output
import HeapSort

readibleForm [] = []
readibleForm (x:xs) = (show x) ++ " " ++ (readibleForm xs)

main = do
       string <- getLine
	   
       let sos = (heapSort(string))
       putStrLn (readibleForm(sos))

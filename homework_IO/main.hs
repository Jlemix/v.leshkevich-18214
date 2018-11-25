import HeapSort

main = do
       string <- getLine
	   
       let sos = (heapSort(string))
       putStrLn (heapSort sos)
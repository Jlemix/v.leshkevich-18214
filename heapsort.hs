heapSort :: [Int] -> [Int]
heapSort [] = []
heapSort xs = heapSort (take (length xs - 1) (swapped xs)) ++ [head (buildMaxHeap xs)]
    where		
		swapped xs = swap 1 (length xs) (buildMaxHeap xs)
		swap :: Int -> Int -> [a] -> [a]
		swap m n list = take ((min m n) - 1) list ++ [list !! ((max m n) - 1)] ++ (drop m (take ((max m n) - 1) list)) ++ [list !! ((min m n) - 1)] ++ drop (max m n) list
		buildMaxHeap :: [Int] -> [Int]
		buildMaxHeap xs = foldr heapify xs ([1 .. (length xs) `div` 2])
			where
				heapify :: Int -> [Int] -> [Int]
				heapify i arr = if largest i /= i then heapify (largest i) (swap (largest i) i arr) else arr
					where
						iLeftChild i = 2*i
						iRightChild i = 2*i + 1
						big_iLeftChild i j = if iLeftChild i <= length arr && (arr !! ((iLeftChild i) - 1)) > (arr !! (j-1)) then iLeftChild i else j
						big_iRightChild i j = if iRightChild i <= length arr && (arr !! ((iRightChild i) - 1)) > (arr !! (j-1)) then iRightChild i else j
						largest i = big_iRightChild i (big_iLeftChild i i)

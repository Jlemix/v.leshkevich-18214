heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort xs = heapSort (take (length xs - 1) (swapped xs)) ++ [head (buildMaxHeap xs)]
    where 
		--swap :: Int -> Int -> [a] -> [a] --Swap the first element of the list with the final element
		swap m n list = take ((min m n) - 1) list ++ [list !! ((max m n) - 1)] ++ (drop (min m n) (take ((max m n) - 1) list)) ++ [list !! ((min m n) - 1)] ++ drop (max m n) list
		swapped xs = swap 1 (length xs) (buildMaxHeap xs)
		--buildMaxHeap :: [Int] -> [Int]
		buildMaxHeap xs = foldr heapify xs ([1 .. (length xs) `div` 2]) --builds a heap from a list
			where
				--heapify :: Int -> [Int] -> [Int] --Build the heap in array so that largest value is at the root
				heapify i arr = if largest i /= i then heapify (largest i) (swap (largest i) i arr) else arr
					where --leafSearch
						iLeftChild i = 2*i
						iRightChild i = 2*i + 1
						big_iLeftChild i j = if iLeftChild i <= length arr && (arr !! ((iLeftChild i) - 1)) > (arr !! (j-1)) then iLeftChild i else j
						big_iRightChild i j = if iRightChild i <= length arr && (arr !! ((iRightChild i) - 1)) > (arr !! (j-1)) then iRightChild i else j
						largest i = big_iRightChild i (big_iLeftChild i i)

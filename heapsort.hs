swap :: Int -> Int -> [a] -> [a]
swap m n list = take (i1 - 1) list ++ [list !! (i2 - 1)] ++ (drop i1 (take (i2 - 1) list)) ++ [list !! (i1 - 1)] ++ drop i2 list
    where i1 = min m n
          i2 = max m n

heapSort :: [Int] -> [Int]
heapSort [] = []
heapSort xs = heapSort (take (length xs - 1) (swapped xs)) ++ [head (buildMaxHeap xs)]
    where 
		swapped xs = swap 1 (length xs) (buildMaxHeap xs)
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
						

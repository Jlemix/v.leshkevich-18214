--1. get xs n — получим n-й произвольный элемент списка xs, начиная с нулевого;

myGet :: [a]->Integer->a
myGet (x:xs) n | n == 0 = x
	        | otherwise = myGet xs (n-1)
--error

--2. head xs — вернет первый элемент списка xs;

myHead :: [a]->a
myHead [] = error "Empty list"
myHead (x:_) = x

--3. last xs — вернет последний элемент списка xs;

myLast :: [a]->a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

--4. tail xs — вернет список xs без первого элемента;

myTail :: [a]->[a]
myTail [] = error "Empty list"
myTail [x] = []
myTail (_:xs) = xs

--5. init xs — вернет список xs без последнего элемента;

myInit :: [a]->[a]
myInit [] = error "Empty list"
myInit [x] = []
myInit (x:xs) = x : myInit xs -- myInit [1,2,3] => 1 : myInit [2,3] => 1 : 2 : myInit [3] => 1 : 2 : [] => [1,2]

--6. reverse xs — вернет обратный список;

myReverse :: [a]->[a]
myReverse [] = error "Empty list"
myReverse [x] = [x]
myReverse (x:xs) = myConcat (myReverse xs) [x] -- myReverse [3,4,5] => myReverse [4,5] ++ [3] => myReverse [5] ++ [4] ++ [3] => [5] ++ [4] ++ [3]
--ne optimal'no

--7. length xs — вернет длину списка xs;

myLength :: [a]->Integer
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs -- myLength [1,2,3] => 1 + myLength [2,3] => 1 + 1 + myLength [3] => 1 + 1 + 1

--8. append xs x — добавит x в качестве последнего элемента к списку xs;

myAppend :: [a]->a->[a]
myAppend [] x = [x]
myAppend (hed:xs) x = hed : (myAppend xs x) -- myAppend [1,2] 3 => 1 : myAppend [2] 3 => 1 : 2 : 3 => [1,2,3]


--9. concat xs ys — конкатенация двух списков list1 и list2;

myConcat :: [a]->[a]->[a]
myConcat [] ys = ys
myConcat xs [] = xs
myConcat (x:xs) ys = x : myConcat xs ys -- myConcat [1,2] [3,4] => 1 : myConcat [2] [3,4] => 1 : 2 : [3,4] => [1,2,3,4]


--10. drop n xs — вернет список, где удалены первых n элементов из списка xs;

myDrop :: Integer->[a]->[a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (_:xs) = myDrop (n-1) xs -- myDrop 2 [1,2,3] => myDrop 1 [2,3] => myDrop 0 [3] => [3]

--11. take n xs — вернет список из первых n элементов из списка xs;

myTake :: Integer->[a]->[a]
myTake n [] = []
myTake 0 xs = []
myTake n (x:xs) = x : (myTake (n-1) xs) --myTake 2 [1,2,3] => 1 : myTake 1 [2,3] => 1 : 2 : myTake 0 [3] => [1,2]

--12. splitAt n xs — вернет пару списков, полученных из списка xs разбиением c n-й позиции (причем n-й элемент войдет во второй список);

mySplitAt :: Integer->[a]->([a],[a])
mySplitAt n [] = ([],[])
mySplitAt 0 xs = ([],xs)
mySplitAt n (x:xs) = (x : list1, list2)
					where 
						(list1,list2) = mySplitAt (n - 1) xs -- mySplitAt 2 [1,2,3,4] => 1 : mySplitAt 1 [2,3,4] => (1 : 2 : [], [3,4]) => ([1,2],[3,4])

-- 13. null xs — проверяет, пуст ли список xs (возвращает True или False);

myNull :: [a]->Bool
myNull [] = True
myNull (_:_) = False

--14. elem xs x — проверит, лежит ли элемент x в списке xs (возвращает True или False);

myElem :: Eq a=>[a]->a->Bool
myElem [] x = False
myElem (y:xs) x = if y == x then True else myElem xs x

--15. filter test xs — вернет только те элементы списка, которые удовлетворяют условию test (с аналогичными выше требованиями);

myFilter :: (a->Bool)->[a]->[a]
myFilter test [] = []
myFilter test (x:xs) = if test x then x : (myFilter test xs) else myFilter test xs -- myFilter (>3) [1,2,3,4] => >3 1 => myFilter [2,3,4] => ... => >3 4 => 4 : myFilter test [] => [4]

--16. map f xs — применит функцию f ко всем элементам списка xs и вернет новый список;

myMap :: (a->b)->[a]->[b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

--17. zip xs ys — вернет список пар вида (x,y), в каждой из которых первый элемент x является очередным элементом первого списка xs, а второй элемент y — второго списка ys;

myZip :: [a]->[b]->[(a,b)]
myZip [] [] = []
myZip xs [] = []
myZip [] ys = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys -- myZip [1,2,3] [9,8,7] => (1,9) : myZip [2,3] [8,7] => (1,9) : (2,8) : myZip [3,7] => (1,9) : (2,8) : (3,7) : [] => [(1,9),(2,8),(3,7)]

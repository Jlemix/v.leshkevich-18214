module HashTable where

import Data.Char
import Data.List
import Data.Int
import Data.Bits
import GHC.Int 

--ord na eq, izmenit insert'

data HT_Element key val = Equal | Element (key,val) [HT_Element key val] deriving (Show, Eq, Ord)

data HashTable key val = HashTable [HT_Element key val] Integer deriving (Show)

defaultHashTable :: HashTable key val
defaultHashTable = HashTable [] 0

fromList :: (Show key, Eq key)=>[(key,val)]->HashTable key val
fromList xs = HashTable (construct xs) (toInteger(length xs))
				where
					construct [] = []
					construct ((key,val):xs) = (Element (key,val) []):(construct xs)

clear :: HashTable key val -> HashTable key val
clear table = defaultHashTable

hash :: String -> Int32
hash = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + hashInt m
         magic = 0xdeadbeef
         golden :: Int32
         golden = 1013904242
         hashInt :: Int32 -> Int32
         hashInt x = mulHi x golden + x
              where
                mulHi :: Int32 -> Int32 -> Int32
                mulHi a b = fromIntegral (r `shiftR` 32)
	              	where 
			              r :: Int32
			              r = fromIntegral a * fromIntegral b

erase::(Show key, Eq key)=>HashTable key val->key->HashTable key val
erase (HashTable xs len) key = HashTable (destroy xs key) (len-1)
	where
		destroy [] _ = []
		destroy ((Element (key,val) next):xs) find | hash (show key) == hash (show find) = xs 
							   | otherwise = (Element (key, val) next):(destroy xs find)
							   
insert' :: (Show key, Eq key) => HashTable key val -> key -> val -> HashTable key val
insert' (HashTable xs len) key val = HashTable (elem xs key val) (len + 1)
					where
						elem [] find value = [Element (find, value) []]
						elem ((Element (key,val) next):xs) find value | hash (show key) == hash (show find) && key == find = (Element (key,val) ((Element (find,value) [Equal]):next)):xs
--											      | hash (show key) == hash (show find) && key < find = (Element (key,val) next):(elem xs find value)
											      | otherwise = (Element (find,value) []):(Element (key,value) next):xs
contains :: (Show key, Eq key) => HashTable key val -> key -> Bool
contains (HashTable xs len) key = lookup xs key
					where
						lookup [] _ = False
						lookup ((Element (key,val) next):xs) find | hash (show key) == hash (show find) && key == find = True
									                  | otherwise = lookup xs find
at :: (Show key, Eq key) => HashTable key val -> key -> val
at (HashTable xs len) key = retrn xs key
				where
					retrn [] _ = error "Can not find value by this key"
					retrn ((Element (key,val) next):xs) find | hash (show key) == hash (show find) = val
									         | otherwise = retrn xs find
size :: (Show key, Eq key) => HashTable key val -> Integer
size (HashTable xs len) = len

empty :: (Show key, Eq key) => HashTable key val -> Bool
empty (HashTable xs len) | len == 0 = True
			 | otherwise = False


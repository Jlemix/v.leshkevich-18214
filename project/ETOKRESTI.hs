import Data.List
import Data.Char
import qualified Data.Time.Clock.POSIX as Random (getPOSIXTime)

main = do 
 putStr "-------------------------------------------------------------------\n"
 putStr "                                                                  |"
 putStr "\nHAAAAI BRATISHKA, PROTIV KOGO RYBAT' BYDESH? 'f' - dryg, 'b' - bot|\n"
 putStr "                                                                  |"
 putStrLn "\n------------------------------------------------------------------|\n\n"
 line <- getLine
 if line == "f" then
  game initialField 'X' 88
 else if line == "b" then
  game initialField 'X' 42
 else do
  putStrLn "a, ny togda davai (f ili b??????)"
  main
  
initialField = ["...", 
                "...", 
                "..."]

type Field = [String]
type Move = (Int, Int)

game :: Field -> Char -> Int -> IO()
game field pSymb check = do
 if (check == 42 && pSymb == 'O') then do
  m <- round `fmap` Random.getPOSIXTime
  let ai = aiMove field m
  let newfield = fst (mmove field ai pSymb)
  if winCond newfield pSymb == True then do
    putStrLn ("\n\n\n KRESTIKI-NOLIKI\n-----------------\n" ++ fieldLook newfield ++ "\n-----------------\n" ++ "\n\nmister " ++ [pSymb] ++ " ti proigral...")
    playMore
  else if tie newfield ai == True then do
    putStrLn ("\n\n\n KRESTIKI-NOLIKI\n-----------------\n" ++ fieldLook newfield ++ "\n-----------------\n" ++ "\n\nnikto ne pobedil...")
    playMore
  else
    game newfield (next pSymb) check
 else do
    putStrLn ("\n\n\n KRESTIKI-NOLIKI\n-----------------\n" ++ fieldLook field ++ "\n-----------------\nMister " ++ [pSymb] ++ " - teper vash hod, vvod : xy, gde x - stolbec, y - stroka.")
    shish <- getLine
    let playerTurn = plTurn shish
    if fst playerTurn /= (666,666) then do
      let checkfield = mmove field (fst playerTurn) pSymb
      if snd checkfield == True then do
        let newfield_ = fst checkfield
        if (winCond newfield_ pSymb) == True then do
          putStrLn ("\n\n\n KRESTIKI-NOLIKI\n-----------------\n" ++ fieldLook newfield_ ++ "\n-----------------\n" ++ "\n\nMISTER " ++ [pSymb] ++ " TI POBEDIL!!!!!")
          playMore
        else if tie newfield_ (fst playerTurn) == True then do
          putStrLn ("\n\n\n KRESTIKI-NOLIKI\n-----------------\n" ++ fieldLook newfield_ ++ "\n-----------------\n" ++ "\n\nnikto ne pobedil...")
          playMore
        else
          game newfield_ (next pSymb) check
      else do
        putStrLn "Wrong move, retry"
        game field pSymb check
    else do
      putStrLn "Wrong move, retry"
      game field pSymb check

plTurn :: [Char] -> (Move, Bool)
plTurn s | (elem (s!!0) ['0'..'9']) && (elem (s!!1) ['0'..'9']) == True = ( ((ord (s!!0))-48, (ord (s!!1))-48), True )
         | otherwise = ((666,666), False)

next :: Char -> Char
next a | a == 'X' = 'O'
	   | otherwise = 'X'

tie :: Field -> Move -> Bool
tie f m = if ((f!!0!!0 /= '.') && (f!!0!!1 /= '.') && (f!!0!!2 /= '.') && (f!!1!!0 /= '.') && (f!!1!!1 /= '.') && (f!!1!!2 /= '.') && (f!!2!!0 /= '.') && (f!!2!!1 /= '.') && (f!!2!!2 /= '.')) == True then True else False

playMore :: IO()
playMore = do
 putStrLn "Nu kak pirojok, igraem snova? 'da' 'ustal'"
 checkstr <- getLine
 if checkstr == "da" then do
  main
 else if checkstr == "ustal" then
  return ()
 else do
  putStrLn "durak chi kak?"
  playMore
	
aiMove :: Field -> Int -> Move
aiMove field m | snd (mmove field (x,y) 'O') = (x,y)
               | otherwise = aiMove field ((3 * m + 42)*2)
                            where
                              size = length field
                              x = m `mod` size
                              y = (m `div` 12) `mod` size -- 22 00 21

mmove :: Field -> Move -> Char -> (Field,Bool)
mmove f m pl | ((fst m) < 0) || ((snd m) < 0) || ((fst m) >= (length f)) || ((snd m) >= (length f)) = (f, False)
             | checkPos (fst m) (snd m) f == 'X' || checkPos (fst m) (snd m) f == 'O' = (f, False)
			 | otherwise = (xomov (fst m) (snd m) pl f, True) 

checkPos :: Int -> Int -> Field	-> Char
checkPos x y f = (f!!y)!!x		   

mov :: Int -> a -> [a] -> [a]
mov place pl f = take place f ++ pl : drop (place + 1) f

xomov :: Int -> Int -> a -> [[a]] -> [[a]]
xomov x y pl f = mov y (mov x pl (f!!y)) f

slStr :: [Char] -> [[Char]]
slStr = map (\f -> [f])

fieldLook :: Field -> [Char]
fieldLook field = intercalate rowSpace (map strSpace field)
                                                   where
                                                      strSpace = intercalate "       " . slStr
                                                      rowSpace = "\n" ++ "\n" ++ "\n"

winCond :: Field -> Char -> Bool
winCond f c | ((f!!0!!0 == c) && (f!!0!!1 == c) && (f!!0!!2 == c)) == True = True
            | ((f!!1!!0 == c) && (f!!1!!1 == c) && (f!!1!!2 == c)) == True = True
			| ((f!!2!!0 == c) && (f!!2!!1 == c) && (f!!2!!2 == c)) == True = True
			| ((f!!0!!0 == c) && (f!!1!!0 == c) && (f!!2!!0 == c)) == True = True
			| ((f!!0!!1 == c) && (f!!1!!1 == c) && (f!!2!!1 == c)) == True = True
			| ((f!!0!!2 == c) && (f!!1!!2 == c) && (f!!2!!2 == c)) == True = True
			| ((f!!0!!0 == c) && (f!!1!!1 == c) && (f!!2!!2 == c)) == True = True
			| ((f!!0!!2 == c) && (f!!1!!1 == c) && (f!!2!!0 == c)) == True = True
			| otherwise = False
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Data.Monoid
import System.Exit
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

data Mark = X | O deriving Eq

type Field = [[Maybe Mark]]

initialField :: Field
initialField = replicate 3 (replicate 3 Nothing)

win = animate (InWindow "ETO KRESTI" (800, 300) (100, 100)) white winningPicture

lose = animate (InWindow "ETO KRESTI" (800, 300) (100, 100)) white losingPicture

noOne = animate (InWindow "ETO KRESTI" (800, 300) (100, 100)) white tiePicture


winningPicture :: Float -> Picture
winningPicture _ = translate (-300) (5) $ text "YOU WON"

losingPicture :: Float -> Picture
losingPicture _ = translate (-300) (5) $ text "YOU LOST"

tiePicture :: Float -> Picture
tiePicture _ = translate (-300) (5) $ text "TIE"


main :: IO ()
main = do
  aiMove <- newEmptyMVar

  playIO
    (InWindow "ETO KRESTI" (600, 600) (100, 100))
    black
    60
    (initialField, X)
    fieldLook
    (handleInput aiMove)
    (playerTurn aiMove)

fieldLook :: (Field, Mark) -> IO Picture
fieldLook (field, _) = return (grid <> marks)
 where
  grid = color white (line [ (-100, -300), (-100,  300) ]) <> -- delaem setky, kombiniruya 'kartinki'
         color white (line [ ( 100, -300), ( 100,  300) ]) <>
         color white (line [ (-300,  100), ( 300,  100) ]) <>
         color white (line [ (-300, -100), ( 300, -100) ])

  marks = mconcat
    [ translate (fromIntegral $ (x - 1) * 200) -- koordinati
                (fromIntegral $ (y - 1) * 200) $
        case mark of
          X -> color red (rotate 45 (pictures [rectangleSolid 90 10, rectangleSolid 10 90]))
          O -> color blue (thickCircle 35 10)
    | x <- [0..2] -- ciklom probegaem kajdyu kletky
    , y <- [0..2]
    , Just mark <- [ (field !! x) !! y ] -- esli fail, to prodoljaem, esli vipolnitsa, to mojem postavit mark. Sozdaetsa spisok mark, kotorie concatiniryem [Picture] -> Picture
    ]

handleInput :: MVar Field -> Event -> (Field, Mark) -> IO (Field, Mark)
handleInput aiMove (EventKey (MouseButton LeftButton) Up _ (x, y)) (field, X) =
    let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 100) . (+ 50) -- convertiryem koordinati mishki v koordinati na setke
        (gridX, gridY) = (snap x, snap y)
    in case (field !! gridX) !! gridY of
      Just _ -> return (field, X) -- esli zanyato
   
      Nothing -> do --norm
        let newField = (ix gridX . ix gridY .~ (Just X)) field  -- novoe pole s 'X' gde najal polzovatel, menyaem hod.
        when (winCond newField (maybeContainer X) == True) (win)
        when (tie field (maybeContainer O) == True) (noOne)
        aiHandle aiMove newField
        return (newField, O)

handleInput _ _ (field, mark) = return (field, mark)

maybeContainer :: a -> Maybe a
maybeContainer x = Just x
maybeContainer _ = Nothing

aiHandle :: MVar Field -> Field -> IO () -- mvar dlya raboti s IO, mvar - tipo yashik v kotorom est/nety zna4eniya
aiHandle aiMove field = do 
  when (winCond field (maybeContainer O) == True) (lose)

  let turns = [ (ix x . ix y .~ Just O) field -- spisok vseh vozmojnih hodov iz tekyshego polya , ix dlya indeksirovaniya spiska, tipo obhod zadannogo indeksa (i - tiy element v strukture)
              | x <- [0..2] -- .~ eto tipo set(ystanovit zna4enie), kak =
              , y <- [0..2]
              , Nothing <- [ (field !! x) !! y ]
              ]
  
  case turns of
    [] -> do -- net hodov
      putMVar aiMove field -- putMVar - lojim rezyltat v yacheiky
      when (winCond field (maybeContainer O) == True) (lose)
      when (tie field (maybeContainer O) == True) (noOne)
      

    _ -> do -- hodim
      when (winCond field (maybeContainer O) == True) (lose)
      newField <- (turns !!) <$> randomRIO (0, length turns - 1) -- delaem randomniy hod
      when (winCond field (maybeContainer O) == True) (lose)
      putMVar aiMove newField -- putMVar - lojim rezyltat v yacheiky
      when (winCond field (maybeContainer O) == True) (lose)
      when (tie field (maybeContainer O) == True) (noOne)
      

playerTurn :: MVar Field -> Float -> (Field, Mark) -> IO (Field, Mark)
playerTurn aiMove _ (field, O) = tryTakeMVar aiMove >>= return . maybe (field, O) (\newField -> (newField, X)) -- proveryaem, sdelal li kompukter hod, ispolzuya tryTakeMVar. sdelal -> imeem Just Field, novoe pole
playerTurn _ _ state = return state -- ne sdelal hod -> Nothing => ni4e ne menyaem

tie :: Field -> Maybe Mark -> Bool
tie f m = if ((f!!0!!0 /= Nothing) && (f!!0!!1 /= Nothing) && (f!!0!!2 /= Nothing) && 
             (f!!1!!0 /= Nothing) && (f!!1!!1 /= Nothing) && (f!!1!!2 /= Nothing) && 
             (f!!2!!0 /= Nothing) && (f!!2!!1 /= Nothing) && (f!!2!!2 /= Nothing)) == True then True 
               else False

winCond :: Field -> Maybe Mark -> Bool
winCond f c | ((f!!0!!0 == c) && (f!!0!!1 == c) && (f!!0!!2 == c)) == True = True
            | ((f!!1!!0 == c) && (f!!1!!1 == c) && (f!!1!!2 == c)) == True = True
            | ((f!!2!!0 == c) && (f!!2!!1 == c) && (f!!2!!2 == c)) == True = True
            | ((f!!0!!0 == c) && (f!!1!!0 == c) && (f!!2!!0 == c)) == True = True
            | ((f!!0!!1 == c) && (f!!1!!1 == c) && (f!!2!!1 == c)) == True = True
            | ((f!!0!!2 == c) && (f!!1!!2 == c) && (f!!2!!2 == c)) == True = True
            | ((f!!0!!0 == c) && (f!!1!!1 == c) && (f!!2!!2 == c)) == True = True
            | ((f!!0!!2 == c) && (f!!1!!1 == c) && (f!!2!!0 == c)) == True = True
            | otherwise = False

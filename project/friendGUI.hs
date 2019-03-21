import Control.Applicative
import Graphics.Gloss.Data.Picture
import Control.Lens
import Control.Monad
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

data Mark = X | O deriving Eq

type Field = [[Maybe Mark]]

initialBoard :: Field
initialBoard = replicate 3 (replicate 3 Nothing)

winP1 = animate (InWindow "ETO KRESTI" (800, 300) (100, 100)) white winningPictureP1

winP2 = animate (InWindow "ETO KRESTI" (800, 300) (100, 100)) white winningPictureP2

winningPictureP1 :: Float -> Picture
winningPictureP1 _ = translate (-300) (5) $ text "X WON"

winningPictureP2 :: Float -> Picture
winningPictureP2 _ = translate (-300) (5) $ text "O WON"

main :: IO ()
main = do
  playIO
    (InWindow "ETO KRESTI" (600, 600) (100, 100))
    black
    60
    (initialBoard, X)
    fieldLook
    handleInput
    playerTurn

fieldLook :: (Field, Mark) -> IO Picture
fieldLook (field, _) = return (grid <> marks)
 where
  grid = 
    color white (line [ (-100, -300), (-100,  300) ]) <> -- delaem setky, kombiniruya 'kartinki'
    color white (line [ ( 100, -300), ( 100,  300) ]) <>
    color white (line [ (-300,  100), ( 300,  100) ]) <>
    color white (line [ (-300, -100), ( 300, -100) ])

  marks = mconcat
    [ translate (fromIntegral $ (x - 1) * 200) -- koordinati
                (fromIntegral $ (y - 1) * 200) $
        case mark of
          X -> color red (rotate 45 (pictures [rectangleSolid 90 10, rectangleSolid 10 90]))
          O -> color black (thickCircle 35 10)
    | x <- [0..2] -- ciklom probegaem kajdyu kletky
    , y <- [0..2]
    , Just mark <- [ (field !! x) !! y ] -- esli fail, to prodoljaem, esli vipolnitsa, to mojem postavit mark. Sozdaetsa spisok mark, kotorie concatiniryem [Picture] -> Picture
    ]

handleInput :: Event -> (Field, Mark) -> IO (Field, Mark)
handleInput
  (EventKey (MouseButton LeftButton) Up _ (x, y))
  (field, X) = 
    let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 100) . -- convertiryem koordinati mishki v koordinati na setke
             (+ 50)
        (gridX, gridY) = (snap x, snap y)
    
    in case (field !! gridX) !! gridY of --  zanyato
      Just _ -> return (field, X)

      Nothing -> do
        let newField = (ix gridX . ix gridY .~ (Just X)) field -- novoe pole s 'X' gde najal polzovatel, menyaem hod, ix dlya indeksirovaniya spiska, tipo obhod zadannogo indeksa (i - tiy element v strukture)
        when (winCond newField (maybeContainer X) == True) (winP1) -- .~ eto tipo set(ystanovit zna4enie), kak =

        return (newField, O)
handleInput
  (EventKey (MouseButton LeftButton) Up _ (x, y))
  (field, O) = 
    let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 100) .
             (+ 50)
        (gridX, gridY) = (snap x, snap y)
    
    in case (field !! gridX) !! gridY of
      Just _ -> return (field, O)

      Nothing -> do
        let newField = (ix gridX . ix gridY .~ (Just O)) field
        when (winCond newField (maybeContainer O) == True) (winP2)

        return (newField, X)
handleInput _ (field, mark) = return (field, mark)

playerTurn :: Float -> (Field, Mark) -> IO (Field, Mark)
playerTurn _ (field, O) = return (field, O)

maybeContainer :: a -> Maybe a
maybeContainer x = Just x
maybeContainer _ = Nothing

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

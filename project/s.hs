import Control.Applicative
import Graphics.Gloss.Data.Picture
import Control.Lens
import Control.Monad
import Data.Monoid
import Graphics.Gloss.Interface.IO.Game
import System.Random

data Mark = X | O deriving Eq

type Field = [[Maybe Mark]]

initialBoard :: Field
initialBoard = replicate 3 (replicate 3 Nothing)

main :: IO ()
main = do
  playIO
    (InWindow "ETO KRESTI" (600, 600) (100, 100))
    white
    60
    (initialBoard, X)
    drawBoard
    handleInput
    playerTurn

drawBoard :: (Field, Mark) -> IO Picture
drawBoard (field, _) = return (grid <> marks)
 where
  grid = 
    color black (line [ (-100, -300), (-100,  300) ]) <> -- delaem setky, kombiniruya 'kartinki'
    color black (line [ ( 100, -300), ( 100,  300) ]) <>
    color black (line [ (-300,  100), ( 300,  100) ]) <>
    color black (line [ (-300, -100), ( 300, -100) ])

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
        let newField = (ix gridX . ix gridY .~ (Just X)) field -- novoe pole s 'X' gde najal polzovatel, menyaem hod.

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

        return (newField, X)
handleInput _ (field, mark) = return (field, mark)

playerTurn :: Float -> (Field, Mark) -> IO (Field, Mark)
playerTurn _ (field, O) = return (field, O)

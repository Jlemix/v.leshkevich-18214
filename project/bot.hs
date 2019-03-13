import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Data.Monoid
import Graphics.Gloss.Interface.IO.Game
import System.Random

data Mark = X | O deriving Eq

type Field = [[Maybe Mark]]

initialField :: Field
initialField = replicate 3 (replicate 3 Nothing)

main :: IO ()
main = do
  aiMove <- newEmptyMVar

  playIO
    (InWindow "ETO KRESTI" (600, 600) (100, 100))
    white
    60
    (initialField, X)
    fieldLook
    (handleInput aiMove)
    (playerTurn aiMove)

fieldLook :: (Field, Mark) -> IO Picture
fieldLook (field, _) = return (grid <> marks)
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
        aiHandle aiMove newField
        return (newField, O)

handleInput _ _ (field, mark) = return (field, mark)

aiHandle :: MVar Field -> Field -> IO () -- mvar dlya raboti s IO, mvar - tipo yashik v kotorom est/nety zna4eniya
aiHandle aiMove field = void $ forkIO $ do -- forkIO - fonovoe deistvie
  randomRIO (100000, 1000000) >>= threadDelay -- zamedlyaem AI

  let turns = [ (ix x . ix y .~ Just O) field -- spisok vseh vozmojnih hodov iz tekyshego polya
              | x <- [0..2]
              , y <- [0..2]
              , Nothing <- [ (field !! x) !! y ]
              ]

  case turns of
    [] -> do -- net hodov
      putMVar aiMove field -- putMVar - lojim rezyltat v yacheiky

    _ -> do -- hodim
      newField <- (turns !!) <$> randomRIO (0, length turns - 1) -- delaem randomniy hod
      putMVar aiMove newField -- putMVar - lojim rezyltat v yacheiky

playerTurn :: MVar Field -> Float -> (Field, Mark) -> IO (Field, Mark)
playerTurn aiMove _ (field, O) = tryTakeMVar aiMove >>= return . maybe (field, O) (\newField -> (newField, X)) -- proveryaem, sdelal li kompukter hod, ispolzuya tryTakeMVar. sdelal -> imeem Just Field, novoe pole
playerTurn _ _ state = return state -- ne sdelal hod -> Nothing => ni4e ne menyaem
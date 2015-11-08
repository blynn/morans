import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Data.Ord
import Haste
import Haste.Graphics.Canvas
import Haste.Foreign

foreign import ccall "think" jsThink :: JSString -> IO JSString
foreign import ccall "random_sample" jsSample :: IO JSString

sz = 4; lim = sz * 28

darken (xs, x, y) = let
  r = 5
  x0 = max 0 $ x - r
  y0 = max 0 $ y - r
  x1 = min (lim - 1) $ x + r
  y1 = min (lim - 1) $ y + r
  circle = [(div u sz + 28*(div v sz), 16) |
     u <- [x0..x1], v <- [y0..y1], let d2 = (u-x)^2 + (v-y)^2, d2 < r^2]
  in (elems $ accum (+) (listArray (0, 28^2 - 1) xs) circle, x, y)

main = withElems ["canvas", "message", "clearB", "sampleB", "goB"] $
    \[cElem, message, clearButton, sampleButton, goButton] -> do
  Just canvas <- getCanvas cElem
  xVar <- newMVar (replicate 784 0, 0, 0)
  penVar <- newMVar False

  let
    box n (y, x) = let m = 255 - n in color (RGB m m m) $ fill $ rect (fromIntegral (x*sz), fromIntegral (y*sz)) (fromIntegral (x*sz + sz), fromIntegral (y*sz + sz))

    update = do
      (xs, _, _) <- readMVar xVar
      render canvas $ zipWithM box xs $ (,) <$> [0..27] <*> [0..27]
      return ()

    penCheck = do
      pen <- readMVar penVar
      when pen $ do
        (xs, x, y) <- takeMVar xVar
        putMVar xVar $ darken (xs, x, y)
        update
      setTimeout 50 penCheck

    guess = do
      (xs, _, _) <- readMVar xVar
      a <- jsThink $ toJSString $ show $ ((/ 256) . fromIntegral) <$> xs
      let
        ys = read $ show a :: [Float]
        best = fst . maximumBy (comparing snd) . zip [0..] $ ys
      void $ setProp message "innerHTML" $ "best guess: " ++ show best

  _ <- cElem `onEvent` OnMouseDown $ \_ (x, y) -> do
    orig@(xs, _, _) <- takeMVar xVar
    if x < lim && y < lim then do
      putMVar xVar $ darken (xs, x, y)
      swapMVar penVar True
      update
    else putMVar xVar orig

  _ <- cElem `onEvent` OnMouseUp $ \_ _ -> void $ swapMVar penVar False

  _ <- cElem `onEvent` OnMouseOut $ void $ swapMVar penVar False

  _ <- cElem `onEvent` OnMouseMove $ \(x, y) -> do
    pen <- readMVar penVar
    when (pen && x < lim && y < lim) $ do
      (xs, _, _) <- takeMVar xVar
      putMVar xVar $ darken (xs, x, y)
      update

  _ <- clearButton `onEvent` OnClick $ \_ _ -> do
    swapMVar xVar (replicate 784 0, 0, 0)
    update
    void $ setProp message "innerHTML" ""

  _ <- sampleButton `onEvent` OnClick $ \_ _ -> do
    (_, x, y) <- takeMVar xVar
    a <- jsSample
    putMVar xVar (read $ show a, x, y)
    update
    guess

  _ <- goButton `onEvent` OnClick $ \_ _ -> guess

  penCheck

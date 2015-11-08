{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Array
import Data.IORef
import Data.List
import Data.Ord
import Haste
import Haste.DOM
import Haste.Events
import Haste.Foreign
import Haste.Graphics.Canvas

jsThink :: String -> IO String
jsThink = ffi "think"

jsSample :: IO String
jsSample = ffi "random_sample"

sz = 4; lim = sz * 28

darken (xs, x, y) = let
  r = 4
  x0 = max 0 $ x - r
  y0 = max 0 $ y - r
  x1 = min (lim - 1) $ x + r
  y1 = min (lim - 1) $ y + r
  circle = [(div u sz + 28*div v sz, 16) |
     u <- [x0..x1], v <- [y0..y1], let d2 = (u-x)^2 + (v-y)^2, d2 < r^2]
  in (elems $ accum (+) (listArray (0, 28^2 - 1) xs) circle, x, y)

main = withElems ["canvas", "message", "clearB", "sampleB"] $
    \[cElem, message, clearButton, sampleButton] -> do
  Just canvas <- getCanvas cElem
  xVar <- newIORef (replicate 784 0, 0, 0)
  penVar <- newIORef False

  let
    box n (y, x) = let m = 255 - n in color (RGB m m m) $ fill $ rect (fromIntegral (x*sz), fromIntegral (y*sz)) (fromIntegral (x*sz + sz), fromIntegral (y*sz + sz))

    update = do
      (xs, _, _) <- readIORef xVar
      render canvas $ zipWithM box xs $ (,) <$> [0..27] <*> [0..27]
      return ()

    penCheck = do
      pen <- readIORef penVar
      when pen $ do
        (xs, x, y) <- readIORef xVar
        writeIORef xVar $ darken (xs, x, y)
        update
      void $ setTimer (Once 50) penCheck

    guess = do
      (xs, _, _) <- readIORef xVar
      a <- jsThink $ show $ ((/ 256) . fromIntegral) <$> xs
      let
        scores = zip [0..] (read a :: [Float])
        best = fst . maximumBy (comparing snd) $ scores
      void $ setProp message "innerHTML" $ "best guess: " ++ show best ++
        "\n<pre>\n" ++
        unlines (map (\(d, y) -> show d ++ ": " ++ replicate (round $ 16 * y) '━') scores) ++
        "</pre>\n"

  set cElem [style "cursor" =: "crosshair"]

  cElem `onEvent` MouseDown $ \(MouseData (x, y) _ _) -> do
    preventDefault
    orig@(xs, _, _) <- readIORef xVar
    if x < lim && y < lim then do
      writeIORef xVar $ darken (xs, x, y)
      writeIORef penVar True
      update
    else writeIORef xVar orig

  _ <- cElem `onEvent` MouseUp $ \_ -> writeIORef penVar False >> guess

  _ <- cElem `onEvent` MouseOut $ \_ -> writeIORef penVar False >> guess

  _ <- cElem `onEvent` MouseMove $ \(MouseData (x, y) _ _) -> do
    pen <- readIORef penVar
    when (pen && x < lim && y < lim) $ do
      (xs, _, _) <- readIORef xVar
      writeIORef xVar $ darken (xs, x, y)
      update

  _ <- clearButton `onEvent` Click $ \_ -> do
    writeIORef xVar (replicate 784 0, 0, 0)
    update
    void $ setProp message "innerHTML" ""

  _ <- sampleButton `onEvent` Click $ \_ -> do
    (_, x, y) <- readIORef xVar
    a <- jsSample
    writeIORef xVar (read a, x, y)
    update
    guess

  penCheck

-- Too slow. Turns out there's too much overhead when compiling this to
-- JavaScript with Haste.
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.Ord
import Haste
import Haste.Ajax
import Haste.Graphics.Canvas

sz = 4; lim = sz * 28

zLayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs

feed = foldl' (((max 0 <$>) . ) . zLayer)

think ns brain = let xs = ((/ 256) . fromIntegral) <$> ns in
  fst . maximumBy (comparing snd) . zip [0..] $ feed xs brain

main = withElems ["canvas", "message", "clearButton", "goButton"] $ \elems -> do
  textRequest GET "85.txt" [] $ go elems

darken (xs, x, y) = let (as, b:bs) = splitAt (x + 28*y) xs in
  (as ++ (min 255 (b + 255):bs), x, y)

go [cElem, message, clearButton, goButton] (Just braintxt) = do
  void $ setProp message "innerHTML" $ "version qux"
  Just canvas <- getCanvas cElem
  xVar <- newMVar (replicate 784 0, 0, 0)
  penVar <- newMVar False

  let
    brain = read braintxt :: [([Float], [[Float]])]
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
      setTimeout 50 $ penCheck

    guess = do
      (xs, _, _) <- readMVar xVar
      setTimeout 1 $ void $ setProp message "innerHTML" $ show $ think xs brain

  _ <- cElem `onEvent` OnMouseDown $ \_ (x, y) -> do
    orig@(xs, _, _) <- takeMVar xVar
    if x < lim && y < lim then do
      putMVar xVar $ darken (xs, x `div` sz, y `div` sz)
      swapMVar penVar True

      update
    else putMVar xVar orig

  _ <- cElem `onEvent` OnMouseUp $ \_ _ -> void $ swapMVar penVar False

  _ <- cElem `onEvent` OnMouseOut $ void $ swapMVar penVar False

  _ <- cElem `onEvent` OnMouseMove $ \(x, y) -> do
    pen <- readMVar penVar
    when (pen && x < lim && y < lim) $ do
      (xs, _, _) <- takeMVar xVar
      putMVar xVar $ darken (xs, x `div` sz, y `div` sz)
      update

  _ <- clearButton `onEvent` OnClick $ \_ _ ->
    swapMVar xVar (replicate 784 0, 0, 0) >> update

  _ <- goButton `onEvent` OnClick $ \_ _ -> guess

  penCheck

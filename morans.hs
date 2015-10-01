-- Requires files from http://yann.lecun.com/exdb/mnist/
import Codec.Compression.GZip (decompress)
import Data.Int
import qualified Data.ByteString.Lazy as BS
import System.Random
import Data.Ord

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List

--import Data.Random.Normal

relu = max 0

relu' x | x < 0 = 0
        | True  = 1

gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

newBrain :: [Int] -> IO [([Float], [[Float]])]
{-
newBrain szs@(_:ts) = do
  --biases  <- forM ts $ flip replicateM normalIO
  let biases = flip replicate 1 <$> ts
  --weights <- zipWithM (\m n -> replicateM n $ replicateM m normalIO) szs ts
  --weights <- zipWithM (\m n -> replicateM n $ replicateM m (normalIO' (0, 0.01))) szs ts
  weights <- zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts
  return $ zip biases weights
  -}
newBrain szs@(_:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts

zLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
zLayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs

feed :: [Float] -> [([Float], [[Float]])] -> [Float]
feed = foldl' (((relu <$>) . ) . zLayer)

revaz :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
revaz xs layers = foldl' (\(avs@(av:_), zs) (bs, wms) -> let
  zs' = zLayer av (bs, wms)
  in (((relu <$> zs'):avs), (zs':zs))) ([xs], []) layers

--dCost a y = a - y
dCost a y | y == 1 && a >= y = 0
          | True             = a - y

deltas :: [Float] -> [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
deltas xv yv layers = let
  (avs@(av:_), zv:zvs) = revaz xv layers
  delta0 = zipWith (*) (zipWith dCost av yv) (relu' <$> zv)
  in (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
      zipWith (*) [(sum $ zipWith (*) row dv) | row <- wm] (relu' <$> zv)

eta = 0.002

descend av dv = zipWith (-) av ((eta *) <$> dv)

learn xv yv layers = let (avs, dvs) = deltas xv yv layers
  in zip (zipWith descend (fst <$> layers) dvs) $
    zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv) 
      (snd <$> layers) avs dvs

getImage s n = fromIntegral . BS.index s . (n*28^2 + 16 +) <$> [0..28^2 - 1]
getX     s n = (/ 256) <$> getImage s n
getLabel s n = fromIntegral $ BS.index s (n + 8)
getY     s n = fromIntegral . fromEnum . (getLabel s n ==) <$> [0..9]

render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

main = do
  [s, l, testI, testL] <- mapM ((decompress  <$>) . BS.readFile)
    [ "train-images-idx3-ubyte.gz"
    , "train-labels-idx1-ubyte.gz"
    ,  "t10k-images-idx3-ubyte.gz"
    ,  "t10k-labels-idx1-ubyte.gz"
    ]
  b <- newBrain [784, 30, 10]
  n <- (`mod` 60000) <$> randomIO
  putStr . unlines $
    --[(render . BS.index s . (n*28^2 + 16 + r*28 +)) <$> [0..27] | r <- [0..27]]
    --unfoldr (\ns -> if null ns then Nothing else Just $ splitAt 28 ns) (render <$> getImage s n)
    take 28 $ take 28 <$> (iterate (drop 28) $ render <$> getImage s n)

  let
    example = getX s n
    go b ns = foldl' (\b n -> learn (getX s n) (getY l n) b) b ns
    bs = scanl go b [
     [   0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..9999]]
    smart = last bs
    cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
    bestOf = fst . maximumBy (comparing snd) . zip [0..]

  forM bs $ putStrLn . unlines . zipWith cute [0..9] . feed example

  putStrLn $ "best guess: " ++ (show $ bestOf $ feed example smart)

  let guesses = bestOf . (\n -> feed (getX testI n) smart) <$> [0..9999]
  let answers = getLabel testL <$> [0..9999]
  putStrLn $ (show $ sum $ fromEnum <$> zipWith (==) guesses answers) ++
     " / 10000"

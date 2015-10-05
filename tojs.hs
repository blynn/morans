import Data.Functor

layerToJS (biases, weights) id = let
  n = length biases in unlines
  [ "  var bias" ++ id ++ " = " ++ show biases ++ ";"
  , "  var weights" ++ id ++ " = " ++ show weights ++ ";"
  , "  var sum" ++ id ++ " = " ++ show (replicate n 0) ++ ";"
  , "  for (i = 0; i < " ++ show n ++ "; i++) {"
  , "    for (j = 0; j < xs.length; j++) {"
  , "      sum" ++ id ++ "[i] += xs[j] * weights" ++ id ++ "[i][j];"
  , "    }"
  , "    sum" ++ id ++ "[i] += bias" ++ id ++ "[i];"
  , "    if (sum" ++ id ++ "[i] < 0) sum" ++ id ++"[i] = 0;"
  , "  }"
  , "  xs = sum" ++ id ++ ";"
  ]

toJS :: [([Float], [[Float]])] -> String
toJS brain = unlines $
  [ "function think(xstxt) {"
  , "  var xs = JSON.parse(xstxt);"
  ] ++ zipWith layerToJS brain (show <$> [0..]) ++
  [ "  return JSON.stringify(xs);"
  , "}"
  ]

main = interact $ toJS . read

module Main

import Build
import Build.LazyFile
import Build.HTML
import Build.Macros
import Build.Util

simple : Build StorePath
simple = do
  x1 <- cmd [] ["echo", "hello"] (Just $ ./ "hello1.txt")
  x2 <- cmd [] ["cat", x1 // "hello1.txt", x1 // "hello1.txt"] (Just out)
  pure $ x2

simpleLazy : Build StorePath
simpleLazy =
  writeLazy $ h1 { attrs = [("id", "title")] }
    [ (mkEl "i" [ !simple ])
    , ulist [ "a", "b" ]
    ]

record Assets where
  constructor MkAssets
  someFile : StorePath

tree : (assets : Assets) => Build StorePath
tree = dir
  [ ("simple.txt", !simple)
  , ("simple-lazy.txt", !simpleLazy)
  , ("preload-assets", assets.someFile)
  ]

main : IO ()
main = do
  let store = newStore
  Right someFile <- preload store "support.c"
    | Left x => putStrLn $ "failed: " ++ show x
  let assets = MkAssets someFile
  putPlan tree
  build tree




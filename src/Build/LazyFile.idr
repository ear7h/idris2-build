module Build.LazyFile

import Data.List.Quantifiers

import Build

namespace Data.List.Quantifiers.All
  public export
  allStatic : (prf : prop a) -> (as : List a) -> All prop (map (const a) as)
  allStatic prf [] = []
  allStatic prf (x::xs) = prf::(allStatic prf xs)

  public export
  heterogenize : (as : List a) -> HList (map (const a) as)
  heterogenize [] = []
  heterogenize (x::xs) = x::(heterogenize xs)

data LazyFile : Type

public export
data ToFragment : Type -> Type where
  FStr       : ToFragment String
  FStorePath : ToFragment StorePath
  FLazyFile  : ToFragment LazyFile

public export
data LazyFile : Type where
  MkLazyFile : List (Either String StorePath) -> LazyFile

public export
Nil : LazyFile
Nil = MkLazyFile []

public export
(::) : (f : ToFragment a) => a -> LazyFile -> LazyFile
(::) x (MkLazyFile xs) =
  case (f, x) of
       (FLazyFile , MkLazyFile ys) => MkLazyFile (     ys ++ xs)
       (FStr      , s            ) => MkLazyFile ((Left  s)::xs)
       (FStorePath, p            ) => MkLazyFile ((Right p)::xs)

export
concat : All ToFragment as => HList as -> LazyFile
concat xs = go xs
  where
  go : (prf : All ToFragment bs) => HList bs -> LazyFile
  go []      = []
  go (x::xs) = let (p::ps) = prf in x::(go xs)

export
writeLazy : LazyFile -> Build StorePath
writeLazy (MkLazyFile xs) = do
  paths <- map reverse $ foldlM go ("", []) xs >>= finish
  let prfs = allStatic AStorePath paths
  cmdo [] ("cat"::(heterogenize paths))

  where
  go :
    (String, List StorePath) -> Either String StorePath  ->
    Build (String, List StorePath)
  go (s , xs) (Left  x) = pure (s ++ x, xs)
  go ("", xs) (Right x) = pure ("", x::xs)
  go (s , xs) (Right x) = do
    s' <- write s
    pure ("", x::s'::xs)

  finish : (String, List StorePath) -> Build (List StorePath)
  finish ("", xs) = pure xs
  finish (s, xs) = do
    s' <- write s
    pure (s'::xs)


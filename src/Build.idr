module Build

import public Data.List.Quantifiers

import Control.Monad.Either
import Data.List1
import Data.Maybe
import Data.Buffer
import Data.SortedSet
import Data.String
import Debug.Trace
import System.Directory
import System.File
import System.File.Buffer
import System.FFI
import System

import Build.Util

dbg : Show a => a -> a
dbg x = trace (show x) x

Hash : Type
Hash = String -- Vect (digest_nbyte { algo = Sha256 }) Bits8

public export
data Out = MkOut

public export
out : Out
out = MkOut

export
data StorePath : Type where
  MkStorePath : String -> StorePath

data IsArg : Type -> Type

public export
data Concat : Type where
  MkConcat : IsArg a -> IsArg b -> a -> b -> Concat

public export
(++) : (fa : IsArg a) => (fb : IsArg b) => a -> b -> Concat
(++) = MkConcat fa fb

public export
data IsArg : Type -> Type where
  AStr : IsArg String
  AOut : IsArg Out
  AStorePath : IsArg StorePath
  AConcat : IsArg Concat

export infixr 0 //

public export
(//) : (fa : IsArg a) => (fb : IsArg b) => a -> b -> Concat
(//) x y = x ++ "/" ++ y

export prefix 0 ./

public export
(./) : IsArg a => a -> Concat
(./) x = MkOut // x

public export
record Store where
  constructor MkStore
  built : SortedSet Hash
  path : String

export
Show Store where
  show s = "{ built : \{ show s.built }, \{ show s.path }"

export
contains : Hash -> Store -> Bool
contains h = contains h . (.built)

export
insert : Hash -> Store -> Store
insert h = { built $= insert h }

export
newStore : Store
newStore = MkStore neutral "./store"

public export
data BuildPrim : Type where
  BCmd       : List (String, String) -> List1 String -> Maybe String -> BuildPrim
  BWriteFile : (dst : String) -> (dat : String) -> BuildPrim
  BWriteDir  : (dst : String) -> List (String, String) -> BuildPrim

export
Show BuildPrim where
  show (BCmd envs args mbout) = show ("cmd", envs, args, mbout)
  show (BWriteFile dst dat) = show ("file", dst, dat)
  show (BWriteDir dst ents) = show ("dir", dst, ents)

public export
data Error
  = EExec Int
  | EFile FileError

export
Show Error where
  show (EExec n) = "EExec \{ show n }"
  show (EFile e) = "EExec \{ show e }"

public export
IOE : Type -> Type
IOE v = IO (Either Error v)

export
execBuildPrim : BuildPrim -> IOE ()
execBuildPrim (BCmd env args mbout) = do
  Right _ <- mkdir mbout
    | x => pure x
  e <- getEnvironment
  err <- exec (env ++ e) args mbout
  if err == 0
     then pure (pure neutral)
     else pure $ Left $ EExec err
  where
  mkdir : Maybe String -> IOE ()
  mkdir Nothing = pure (pure neutral)
  mkdir (Just x) = do
    let (base, dir) = span (/= '/') $ reverse x
    False <- pure (null dir)
      | _ => pure (pure neutral)
    ret <- mapFst EFile <$> (createDir $ reverse dir)
    putStrLn $ "mkdir: " ++ (reverse dir)
    pure $ case ret of
         Right () => pure neutral
         Left (EFile FileExists) => pure neutral
         x => x

execBuildPrim (BWriteFile dst dat) = do
  mapFst EFile <$> ReadWrite.writeFile dst dat

execBuildPrim (BWriteDir dst xs) = do
  Right _ <- mapFst EFile <$> createDir dst
    | x => pure x
  go dst xs
  where
  go : String -> List (String, String) -> IOE ()
  go dst [] = pure $ Right ()
  go dst (x::xs) = do
    -- Right _ <- mapFst EExec <$>
    err <- the (IO Int) $ exec [] ("ln":::[snd x, dst ++ "/" ++ (fst x)]) Nothing
    if err == 0
       then go dst xs
       else pure $ Left $ EExec err

public export
0 MonoidBuilder : Type
MonoidBuilder = forall o . Monoid o => (BuildPrim -> o) -> o

public export
data Build : Type -> Type where
  -- MkBuild : MonoidBuilderBuilder a -> Build a
  MkBuild : (Store -> (Store, MonoidBuilder, a)) -> Build a

export
buildPrim : Hash -> (String -> BuildPrim) -> Build StorePath
buildPrim hash mkPrim = MkBuild $ \store =>
  let pstore = store.path ++ "/" ++ hash
  in let pstore' = MkStorePath pstore
  in
    if contains hash store
    then (store             , \build => neutral              , pstore')
    else (insert hash store , \build => build (mkPrim pstore), pstore')

export
runBuild : Monoid o => Store -> Build a -> (BuildPrim -> o) -> (Store, a, o)
runBuild store (MkBuild plan) mkm =
  let (newStore, buildm, x) = plan store
  in (newStore, x, buildm mkm)

public export
normalize :
  ( prf : All IsArg xs ) =>
  All f xs -> (forall a . IsArg a -> f a -> b) -> All (const b) xs
normalize args go = All.zipPropertyWith go prf args

export
argNul : IsArg a -> a -> String
argNul AStr x = joinBy "\0\0" $ forget $ split ('\0' ==) x
argNul AOut _ = "\0"
argNul AStorePath (MkStorePath x) = argNul AStr x
argNul AConcat (MkConcat fx fy x y) =
  (argNul fx x) ++ (argNul fy y)


export
argOut : String -> IsArg a -> a -> String
argOut outp AStr x = x
argOut outp AOut _ = outp
argOut outp AStorePath (MkStorePath x) = argOut outp AStr x
argOut outp AConcat (MkConcat fx fy x y) =
  (argOut outp fx x) ++ (argOut outp fy y)

public export
forget1 : All (const ty) (a::as) -> List1 ty
forget1 (x::xs) = x:::(forget xs)

export
cmd :
  All IsArg envv =>
  -- { 0 argts : List1 Type } ->
  All IsArg (a::as) =>
  ( ad : IsArg dst ) =>
  All (Pair String) envv -> HList (a::as) -> Maybe dst ->
  Build StorePath
cmd envs args dst = do
  let hash = sha256list $
      [ (++) "cmd:" $ fromMaybe "" $ map (argNul ad) dst
      , (++) "envs:" $ sha256list $ All.forget $ normalize envs envNul
      , (++) "args:" $ sha256list $ All.forget $ normalize args argNul
      ]
  let mkPrim = \outp => BCmd
        (All.forget $ normalize envs $ envOut outp)
        (forget1 $ normalize args $ argOut outp)
        (map (argOut outp ad) dst)
  buildPrim hash mkPrim
  where

  envNul : IsArg b -> (String, b) -> String
  envNul ia (k, v) = k ++ "=" ++ argNul ia v

  envOut : String -> IsArg b -> (String, b) -> (String, String)
  envOut outp ia (k, v) = (k, argOut outp ia v)

export
isIdent : Char -> Bool
isIdent c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') ||
  (c == '-') || (c == '_') || (c == '.')

export
cmdo :
  All IsArg ys =>
  All IsArg (String :: xs) =>
  All (Pair String) ys -> HList (String::xs) -> Build StorePath
cmdo envs args =
  let name = joinBy "_" $ forget $ split isIdent $ All.head args
  in cmd envs args (Just out)

export
dir : All IsArg xs => All (String,) xs -> Build StorePath
dir xs = do
  let hash = sha256list $ "dir" :: (join $ All.forget $ normalize xs elNul)
  -- TODO: this allows creating cycles...
  let prim = \pout => BWriteDir pout $ All.forget $ normalize xs (elOut pout)
  buildPrim hash prim
  where

  elNul : IsArg a -> (String, a) -> List String
  elNul a (x, y) = [x, argNul a y]

  elOut : String -> IsArg a -> (String, a) -> (String, String)
  elOut outp a (x, y) = (x, argOut outp a y)

export
write : String -> Build StorePath
write content =
  let hash = sha256 content
  in buildPrim hash (\path => BWriteFile path content)

export
preload : Store -> String -> IOE StorePath
preload store file = do
  Right buf <- createBufferFromFile file
    | Left x => pure $ Left $ EFile x
  e <- getEnvironment
  _ <- exec e ("pwd":::[]) (Nothing { ty = String })
  hash <- sha256Buf buf
  let p = store.path ++ "/" ++ hash
  0 <- exec e ("cp":::["--update=none", file, p]) (Nothing { ty = String })
    | x => pure $ Left $ EExec x
  pure $ pure $ MkStorePath p

export
Functor Build where
  map g (MkBuild mbb) = MkBuild $ \store =>
    let (store, mb, x) = mbb store
    in (store, mb, g x)

export
Applicative Build where
  -- pure x = MkBuild $ \store, mkout => (store, x, neutral)
  pure x = MkBuild $ \store => (store, \buildMonoid => neutral, x)

  (<*>) (MkBuild mkfx) (MkBuild mkx) =
    MkBuild $ \store =>
      let    (store, mb1, fx) = mkfx store
      in let (store, mb2, x ) = mkx  store
      in
        ( store
        , mb1 <+> mb2
        , fx x
        )

export
Monad Build where
  (>>=) (MkBuild mkx) (fmy) =
    MkBuild $ \store =>
      let (store, mb1, x) = mkx store
      in let (MkBuild mky) = fmy x
      in let (store, mb2, y) = mky store
      in (store, mb1 <+> mb2, y)


export
plan : Build a -> List BuildPrim
plan b = do
  let (newStore, _, plan) =
    runBuild newStore b List.singleton
  plan

export
putPlan : Build a -> IO ()
putPlan b = do
  let p = plan b
  traverse_ (putStrLn . show) p

export
build : Build StorePath -> IO ()
build b = do
  Right files <- listDir "./store"
    | Left err => putStrLn $ "FAIL: " ++ show err
  let (_, MkStorePath pout, plan) =
    runBuild (MkStore (fromList files) "./store") b (List.singleton . execBuildPrim)
  res <- runEitherT $ sequence_ $ map (MkEitherT { m = IO }) plan
  putStrLn $ show res
  Right _ <- pure res
    | Left err => putStrLn $ "FAIL: " ++ (show err)
  putStrLn $ "SUCCESS: " ++ pout
  0 <- exec [] ("ln":::["-fsn", pout, "out"]) (Nothing { ty = String })
    | x => putStrLn $ "error linking: " ++ show x
  pure ()



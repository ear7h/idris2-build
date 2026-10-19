module Build.Util

import Data.List1
import Data.Maybe
import Data.String
import Data.Buffer
import System

%foreign "C:prim__sha256,libwebsite_support"
prim__sha256: String -> Int -> String

%foreign "C:prim__sha256,libwebsite_support"
prim__sha256Buf: Buffer -> Int -> PrimIO String

export
sha256 : String -> String
sha256 x = prim__sha256 x (cast $ length x)

export
sha256Buf : HasIO io => Buffer -> io String
sha256Buf x = primIO $ prim__sha256Buf x !(rawSize x)

export
sha256list : List String -> String
sha256list = sha256 . concat . map sha256

%foreign "C:prim__exec,libwebsite_support"
prim__exec: String -> Int -> String -> Int -> String -> Int -> PrimIO Int

export
exec : List (String, String) -> List1 String -> Maybe String -> IO Int
exec envs args newout =
  primIO $ prim__exec
    envs' (cast $ length envs)
    args' (cast $ length args)
    (fromMaybe "" newout) (if isJust newout then 1 else 0)
  where
  fix0 : String -> String
  fix0 = head . split (== '\0')

  args' : String
  args' = joinBy "\0" $ map fix0 (forget args)

  envs' : String
  envs' = joinBy "\0" $ map (\(x, y) => fix0 x ++ "=" ++ fix0 y) envs

test : IO ()
test = do
  e <- getEnvironment
  x <- show <$> exec e ("ls":::[]) Nothing
  putStrLn $ "got: " ++ x
  y <- show <$> exec e ("echo":::["hello"]) (Just "hello.txt")
  putStrLn $ "got: " ++ y

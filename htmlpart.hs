module Main where
import Codec.MIME.Parse
import Codec.MIME.Type
import Data.List
import Data.Maybe
import System.IO
import Text.Printf


findPart :: MIMEValue -> String
findPart m@(MIMEValue t@(Type (Text "html") _) d (Single c) h inc) = c
-- attachments
findPart (MIMEValue t@(Type _ mparams) d (Single c) h inc) = ""
findPart (MIMEValue t@(Type (Multipart _) _) d (Multi cs) h inc) = concat $ map findPart cs
findPart m = error $ "undefined: " ++ show m

main = do
  getContents >>= putStrLn . findPart . parseMIMEMessage 

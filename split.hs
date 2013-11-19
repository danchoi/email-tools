module Main where
import Codec.MIME.Parse
import Codec.MIME.Type
import Data.List
import Data.Maybe
import System.IO
import Text.Printf


filenameExt (Text st) = st
filenameExt _ = "something.out"

saveFile :: MIMEType -> [(String,String)] -> String -> IO ()
saveFile t hs c = do
    putStrLn $ "Saving file: "++filename
    putStrLn $ show hs
    withBinaryFile filename WriteMode (\h -> hPutStr h c) 
  where filename = "content." ++ (filenameExt t)


split :: MIMEValue -> IO ()
split m@(MIMEValue t@(Type (Text _) _) d (Single c) h inc) = saveFile (mimeType.mime_val_type $ m) (mime_val_headers m) c

-- save attachment
split m@(MIMEValue t@(Type _ mparams) d (Single c) h inc) = do 
    putStrLn $ "Saving binary: " ++ filename
    putStrLn $ show h
    putStrLn $ show d
    -- appendFile "attachments" (attachmentInfo h d)
    putStrLn (attachmentInfo h d)
    withBinaryFile filename WriteMode (\h -> hPutStr h c)
  where filename = fromMaybe "unknown_filename" $ lookup "name" mparams
        attachmentInfo hs d = printf "%-20.20s" (fromMaybe "-" (lookup "content-type" hs))
          

split (MIMEValue t@(Type (Multipart _) _) d (Multi cs) h inc) = mapM_ split cs
split m = putStrLn ("undefined: " ++ show m)

saveHeaders :: [(String, String)] -> IO ()
saveHeaders xs = 
    withBinaryFile "headers" WriteMode (\h -> hPutStrLn h $ plaintext xs)
  where plaintext xs = unlines $ map fmtline xs
        fmtline (k,v) = printf "%-20.20s %s" k v

main = do
  putStrLn $ "localeEncoding is "++show localeEncoding
  raw <- getContents
  let m = parseMIMEMessage raw
  split m
  saveHeaders (mime_val_headers m) 
  {-
  let (MIMEValue mtype disp content headers inc_type) = parseMIMEMessage raw
  mapM_ print headers
  print mtype
  putStrLn "Bodies"
  prettyContent 0 $ shortContent content
  -}

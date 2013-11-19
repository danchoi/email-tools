{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.ByteString.Lazy.Char8 as B
import Network.Mail.Mime

test = emptyMail $ Address (Just "Daniel Choi") "dhchoi@gmail.com"


main = do 
    m <-  simpleMail (Address (Just "Daniel Choi") "dhchoi+test@gmail.com")
                 (Address (Just "MackeyRMS") "jet@mackeyllc.com")
                 "Subject "
                 "test body"
                 "<p>HTML body</p>"
                 [("Dewey Book", "/Users/choi/Downloads/Human_Nature_and_Conduct.pdf")]
    m' <- renderMail' m
    B.putStrLn m'


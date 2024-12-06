{-# LANGUAGE OverloadedStrings #-}
module Main where
import Types 
import Parser 
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Web.Scotty 
import Data.Bits 
import System.IO 


------------------------------------

main :: IO ()
main = do 
    putStr "Enter a binary number: "
    hFlush stdout
    binNum <- getLine 
    case parse parseBin "" binNum of 
      Left e  -> putStrLn $ errorBundlePretty e 
      Right r -> do 
                  (putStr "You entered: ") >> (hFlush stdout)
                  putStrLn $ show $ r
                  let i = binToInt r 
                      b = intToBin i 
                  (putStr "binary number you entered as an int: ") >> (hFlush stdout)
                  putStrLn $ show $ i
                  (putStr "turning int back to binary to confirm it works: ") >> (hFlush stdout)
                  putStrLn $ show $ b

    {-
    scotty 8000 $ do 
      get "/" $ do 
       file "frontend/index.html"
    -}

    
    
     

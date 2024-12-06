{-# LANGUAGE OverloadedStrings #-}
module Main where
import Types 
import Parser 
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Web.Scotty 
import Data.Bits 
import System.IO 
import Data.ByteString as BS


------------------------------------
{- 

instance Bits (Bin Bool) where 
    bin1 .&. bin2      =  intToBin ((binToInt bin1) .&. (binToInt bin2))
    bin1 .|. bin2      =  intToBin ((binToInt bin1) .|. (binToInt bin2))
    bin1 `xor` bin2    =  intToBin ((binToInt bin1) `xor` (binToInt bin2))
    complement bin     =  intToBin $ complement $ (binToInt bin)
    shiftR bin  i      =  intToBin $ (shiftR (binToInt bin) i) 
    rotateR bin i      =  intToBin $ (rotateR (binToInt bin) i) 
    bitSize bin        =  bitSize $ binToInt bin
    bitSizeMaybe  b    =  bitSizeMaybe $ binToInt b 
    isSigned      b    =  isSigned (binToInt b)
    testBit bin i      =  (testBit (binToInt bin) i)
    bit i              =  intToBin $ (bit i :: Int)
    popCount (Bin l)   = length (filter (== True) l) 
-}
main :: IO ()
main = do 
    scotty 8000 $ do 
      get "/" $ do  
       file "frontend/index.html"
      post "/submit" $ do 
        bodyBS <- body 
        liftIO (BS.writeFile "./debugBS.txt" (BS.toStrict bodyBS))
        liftIO $ hFlush stdout 
        req    <- jsonData :: ActionM ClientRequest
        let binnum = binNum req 
            resp   = case parse parseBin "" binnum of 
                      Left  e -> ServerResponse {binaryNum = "", decimal = ""}
                      Right r -> ServerResponse {binaryNum = show r, decimal = show $ binToInt r }
        json resp 


    
    
     

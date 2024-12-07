{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where 

import Data.Bits 
import GHC.Generics 
import Data.Aeson
import Control.Monad
--------------------------------------
-- Server Response and Client Request
data ServerResponse = ServerResponse 
                    { binaryNum  :: String
                    , decimal    :: String
                    , comp :: String 
                    } deriving (Generic, Eq, Show)
instance ToJSON ServerResponse 

newtype ClientRequest  = ClientRequest {binNum :: String} deriving (Generic, Eq, Show)
instance FromJSON ClientRequest
-------------------------------

-- Our Binary data type .
newtype Bin a = Bin [a] deriving (Eq)

instance Show (Bin Bool) where 
    show (Bin l) = case l of 
                    []       -> ""
                    (b : bs) -> (if b then '1' else '0') : (show $ Bin bs)

-- Useless instances 
instance Functor Bin where 
    fmap f (Bin l) = Bin $ f <$>  l 
instance Applicative Bin where 
    pure a = Bin [a]
    liftA2 f (Bin l) (Bin l') = Bin $ ((uncurry f) <$> (zip l l'))
instance Monad Bin where 
    (Bin l) >>= f = Bin $ concat $ toList <$> f <$> l  


toList :: Bin a -> [a]
toList (Bin l) = l


-- Useful instance
instance Bits (Bin Bool) where 
    bin1 .&. bin2      =  intToBin ((binToInt bin1) .&. (binToInt bin2))
    bin1 .|. bin2      =  intToBin ((binToInt bin1) .|. (binToInt bin2))
    bin1 `xor` bin2    =  intToBin ((binToInt bin1) `xor` (binToInt bin2))
    complement bin     =  (\t -> if t then False else True) <$> bin
    shiftR bin  i      =  intToBin $ (shiftR (binToInt bin) i) 
    rotateR bin i      =  intToBin $ (rotateR (binToInt bin) i) 
    bitSize bin        =  bitSize $ binToInt bin
    bitSizeMaybe  b    =  bitSizeMaybe $ binToInt b 
    isSigned      b    =  isSigned (binToInt b)
    testBit bin i      =  (testBit (binToInt bin) i)
    bit i              =  intToBin $ (bit i :: Int)
    popCount (Bin l)   = length (filter (== True) l) 

-- Helper functions
binToInt :: Bin Bool -> Int 
binToInt (Bin l) = 
     foldr 
     (\t acc -> ((fst t) * (snd t)) + acc) 
     0 
     (zipWith 
      (\bool i -> (if bool then 1 else 0, 2 ^ (i - 1))) 
      l 
      (take (length l) [(length l), (length l) - 1..])
     )

intToBin :: Int -> Bin Bool 

intToBin i = Bin $ intToBin' i 
intToBin' :: Int -> [Bool] 
intToBin' i = 
    case divMod i 2 of 
        (0, r)  -> [if r == 1 then True else False]
        (x, r') -> (if r' == 1 then True else False) : (intToBin' x)









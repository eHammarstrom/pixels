module Codec.BMP.Pixels
  ( Pixel(..)
  , bmpToPixelMatrix
  , getPixel
  , pixelMatrixToBmp
  ) where

import Codec.BMP
import Data.ByteString as B (pack, unpack)
import Data.Word8

data Pixel = Pixel
  { red, green, blue, alpha :: Int
  } deriving (Show, Eq)

bmpToPixelMatrix :: BMP -> [[Pixel]]
bmpToPixelMatrix bmp = do
  let rgba = unpackBMPToRGBA32 bmp
  let (w, _) = bmpDimensions bmp
  let rgba' = splitToPixels (B.unpack rgba)
  chunk w rgba'

getPixel :: (Int, Int) -> [[Pixel]] -> Maybe Pixel
getPixel (x, y) pxs
  | y < length pxs && x < length (head pxs) = Just ((pxs !! y) !! x)
  | otherwise = Nothing

pixelMatrixToBmp :: [[Pixel]] -> BMP
pixelMatrixToBmp mat = do
  let height = length mat
  let width = length (head mat)
  let rgba = pack $ concatMap toWord (concat mat)
  packRGBA32ToBMP width height rgba

{- private -}
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

toWord :: Pixel -> [Word8]
toWord (Pixel r g b a) = intToWord8 r : intToWord8 g : intToWord8 b : [intToWord8 a]

intToWord8 :: Int -> Word8
intToWord8 i
  | i <= 255 = fromIntegral i :: Word8
  | otherwise = error "Int greater than 8 bits"

toPixel :: (Word8, Word8, Word8, Word8) -> Pixel
toPixel (r, g, b, a) =
  Pixel
  {red = fromIntegral r, green = fromIntegral g, blue = fromIntegral b, alpha = fromIntegral a}

splitToPixels :: [Word8] -> [Pixel]
splitToPixels list =
  case list of
    r:g:b:a:xs -> toPixel (r, g, b, a) : splitToPixels xs
    [] -> []
    _ -> error "bitmap does not split into 4 bytes"

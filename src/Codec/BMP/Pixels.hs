module Codec.BMP.Pixels
  ( Pixel(..)
  , bmpToPixelMatrix
  , getPixel
  ) where

import Codec.BMP
import Data.ByteString as B (unpack)
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

{- private -}
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

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

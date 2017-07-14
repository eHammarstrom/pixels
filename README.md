# pixels

Extends [Codec.BMP](https://github.com/benl23x5/bmp) with a pixel abstraction.

Read pixels,
```
Right bmp <- readBMP "palette.bmp"
let matrix = bmpToPixelMatrix bmp -- matrix :: [[Pixel]]
let pixel = getPixel (0, 3) matrix -- pixel ::  Pixel { red, green, blue, alpha :: Int }
```

Write back to bitmap,
```
let bmp = pixelMatrixToBmp matrix -- matrix :: [[Pixel]]
writeBMP "myBitmap.bmp" bmp
```

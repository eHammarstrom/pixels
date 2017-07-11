# pixels

Extends [Codec.BMP](https://github.com/benl23x5/bmp) with a pixel abstraction.

```
Right bmp <- readBMP "palette.bmp"
let matrix = bmpToPixelMatrix bmp -- [[Pixel]]
let pixel = getPixel (0, 3) matrix -- Pixel { red, green, blue, alpha :: Int }
```

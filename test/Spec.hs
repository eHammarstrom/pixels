import Codec.BMP
import Codec.BMP.Pixels
import Test.Hspec

main :: IO ()
main = do
  Right bmp <- readBMP "palette.bmp"
  let matrix = bmpToPixelMatrix bmp
  hspec $
    describe "Codec.BMP.Pixels" $ do
      it "creates a matrix of height 4" $ length matrix `shouldBe` 4
      it "creates a matrix of width 12" $ length (head matrix) `shouldBe` 12
      it "does not create a matrix of height 3" $ length matrix `shouldNotBe` 3
      it "retrieves a {255,255,255,255} pixel at (0,3)" $
        getPixel (0, 3) matrix `shouldBe`
        Just Pixel {red = 255, green = 255, blue = 255, alpha = 255}
      it "retrieves a Nothing pixel at (0,4)" $ getPixel (0, 4) matrix `shouldBe` Nothing

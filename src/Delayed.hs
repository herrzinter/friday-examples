{-# LANGUAGE ScopedTypeVariables #-}
import System.Environment (getArgs)

import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (Z (..), (:.) (..), Rect (..), ix2)

-- Reads an image from a file, applies a composition of transformations to
-- create a centred and squared miniature and then writes the result to a file:
--
-- usage: ./delayed input.png output.png
main :: IO ()
main = do
    [input, output] <- getArgs

    -- Loads the image. Automatically infers the format.
    io <- load Autodetect input

    case io of
        Left err           -> do
            putStrLn "Unable to load the image:"
            print err
        Right (rgb :: RGB) -> do
            let -- Gets the size of the image.
                Z :. h :. w = shape rgb

                -- Creates a Rect object which will be used to define how we
                -- will crop our image. The rectangle is centered on the largest
                -- side of the image.
                rect | w > h     = Rect ((w - h) `quot` 2) 0 h h
                     | otherwise = Rect 0 ((h - w) `quot` 2) w w

                -- Crops the image. Doesn't compute the image into a "real"
                -- image: by using a delayed representation, this intermediate
                -- image will not exist in the computer memory as a large array.
                cropped = delayed $ crop rect rgb

                -- Resizes the image. By using the delayed representation of the
                -- cropped image, our compiler should be able to fuse these two
                -- transformations into a single loop.
                resized = manifest $ resize Bilinear (ix2 250 250) cropped

            -- Saves the resized image. Automatically infers the output format.
            mErr <- save Autodetect output resized
            case mErr of
                Nothing  ->
                    putStrLn "Success."
                Just err -> do
                    putStrLn "Unable to save the image:"
                    print err

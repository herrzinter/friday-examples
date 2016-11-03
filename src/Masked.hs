{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}


import Prelude hiding (map)
import System.Environment (getArgs)
import Vision.Image (MaskedImage, Image, ImagePixel, FromFunction,
                     FromFunctionPixel, DelayedMask, RGB, RGBPixel (..),
                     shape, fromFunction, map, maskedIndex, index, delayed,
                     compute)
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (Z (..), (:.) (..))


-- | Mask an image by replacing pixels in certain columns by 'Nothing'
maskImage
    :: (Image src, FromFunction res,
        FromFunctionPixel res ~ Maybe (ImagePixel src))
    => src -- ^ Unmasked source image
    -> res -- ^ Resulting masked image
maskImage img = fromFunction (shape img) maskPixel
  where
    maskPixel (Z :. y :. x) =
        -- Switch between masking and not masking every 50 columns
        if (round (fromIntegral x / 50 :: Rational) :: Int) `mod` 2 /= 0
        -- Mask by wrapping the original pixel in the 'Just' constructor
        then Just $ img `index` (Z :. y :. x)
        -- Mask by replacing the original pixel with 'Nothing'
        else Nothing

-- | Convert an masked to an unmasked image by replacing 'Nothing' with default
-- pixel value
unmaskImage
    :: (MaskedImage src, FromFunction res,
        FromFunctionPixel res ~ ImagePixel src)
    =>  ImagePixel src  -- ^ Default pixel substituted to all masked values
    ->  src             -- ^ Masked input image
    ->  res             -- ^ Resulting unmasked image
unmaskImage nothingPixel img = fromFunction (shape img) unmaskPoint
  where
    unmaskPoint point =
        case img `maskedIndex` point of
            Just pixel  -> pixel
            Nothing     -> nothingPixel

-- | Invert all three channels of an rgb pixel
invertRGBPixel (RGBPixel r g b) = RGBPixel (255 - r) (255 - g) (255 - b)


-- | Forces an image to be in its delayedMask represenation. Does nothing.
delayedMasked :: DelayedMask p -> DelayedMask p
delayedMasked = id


-- usage: ./delayed input.png output.png
main = do
    [input, output] <- getArgs

    -- Loads the image. Automatically infers the format.
    io <- load Autodetect input

    case io of
        Left err           -> do
            putStrLn "Unable to load the image:"
            print err
        Right (rgb :: RGB) -> do
            -- The 'delayedMasked' and 'delayed' functions are used instead of
            -- type annotations, eg.
            -- >    masked = maskImage rgb :: DelayedMask RGBPixel
            let -- Mask image by replacing certain pixels with 'Nothing' and
                -- wrapping the others with 'Just'
                masked = delayedMasked $ maskImage rgb
                -- Invert all rgb pixels. The map operation automatically only
                -- alters 'Just' pixels and ignores 'Nothing' pixels.
                masked' = delayedMasked $ map invertRGBPixel masked
                -- Unmask image by replacing 'Nothing' with a blue pixel
                unmaskedImage = delayed $ unmaskImage (RGBPixel 0 0 255) masked'

            -- Up until now, 'unmaskImage' is only a chain of delayed
            -- operations. Before the image is saved, these operations have to
            -- be executed, and the actual output image has to be computed in
            -- a 'Manifest' represenation. Alternatively to using the 'compute'
            -- function, the type could again just be annotated directly, eg.
            -- >    save Autodetect output (unmaskedImage :: RGB)'
            -- or the 'delayed' function used to create 'unmaskedImage' could be
            -- replaced by the 'manifest' function and consequentielly, the
            -- 'compute' function could be omitted.
            mErr <- save Autodetect output (compute unmaskedImage)

            case mErr of
                Nothing  ->
                    putStrLn "Success."
                Just err -> do
                    putStrLn "Unable to save the image:"
                    print err

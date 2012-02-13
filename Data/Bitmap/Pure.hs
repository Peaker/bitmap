
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Pure
-- Version     : 0.0.1
-- License     : BSD3
-- Copyright   : (c) 2009 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI, CPP and ScopedTypeVariables
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Data.Bitmap.Pure 
  ( 
    module Data.Bitmap.Base
    
    -- * Creating bitmaps
  , emptyBitmap
  , createSingleChannelBitmap
    -- * Mapping over bitmaps
  , componentMap
  , componentMap'
    -- * Cropping and extending
  , copySubImage
  , copySubImage'  
    -- * Manipulating channels
  , combineChannels 
  , extractChannels 
  , extractSingleChannel 
    -- * Bilinear resampling
  , bilinearResample
  , bilinearResampleChannel  
    -- * Blending
  , blendBitmaps  
  , blendChannels  
    -- * Gamma correction
  , powerlawGammaCorrection
  , powerlawGammaCorrectionChannel   
    -- * Conversion to ByteString
  , bitmapToByteString  
  ) 
  where

--------------------------------------------------------------------------------

import Data.Word

import Foreign hiding (unsafePerformIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Data.Bitmap.Base
import Data.Bitmap.Internal
import qualified Data.Bitmap.IO as IO

import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | A bitmap filled with zero values.
-- Note: we /cannot/ guarantee the alignment
-- of the memory block (but typically it is aligned at least to machine word boundary),
-- but what we /can/ guarantee is that the rows are properly padded.
emptyBitmap 
  :: forall t. PixelComponent t 
  => Size             -- ^ (width,height)
  -> NChn             -- ^ number of channels (components\/pixel)
  -> Maybe Alignment  -- ^ the row alignment of the new image
  -> Bitmap t
emptyBitmap siz nchn malign = unsafePerformIO $ IO.newBitmap siz nchn malign

-- | Creates a single channel bitmap from a function.
-- This is probably a bit slow.
createSingleChannelBitmap
  :: forall t. PixelComponent t 
  => Size               -- ^ (width,height)
  -> Maybe Alignment    -- ^ the row alignment of the new image
  -> (Int -> Int -> t)  -- ^ the function used to create the bitmap
  -> Bitmap t
createSingleChannelBitmap siz maling fun = unsafePerformIO $ 
  IO.createSingleChannelBitmap siz maling fun 

--------------------------------------------------------------------------------

-- | Warning: this is probably slow.
componentMap :: PixelComponent s => (s -> s) -> Bitmap s -> Bitmap s
componentMap f bm = unsafePerformIO $ IO.componentMap f bm

-- | Warning: this is probably slow.
componentMap' :: (PixelComponent s, PixelComponent t) => (s -> t) -> Bitmap s -> Maybe Alignment -> Bitmap t
componentMap' f bm malign = unsafePerformIO $ IO.componentMap' f bm malign

--------------------------------------------------------------------------------

-- | Copies a subrectangle of the source image into a new image.  
copySubImage
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Bitmap t
copySubImage bm ofs siz = unsafePerformIO $ IO.copySubImage bm ofs siz

-- | Copy into a new \"black\" bitmap; common generalization of crop and extend.
copySubImage'
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Size             -- ^ target image size
  -> Offset           -- ^ target rectangle offset
  -> Bitmap t
copySubImage' bm1 ofs1 rsiz tsiz ofs2 = unsafePerformIO $
  IO.copySubImage' bm1 ofs1 rsiz tsiz ofs2
  
--------------------------------------------------------------------------------

extractSingleChannel 
  :: PixelComponent t 
  => Bitmap t               -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> Int                    -- ^ source channel index
  -> Bitmap t
extractSingleChannel bm1 malign j = unsafePerformIO $ 
  IO.extractSingleChannel bm1 malign j 

extractChannels :: PixelComponent t => Bitmap t -> Maybe Alignment -> [Bitmap t]
extractChannels bm malign = unsafePerformIO $
  IO.extractChannels bm malign 

combineChannels :: forall t. PixelComponent t => [Bitmap t] -> Maybe Alignment -> Bitmap t
combineChannels bms malign = unsafePerformIO $
  IO.combineChannels bms malign

--------------------------------------------------------------------------------

bilinearResample 
  :: PixelComponent t 
  => Bitmap t           -- ^ source image
  -> Size               -- ^ target image size
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
bilinearResample bm siz malign = unsafePerformIO $ 
  IO.bilinearResample bm siz malign

bilinearResampleChannel
  :: PixelComponent t 
  => Bitmap t           -- ^ source image
  -> Int                -- ^ source channel indexe
  -> Size               -- ^ target image size
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
bilinearResampleChannel bm j siz malign = unsafePerformIO $ 
  IO.bilinearResampleChannel bm j siz malign
  
--------------------------------------------------------------------------------

-- | Blends two bitmaps with the given weights; that is, the result is
-- the specified linear combination. If the values are outside the allowed
-- range (this can happen with the Word8, Word16, Word32 types and weights
-- whose sum is bigger than 1, or with a negative weight), then they are
-- clipped. The clipping /does not/ happen with the Float component type.
blendBitmaps
  :: PixelComponent t 
  => Float           -- ^ weight 1
  -> Float           -- ^ weight 2
  -> Bitmap t        -- ^ source image 1 
  -> Bitmap t        -- ^ source image 2
  -> Maybe Alignment -- ^ target alignment
  -> Bitmap t
blendBitmaps w1 w2 bm1 bm2 malign = unsafePerformIO $ 
  IO.blendBitmaps w1 w2 bm1 bm2 malign

blendChannels
  :: PixelComponent t 
  => Float           -- ^ weight 1
  -> Float           -- ^ weight 2
  -> Bitmap t        -- ^ source image 1 
  -> Int             -- ^ channel index 1
  -> Bitmap t        -- ^ source image 2
  -> Int             -- ^ channel index 2
  -> Maybe Alignment -- ^ target alignment
  -> Bitmap t
blendChannels w1 w2 bm1 ofs1 bm2 ofs2 malign = unsafePerformIO $ 
  IO.blendChannels w1 w2 bm1 ofs1 bm2 ofs2 malign
  
--------------------------------------------------------------------------------
  
-- | This is equivalent to @componentMap (\c -> c^gamma)@, except that
-- @(^)@ is defined only for integral exponents; but should be faster anyway.
powerlawGammaCorrection
  :: PixelComponent t 
  => Float              -- ^ gamma
  -> Bitmap t           -- ^ source image
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
powerlawGammaCorrection gamma bm malign = unsafePerformIO $ 
  IO.powerlawGammaCorrection gamma bm malign

powerlawGammaCorrectionChannel
  :: PixelComponent t 
  => Float              -- ^ gamma
  -> Bitmap t           -- ^ source image
  -> Int                -- ^ source channel indexe
  -> Maybe Alignment    -- ^ target image row alignment
  -> Bitmap t  
powerlawGammaCorrectionChannel gamma bm j malign = unsafePerformIO $ 
  IO.powerlawGammaCorrectionChannel gamma bm j malign
  
--------------------------------------------------------------------------------

-- | Note that the data is /shared/; and also that the resulting ByteString
-- is encoded using the host machine's endianness.
bitmapToByteString :: PixelComponent t => Bitmap t -> ByteString
bitmapToByteString bm = bs where
  bs = B.fromForeignPtr (castForeignPtr $ bitmapPtr bm) 0 n
  n = bitmapSizeInBytes bm 

{-  
-- | As its name says, this pretty much unsafe!
reallyUnsafeBitmapFromByteString :: forall t. ByteString -> Size -> NChn -> Padding -> Bitmap t
reallyUnsafeBitmapFromByteString bs siz nchn pad = 
  if n > len || ofs /= 0 
    then error "reallyUnsafeBitmapFromByteString: better than segfault :)"
    else bm   
  where
    bm = Bitmap 
      { bitmapSize = siz
      , bitmapNChannels = nchn
      , bitmapPtr = fptr 
      , bitmapRowPadding = pad
      , bitmapRowAlignment = 1
      } :: Bitmap t
    n = bitmapSizeInBytes bm
    (fptr,ofs,len) = B.toForeignPtr bm
-}
  
--------------------------------------------------------------------------------


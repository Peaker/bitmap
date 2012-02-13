
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Internal
-- Version     : 0.0.1
-- License     : BSD3
-- Copyright   : (c) 2009 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI, CPP and ScopedTypeVariables
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

{-# LANGUAGE CPP, ForeignFunctionInterface, ScopedTypeVariables #-}
module Data.Bitmap.Internal where

--------------------------------------------------------------------------------

import Control.Monad

--import Data.Array.IArray
import Data.Word

import Foreign
import Foreign.C

--------------------------------------------------------------------------------

data PixelComponentType 
  = PctWord8 
  | PctWord16
  | PctWord32 
  | PctFloat
  deriving Show
  
pixelComponentSize :: PixelComponentType -> Int
pixelComponentSize pct = case pct of
  PctWord8  -> 1 
  PctWord16 -> 2 
  PctWord32 -> 4 
  PctFloat  -> 4 
  
--------------------------------------------------------------------------------

class (Num t, Storable t) => PixelComponent t where
  c_type :: t -> CInt
--  nbytes :: t -> Int
--  nbytes x = sizeOf x
  toFloat   :: t -> Float
  fromFloat :: Float -> t
  
pixelComponentType :: PixelComponent t => t -> PixelComponentType
pixelComponentType t = case c_type t of
  1 -> PctWord8
  2 -> PctWord16
  3 -> PctWord32
  4 -> PctFloat

-- hmm hmm let's hope ghc will inline this into an 
-- inlined function if i explicitely ask for it... 
{-# INLINE clamp #-}
clamp :: Float -> Float
clamp = min 1 . max 0
 
instance PixelComponent Word8 where 
  {-# SPECIALIZE instance PixelComponent Word8  #-}
  c_type _ = 1
  fromFloat = floor . (+0.5) . (*255) . min 1 . max 0
  toFloat = (*3.92156862745098e-3) . fromIntegral     -- 1/255

instance PixelComponent Word16 where 
  {-# SPECIALIZE instance PixelComponent Word16 #-}
  c_type _ = 2
  fromFloat = floor . (+0.5) . (*65535) . min 1 . max 0
  toFloat = (*1.5259021896696422e-5) . fromIntegral   -- 1/65535

instance PixelComponent Word32 where 
  {-# SPECIALIZE instance PixelComponent Word32 #-}
  c_type _ = 3
  fromFloat = floor . (+0.5) . (*4294967295) . min 1 . max 0
  toFloat = (*2.3283064370807974e-10) . fromIntegral  -- 1/(2^32-1)
  
instance PixelComponent Float where 
  {-# SPECIALIZE instance PixelComponent Float  #-}
  c_type _ = 4
  fromFloat = id
  toFloat = id

--------------------------------------------------------------------------------

{-# SPECIALIZE isValidAlignment :: Int -> Bool #-}
isValidAlignment :: Integral a => a -> Bool
isValidAlignment a = elem a [1,2,4,8,16]

--------------------------------------------------------------------------------
  
-- to provide better documentation
type Size   = (Int,Int)
type Offset = (Int,Int)
type NChn      = Int
type Padding   = Int
type Alignment = Int

data Bitmap t = Bitmap
  { bitmapSize      :: Size          -- ^ (width,height)
  , bitmapNChannels :: NChn          -- ^ number of channels (eg. 3 for RGB)
  , bitmapPtr  :: ForeignPtr t       -- ^ pointer to the data
  , bitmapRowPadding   :: Padding    -- ^ the padding of the rows, measured in /bytes/
  , bitmapRowAlignment :: Alignment  -- ^ the alignment of the rows (in bytes)
  }
  deriving Show

bitmapComponentSizeInBytes :: forall t. PixelComponent t => Bitmap t -> Int
bitmapComponentSizeInBytes _ = sizeOf (undefined::t) 

bitmapPixelSizeInBytes :: PixelComponent t => Bitmap t -> Int
bitmapPixelSizeInBytes bm = bitmapNChannels bm * bitmapComponentSizeInBytes bm
  
bitmapUnpaddedRowSizeInBytes :: forall t. PixelComponent t => Bitmap t -> Int  
bitmapUnpaddedRowSizeInBytes bm = w * sizeOf (undefined::t) * nchn where
  (w,h) = bitmapSize bm
  nchn  = bitmapNChannels bm   
  
bitmapPaddedRowSizeInBytes :: PixelComponent t => Bitmap t -> Int  
bitmapPaddedRowSizeInBytes bm = bitmapUnpaddedRowSizeInBytes bm + bitmapRowPadding bm 
  
bitmapSizeInBytes :: PixelComponent t => Bitmap t -> Int 
bitmapSizeInBytes bm = h*x where
  x = bitmapPaddedRowSizeInBytes bm
  (_,h) = bitmapSize bm  
 
-- | The width divided by the height.
bitmapAspect :: Fractional a => Bitmap t -> a
bitmapAspect bm = (fromIntegral x / fromIntegral y) where
  (x,y) = bitmapSize bm

--------------------------------------------------------------------------------

-- we mimic the OpenGL padding at the moment
recommendedPadding :: forall t. PixelComponent t => Bitmap t -> Int
recommendedPadding bm = pad where
  (w,_) = bitmapSize bm
  n = bitmapNChannels bm
  b = bitmapRowAlignment bm
  s = sizeOf (undefined::t)
  a = if b<s then s else b
  k = case divMod a s of (q,r) -> if r==0 then q else error "recommendedPadding: should not happen"
  pad = s * ( k * div (n*w + k-1) k - n*w )    
  
--------------------------------------------------------------------------------

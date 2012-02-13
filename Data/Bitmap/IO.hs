
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.IO
-- Version     : 0.0.1
-- License     : BSD3
-- Copyright   : (c) 2009 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI, CPP and ScopedTypeVariables
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

-- | The full, mutable API in the IO monad.

{-# LANGUAGE CPP, ForeignFunctionInterface, ScopedTypeVariables #-}
{-# CFILES cbits/bm.c #-}  -- for Hugs 
module Data.Bitmap.IO
  ( 
    module Data.Bitmap.Base
    -- * Creating and accessing bitmaps
  , newBitmap
  , newBitmapUninitialized
  , createSingleChannelBitmap
  , copyBitmapFromPtr
  , bitmapFromForeignPtrUnsafe
  , withBitmap
    -- * Mapping over bitmaps
  , componentMap
  , componentMap'
  , componentMapInPlace
    -- * Cropping and extending
  , copySubImage
  , copySubImage'  
  , copySubImageInto
    -- * Manipulating channels
  , combineChannels 
  , extractChannels 
  , extractSingleChannel 
  , extractChannelInto
    -- * Bilinear resampling
  , bilinearResample
  , bilinearResampleChannel
  , bilinearResampleChannelInto
    -- * Blending
  , blendBitmaps
  , blendChannels
  , blendChannelsInto
    -- * Gamma correction
  , powerlawGammaCorrection
  , powerlawGammaCorrectionChannel  
  , powerlawGammaCorrectionChannelInto  
    -- * Conversion to\/from ByteString
  , copyBitmapToByteString
  , copyBitmapFromByteString
    -- * Reading and writing pixels
  , withComponentPtr
  , unsafeReadComponent
  , unsafeWriteComponent
  , unsafeReadComponents
  , unsafeWriteComponents
  , unsafeReadPixel
  , unsafeReadPixel1
  , unsafeReadPixel2
  , unsafeReadPixel3
  , unsafeReadPixel4
  , unsafeWritePixel1
  , unsafeWritePixel2
  , unsafeWritePixel3
  , unsafeWritePixel4
  ) 
  where
  
--------------------------------------------------------------------------------

import Control.Monad

--import Data.Array.IArray

import Data.Word
import Data.List (nub)

import Foreign
import Foreign.C

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Data.Bitmap.Internal
import Data.Bitmap.Base

--------------------------------------------------------------------------------

defaultAlignment :: Int
defaultAlignment = 4

validateMaybeAlignment :: Maybe Alignment -> Alignment
validateMaybeAlignment = maybe defaultAlignment validateAlignment

validateAlignment :: Alignment -> Alignment
validateAlignment k = 
  if isValidAlignment k 
    then k 
    else error "invalid row alignment (allowed values: 1, 2, 4 and 8)"
  
--------------------------------------------------------------------------------
  
-- we do not initialize the new bitmap!
newBitmapRaw :: forall t. PixelComponent t => Size -> NChn -> Padding -> Alignment -> IO (Bitmap t)   
newBitmapRaw siz nchn pad align = do
  let bm0 = Bitmap 
        { bitmapSize = siz
        , bitmapNChannels = nchn
        , bitmapPtr = undefined 
        , bitmapRowPadding = pad
        , bitmapRowAlignment = align
        } :: Bitmap t
      len = bitmapSizeInBytes bm0
  fptr <- mallocForeignPtrBytes len :: IO (ForeignPtr t)
  return $ bm0 { bitmapPtr = fptr }
  
-- | Note: we /cannot/ guarantee the alignment
-- of the memory block (but typically it is aligned at least to machine word boundary),
-- but what we /can/ guarantee is that the rows are properly padded.
--
-- At the moment, the default alignment is 4, valid alignments are 1, 2, 4, 8 and 16,
-- and the padding method is compatible with the OpenGL one (that is, the padding is the
-- smallest multiple of a component size such that the next row is aligned).
-- 
-- The resulting new bitmap is filled with zeros.
newBitmap 
  :: forall t. PixelComponent t 
  => Size             -- ^ (width,height)
  -> NChn             -- ^ number of channels (components\/pixel)
  -> Maybe Alignment  -- ^ the row alignment of the new image
  -> IO (Bitmap t)
newBitmap siz nchn malign = do  
  bm <- newBitmapUninitialized siz nchn malign -- :: IO (Bitmap t)
  let fptr = bitmapPtr bm
      len  = bitmapSizeInBytes bm
  withForeignPtr fptr $ \p -> c_memset (castPtr p) len 0
  return bm

newBitmapUninitialized :: forall t. PixelComponent t => Size -> NChn -> Maybe Alignment -> IO (Bitmap t)
newBitmapUninitialized siz nchn malign = do
  let bm0 = Bitmap 
        { bitmapSize = siz
        , bitmapNChannels = nchn
        , bitmapPtr = undefined 
        , bitmapRowPadding = pad
        , bitmapRowAlignment = align
        } :: Bitmap t
      x0 = bitmapUnpaddedRowSizeInBytes bm0
      
      align = validateMaybeAlignment malign
      pad = recommendedPadding bm0
{-
      (pad,align) = case malign of
        Nothing -> (0,1)
        Just align -> let x1 = align * ((x0 + align - 1) `div` align) in (x1 - x0, align)
-}        
  newBitmapRaw siz nchn pad align
  
-- | Creates a new single-channel bitmap, using the given function to compute
-- the pixel values.
-- Warning, this is probably slow!  
createSingleChannelBitmap 
  :: forall t. PixelComponent t 
  => Size               -- ^ (width,height)
  -> Maybe Alignment    -- ^ the row alignment of the new image
  -> (Int -> Int -> t)  -- ^ the function we will use to fill the bitmap
  -> IO (Bitmap t)
createSingleChannelBitmap siz malign fun = do  
  bm <- newBitmapUninitialized siz 1 malign 
  let fptr = bitmapPtr bm
      len  = bitmapSizeInBytes bm
      f :: Int -> Int -> t -> t
      f x y _ = fun x y 
  genericComponentMapWithPos f bm bm
  return bm

{-    
createBitmap    
  :: forall t. PixelComponent t 
  => Size               -- ^ (width,height)
  -> Maybe Alignment    -- ^ the row alignment of the new image
  -> [Int -> Int -> t]  -- ^ the functions we will use to fill the bitmap
  -> IO (Bitmap t)
createBitmap siz malign funs = do 
  let nchn = length funs 
  bm <- newBitmapUninitialized siz nchn malign 
  let fptr = bitmapPtr bm
      len  = bitmapSizeInBytes bm
      f :: Int -> Int -> t -> t
      f x y _ = fun x y 
  genericComponentMapWithPos f bm bm
  return bm
-}
    
copyBitmapFromPtr 
  :: forall t. PixelComponent t 
  => Size       -- ^ (width,height) of the source
  -> NChn       -- ^ number of channels in the source 
  -> Padding    -- ^ source padding
  -> Ptr t      -- ^ the source
  -> Maybe Alignment  -- ^ target alignment
  -> IO (Bitmap t)
copyBitmapFromPtr siz@(w,h) nchn srcpad srcptr tgtmalign = do
  bm <- newBitmapUninitialized siz nchn tgtmalign
  withBitmap bm $ \_ _ _ tgtptr -> do
    let pure_line = bitmapUnpaddedRowSizeInBytes bm
        src_line  = pure_line + srcpad
        tgt_line  = bitmapPaddedRowSizeInBytes bm
    forM_ [0..h-1] $ \y -> do
      let p = srcptr `myPlusPtr` (y*src_line)
          q = tgtptr `myPlusPtr` (y*tgt_line)
      c_memcpy (castPtr p) (castPtr q) pure_line 
  return bm  

bitmapFromForeignPtrUnsafe 
  :: forall t. PixelComponent t 
  => Size -> NChn -> Alignment -> Padding -> ForeignPtr t -> Bitmap t
bitmapFromForeignPtrUnsafe siz nchn align pad fptr = Bitmap
  { bitmapSize      = siz 
  , bitmapNChannels = nchn 
  , bitmapPtr       = fptr 
  , bitmapRowPadding   = pad 
  , bitmapRowAlignment = align
  }
      
-- | @withBitmap bitmap $ \\(w,h) nchn padding ptr -> ...@
withBitmap :: PixelComponent t => Bitmap t -> (Size -> NChn -> Padding -> Ptr t -> IO a) -> IO a
withBitmap bm action = 
  withForeignPtr (bitmapPtr bm) $ \p -> 
    action (bitmapSize bm) (bitmapNChannels bm) (bitmapRowPadding bm) p

--------------------------------------------------------------------------------

-- | Note that the resulting pointer is valid only within a line (because of the padding)
withComponentPtr 
  :: forall t a. PixelComponent t 
  => Bitmap t         -- ^ the bitmap
  -> Offset           -- ^ position (x,y)
  -> Int              -- ^ channel index {0,1,...,nchannels-1}
  -> (Ptr t -> IO a)  -- ^ user action
  -> IO a
withComponentPtr bm (x,y) ofs action = 
  withForeignPtr (bitmapPtr bm) $ \p -> do
    let nchn = bitmapNChannels bm
        rowsize = bitmapPaddedRowSizeInBytes bm
        q = p `myPlusPtr` ( ( nchn*x + ofs ) * sizeOf (undefined::t) + y * rowsize ) 
    action q
    
-- | It is not very efficient to read\/write lots of pixels this way.
unsafeReadComponent 
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> IO t
unsafeReadComponent bm xy ofs = withComponentPtr bm xy ofs $ peek
    
unsafeWriteComponent
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> t             -- ^ the value to write
  -> IO ()
unsafeWriteComponent bm xy ofs value = withComponentPtr bm xy ofs $ \q -> poke q value

-- | Please note that the component array to read shouldn't cross 
-- the boundary between lines.
unsafeReadComponents
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> Int           -- ^ the number of components to read
  -> IO [t]
unsafeReadComponents bm xy ofs k = withComponentPtr bm xy ofs $ \p -> peekArray k p

-- | Please note that the component array to write shouldn't cross 
-- the boundary between lines.
unsafeWriteComponents
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> Int           -- ^ channel index {0,1,...,nchannels-1}
  -> [t]           -- ^ the components to write
  -> IO ()
unsafeWriteComponents bm xy ofs values = withComponentPtr bm xy ofs $ \q -> pokeArray q values

unsafeReadPixel 
  :: PixelComponent t 
  => Bitmap t      -- ^ the bitmap
  -> Offset        -- ^ position (x,y)
  -> IO [t]
unsafeReadPixel bm xy = unsafeReadComponents bm xy 0 (bitmapNChannels bm)
   
-- | These functions assume that the number of channels of the bitmap
-- agrees with the number suffix of the function. 
--
-- (Maybe I should put
-- the number of components into the Bitmap type? But that would cause
-- different problems...)
unsafeReadPixel1 :: PixelComponent t => Bitmap t -> Offset -> IO t
unsafeReadPixel2 :: PixelComponent t => Bitmap t -> Offset -> IO (t,t)
unsafeReadPixel3 :: PixelComponent t => Bitmap t -> Offset -> IO (t,t,t)
unsafeReadPixel4 :: PixelComponent t => Bitmap t -> Offset -> IO (t,t,t,t)

unsafeWritePixel1 :: PixelComponent t => Bitmap t -> Offset -> t -> IO ()
unsafeWritePixel2 :: PixelComponent t => Bitmap t -> Offset -> (t,t) -> IO ()
unsafeWritePixel3 :: PixelComponent t => Bitmap t -> Offset -> (t,t,t) -> IO ()
unsafeWritePixel4 :: PixelComponent t => Bitmap t -> Offset -> (t,t,t,t) -> IO ()

unsafeReadPixel1 bm xy = withComponentPtr bm xy 0 $ \p -> liftM (\[x]       ->  x       ) $ peekArray 1 p
unsafeReadPixel2 bm xy = withComponentPtr bm xy 0 $ \p -> liftM (\[x,y]     -> (x,y)    ) $ peekArray 2 p
unsafeReadPixel3 bm xy = withComponentPtr bm xy 0 $ \p -> liftM (\[x,y,z]   -> (x,y,z)  ) $ peekArray 3 p
unsafeReadPixel4 bm xy = withComponentPtr bm xy 0 $ \p -> liftM (\[x,y,z,w] -> (x,y,z,w)) $ peekArray 4 p

unsafeWritePixel1 bm xy  x        = withComponentPtr bm xy 0 $ \q -> pokeArray q [x]
unsafeWritePixel2 bm xy (x,y)     = withComponentPtr bm xy 0 $ \q -> pokeArray q [x,y]
unsafeWritePixel3 bm xy (x,y,z)   = withComponentPtr bm xy 0 $ \q -> pokeArray q [x,y,z]
unsafeWritePixel4 bm xy (x,y,z,w) = withComponentPtr bm xy 0 $ \q -> pokeArray q [x,y,z,w]

--------------------------------------------------------------------------------

{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word8  -> Ptr Word8  -> IO ()) -> Bitmap Word8  -> Bitmap Word8  -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word16 -> Ptr Word16 -> IO ()) -> Bitmap Word16 -> Bitmap Word16 -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word32 -> Ptr Word32 -> IO ()) -> Bitmap Word32 -> Bitmap Word32 -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Float  -> Ptr Float  -> IO ()) -> Bitmap Float  -> Bitmap Float  -> IO () #-}

{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word8  -> Ptr Float  -> IO ()) -> Bitmap Word8  -> Bitmap Float  -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Float  -> Ptr Word8  -> IO ()) -> Bitmap Float  -> Bitmap Word8  -> IO () #-}

{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Word16 -> Ptr Float  -> IO ()) -> Bitmap Word16 -> Bitmap Float  -> IO () #-}
{-# SPECIALIZE genericComponentRowMap 
      :: (Int -> Int -> Ptr Float  -> Ptr Word16 -> IO ()) -> Bitmap Float  -> Bitmap Word16 -> IO () #-}

-- the first Int is the y position
-- the second Int is the number of pixel components (nchn*width)
genericComponentRowMap 
  :: (PixelComponent s, PixelComponent t) 
  => (Int -> Int -> Ptr s -> Ptr t -> IO ())        -- ^ ypos totalNumberOfComps src tgt
  -> Bitmap s -> Bitmap t -> IO ()
genericComponentRowMap rowAction bm1 bm2 = do

  let (w1,h1) = bitmapSize       bm1
      pad1    = bitmapRowPadding bm1
      nchn1   = bitmapNChannels  bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1
      
  let (w2,h2) = bitmapSize       bm2
      pad2    = bitmapRowPadding bm2
      nchn2   = bitmapNChannels  bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2
    
  let minw = min w1 w2  
      npc = nchn1 * minw
      
  when (nchn1 /= nchn2) $ 
    error "bitmap/genericRowMap: number of channels disagree" 
    
  withForeignPtr fptr1 $ \ptr1 -> withForeignPtr fptr2 $ \ptr2 -> 
    forM_ (zip3 [0..h1-1]
               (map (*xlen1) [0..h1-1]) 
               (map (*xlen2) [0..h2-1])) $ \(ypos,vo1,vo2) -> do
      let p1 = ptr1 `myPlusPtr` vo1     
          p2 = ptr2 `myPlusPtr` vo2     
      rowAction ypos npc p1 p2 

-------

-- userAction ypos width ptr1 nchn1 ptr2 nchn2 
genericPixelRowMap 
  :: (PixelComponent s, PixelComponent t) 
  => (Int -> Int -> Ptr s -> NChn -> Ptr t -> NChn -> IO ())    -- ^ ypos width ptr1 nchn1 ptr2 nchn2 
  -> Bitmap s -> Bitmap t -> IO ()
genericPixelRowMap rowAction bm1 bm2 = do

  let (w1,h1) = bitmapSize       bm1
      pad1    = bitmapRowPadding bm1
      nchn1   = bitmapNChannels  bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1
      
  let (w2,h2) = bitmapSize       bm2
      pad2    = bitmapRowPadding bm2
      nchn2   = bitmapNChannels  bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2
    
  let minw = min w1 w2 

  withForeignPtr fptr1 $ \ptr1 -> withForeignPtr fptr2 $ \ptr2 -> 
    forM_ (zip3 [0..h1-1] 
                (map (*xlen1) [0..h1-1]) 
                (map (*xlen2) [0..h2-1])) $ \(ypos,o1,o2) -> do
      let p1 = ptr1 `myPlusPtr` o1     
          p2 = ptr2 `myPlusPtr` o2     
      rowAction ypos minw p1 nchn1 p2 nchn2

--------------------------------------------------------------------------------
      
{-# SPECIALIZE genericComponentMap :: (Word8  -> Word8 ) -> Bitmap Word8  -> Bitmap Word8  -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Word16 -> Word16) -> Bitmap Word16 -> Bitmap Word16 -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Word32 -> Word32) -> Bitmap Word32 -> Bitmap Word32 -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Float  -> Float ) -> Bitmap Float  -> Bitmap Float  -> IO () #-}      

{-# SPECIALIZE genericComponentMap :: (Word8  -> Float ) -> Bitmap Word8  -> Bitmap Float  -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Float  -> Word8 ) -> Bitmap Float  -> Bitmap Word8  -> IO () #-}
      
{-# SPECIALIZE genericComponentMap :: (Word16 -> Float ) -> Bitmap Word16 -> Bitmap Float  -> IO () #-}      
{-# SPECIALIZE genericComponentMap :: (Float  -> Word16) -> Bitmap Float  -> Bitmap Word16 -> IO () #-}      
      
genericComponentMap 
  :: forall s t . (PixelComponent s, PixelComponent t) 
  => (s -> t) -> Bitmap s -> Bitmap t -> IO ()  
genericComponentMap f bm1 bm2 = genericComponentRowMap g bm1 bm2 where
  h :: (Ptr s, Ptr t) -> Int -> IO (Ptr s, Ptr t)
  h (q1,q2) _ = do
    x <- peek q1
    poke q2 (f x)
    return (advancePtr1 q1, advancePtr1 q2)
  g :: Int -> Int -> Ptr s -> Ptr t -> IO ()
  g ypos n p1 p2 = do
    foldM_ h (p1,p2) [0..n-1]

{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Word8  -> Word8 ) -> Bitmap Word8  -> Bitmap Word8  -> IO () #-}      
{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Word16 -> Word16) -> Bitmap Word16 -> Bitmap Word16 -> IO () #-}      
{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Word32 -> Word32) -> Bitmap Word32 -> Bitmap Word32 -> IO () #-}      
{-# SPECIALIZE genericComponentMapWithPos :: (Int -> Int -> Float  -> Float ) -> Bitmap Float  -> Bitmap Float  -> IO () #-}      

genericComponentMapWithPos 
  :: forall s t . (PixelComponent s, PixelComponent t) 
  => (Int -> Int -> s -> t) -> Bitmap s -> Bitmap t -> IO ()  
genericComponentMapWithPos f bm1 bm2 = genericComponentRowMap g bm1 bm2 where
  h :: Int -> (Ptr s, Ptr t) -> Int -> IO (Ptr s, Ptr t)
  h ypos (q1,q2) xpos = do
    x <- peek q1
    poke q2 (f xpos ypos x)
    return (advancePtr1 q1, advancePtr1 q2)
  g :: Int -> Int -> Ptr s -> Ptr t -> IO ()
  g ypos n p1 p2 = do
    foldM_ (h ypos) (p1,p2) [0..n-1]

--------------------------------------------------------------------------------

-- | Maps a function over each component of each pixel. Warning: this is probably slow!
-- Use a specialized function if there is one for your task.
{- 
-- Note: We don't do the more general (s->t) here, because then we would have no idea 
-- about the padding in the new bitmap. See `componentMap'` for that.
-}
componentMap :: PixelComponent s => (s -> s) -> Bitmap s -> IO (Bitmap s)
componentMap f bm1 = do
  let siz   = bitmapSize bm1
      nchn  = bitmapNChannels bm1
      align = bitmapRowAlignment bm1
  bm2 <- newBitmapUninitialized siz nchn (Just align) 
  genericComponentMap f bm1 bm2 
  return bm2

componentMapInPlace :: PixelComponent s => (s -> s) -> Bitmap s -> IO ()
componentMapInPlace f bm = do
  genericComponentMap f bm bm
    
-- See the comments at 'componentMap'.
componentMap' 
  :: (PixelComponent s, PixelComponent t) 
  => (s -> t) 
  -> Bitmap s           -- ^ source bitmap
  -> Maybe Alignment    -- ^ row alignment of the resulting bitmap
  -> IO (Bitmap t)
componentMap' f bm1 malign = do
  let siz  = bitmapSize bm1
      nchn = bitmapNChannels bm1
      x = bitmapPaddedRowSizeInBytes bm1 
  bm2 <- newBitmapUninitialized siz nchn malign 
  genericComponentMap f bm1 bm2 
  return bm2
  
--------------------------------------------------------------------------------

-- | Copies a subrectangle of the source image into a new image.  
copySubImage
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> IO (Bitmap t)
copySubImage bm ofs1 siz1 = copySubImage' bm ofs1 siz1 (0,0) siz1  

-- | Copy into a new \"black\" bitmap; common generalization of crop and extend.
copySubImage'
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Size             -- ^ target image size
  -> Offset           -- ^ target rectangle offset
  -> IO (Bitmap t)
copySubImage' bm1 ofs1 rsiz tsiz ofs2 = do
  let align   = bitmapRowAlignment  bm1
      nchn    = bitmapNChannels  bm1
  bm2 <- newBitmap tsiz nchn (Just align)
  copySubImageInto bm1 ofs1 rsiz bm2 ofs2
  return bm2

-- | The source rectangle may be arbitrary, may or may not intersect the
-- source image in any way. We only copy the intersection of the rectangle
-- with the image.  
copySubImageInto 
  :: PixelComponent t 
  => Bitmap t         -- ^ source image
  -> Offset           -- ^ source rectangle offset
  -> Size             -- ^ source rectangle size
  -> Bitmap t         -- ^ target image
  -> Offset           -- ^ target rectangle offset
  -> IO ()
  
copySubImageInto bm1 ofs1@(o1x0,o1y0) siz1@(sx0,sy0) bm2 ofs2@(o2x0,o2y0) = do

  let (bm1xs,bm1ys) = bitmapSize bm1
      pad1    = bitmapRowPadding bm1
      align1  = bitmapRowAlignment  bm1
      nchn1   = bitmapNChannels  bm1
      pixsiz1 = bitmapPixelSizeInBytes bm1
      fptr1   = bitmapPtr        bm1
      xlen1   = bitmapPaddedRowSizeInBytes bm1

  let (bm2xs,bm2ys) = bitmapSize bm2
      pad2    = bitmapRowPadding bm2
      align2  = bitmapRowAlignment  bm2
      nchn2   = bitmapNChannels  bm2
      pixsiz2 = bitmapPixelSizeInBytes bm2
      fptr2   = bitmapPtr        bm2
      xlen2   = bitmapPaddedRowSizeInBytes bm2

  when (nchn1/=nchn2) $ error "bitmap/copySubImageInto: number of channels disagree" 

  -- handle negative offsets
  let (o1x1,sx1,o2x1) = if o1x0 >= 0 then (o1x0, sx0, o2x0) else (0, sx0+o1x0, o2x0-o1x0) 
      (o1y1,sy1,o2y1) = if o1y0 >= 0 then (o1y0, sy0, o2y0) else (0, sy0+o1y0, o2y0-o1y0) 

      (o1x ,sx ,o2x ) = if o2x1 >= 0 then (o1x1, sx1, o2x1) else (o1x1-o2x1, sx1+o2x1, 0) 
      (o1y ,sy ,o2y ) = if o2y1 >= 0 then (o1y1, sy1, o2y1) else (o1y1-o2y1, sy1+o2y1, 0) 
  
  -- size of the rectangle we actually copy
  let xs = minimum [ sx , (bm1xs - o1x) , (bm2xs - o2x) ] 
      ys = minimum [ sy , (bm1ys - o1y) , (bm2ys - o2y) ] 
      pixsiz = pixsiz1

  when (xs>0 && ys>0) $ do
    withForeignPtr fptr1 $ \ptr1' -> withForeignPtr fptr2 $ \ptr2' -> do
      let ptr1 = ptr1' `myPlusPtr` (pixsiz*o1x)
          ptr2 = ptr2' `myPlusPtr` (pixsiz*o2x)
          nbytes = pixsiz*xs
      forM_ (zip (map (*xlen1) [o1y..o1y+ys-1]) 
                 (map (*xlen2) [o2y..o2y+ys-1])) $ \(vo1,vo2) -> do
        let p1 = ptr1 `plusPtr` vo1     
            p2 = ptr2 `plusPtr` vo2     
        c_memcpy p1 p2 nbytes

--------------------------------------------------------------------------------

extractSingleChannel 
  :: PixelComponent t 
  => Bitmap t               -- ^ source image
  -> Maybe Alignment        -- ^ target image row alignment
  -> Int                    -- ^ source channel index
  -> IO (Bitmap t) 
extractSingleChannel bm1 malign j = do
  let nchn = bitmapNChannels bm1
      siz@(w,h) = bitmapSize bm1
  when (j<0 || j>=nchn) $ error "bitmap/extractSingleChannel: invalid channel index"
  bm2 <- newBitmapUninitialized siz 1 malign
  extractChannelInto bm1 j bm2 0
  return bm2
  
    
extractChannels :: PixelComponent t => Bitmap t -> Maybe Alignment -> IO [Bitmap t]
extractChannels bm malign = 
  mapM (extractSingleChannel bm malign) [0..nchn-1] 
    where nchn = bitmapNChannels bm


combineChannels :: forall t. PixelComponent t => [Bitmap t] -> Maybe Alignment -> IO (Bitmap t)
combineChannels [] _ = error "bitmap/combineChannels: no channel data"
combineChannels bms malign = do
  let sizes = map bitmapSize bms
      nchns = map bitmapNChannels bms
      pixsizs = map bitmapPixelSizeInBytes bms 
      sumchn = sum nchns 
      siz@(w,h) = head sizes
      
  when (length (nub sizes) /= 1) $ error "bitmap/combineChannels: incompatible sizes"

  bm2 <- newBitmapUninitialized siz sumchn malign
  let pad2 = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  let loop = concatMap (\bm -> zip (repeat bm) [0..bitmapNChannels bm - 1]) bms

  withForeignPtr fptr2 $ \ptr2 -> do
    forM_ (zip [0..] loop) $ \(i,(bm1,j)) -> do
      let pad1  = bitmapRowPadding bm1
          fptr1 = bitmapPtr bm1    
          nchn1 = bitmapNChannels bm1    
      withForeignPtr fptr1 $ \ptr1 -> 
        c_extract_channel 
          (c_type (undefined::t))
          (ci w) (ci h)
          ptr1 (ci nchn1)  (ci pad1) (ci j)
          ptr2 (ci sumchn) (ci pad2) (ci i)
          
  return bm2

extractChannelInto 
  :: forall t. PixelComponent t 
  => Bitmap t     -- ^ source image
  -> Int          -- ^ source channel index 
  -> Bitmap t     -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
extractChannelInto bm1 ofs1 bm2 ofs2 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w,h) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2  = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  when (siz1 /= siz2)          $ error "bitmap/extractChannelInto: incompatible dimensions"
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/extractChannelInto: invalid source channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/extractChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_extract_channel 
        (c_type (undefined::t))
        (ci w) (ci h)
        ptr1 (ci nchn1) (ci pad1) (ci ofs1)
        ptr2 (ci nchn2) (ci pad2) (ci ofs2)

--------------------------------------------------------------------------------

bilinearResample
  :: PixelComponent t 
  => Bitmap t        -- ^ source image
  -> Size            -- ^ target image size
  -> Maybe Alignment -- ^ target image alignment
  -> IO (Bitmap t)   
bilinearResample bm1 siz2@(w2,h2) malign = do
  let nchn1 = bitmapNChannels bm1
  bm2 <- newBitmapUninitialized siz2 nchn1 malign
  forM_ [0..nchn1-1] $ \ofs ->
    bilinearResampleChannelInto bm1 ofs bm2 ofs
  return bm2


bilinearResampleChannel
  :: PixelComponent t 
  => Bitmap t        -- ^ source image
  -> Int             -- ^ source channel index 
  -> Size            -- ^ target image size
  -> Maybe Alignment -- ^ target image alignment
  -> IO (Bitmap t)   
bilinearResampleChannel bm1 ofs1 siz2@(w2,h2) malign = do
  let nchn1 = bitmapNChannels bm1
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/bilinearResampleChannel: invalid channel index"
  bm2 <- newBitmapUninitialized siz2 1 malign
  bilinearResampleChannelInto bm1 ofs1 bm2 0
  return bm2


bilinearResampleChannelInto 
  :: forall t. PixelComponent t 
  => Bitmap t     -- ^ source image
  -> Int          -- ^ source channel index 
  -> Bitmap t     -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
bilinearResampleChannelInto bm1 ofs1 bm2 ofs2 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2@(w2,h2) = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/bilinearResampleChannelInto: invalid source channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/bilinearResampleChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_bilinear_resample_channel 
        (c_type (undefined::t))
        (ci w1) (ci h1) ptr1 (ci nchn1) (ci pad1) (ci ofs1)
        (ci w2) (ci h2) ptr2 (ci nchn2) (ci pad2) (ci ofs2)

--------------------------------------------------------------------------------

-- | This is equivalent to @componentMap (\c -> c^gamma)@, except that
-- @(^)@ is defined only for integral exponents; but should be faster anyway.
powerlawGammaCorrection 
  :: forall t. PixelComponent t 
  => Float             -- ^ gamma
  -> Bitmap t          -- ^ source bitmap
  -> Maybe Alignment   -- ^ target alignment
  -> IO (Bitmap t)
powerlawGammaCorrection gamma bm1 malign = do
  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1 = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1
  bm2 <- newBitmapUninitialized siz1 nchn1 malign
  let pad2 = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_gamma_correct_all_channels 
        (c_type (undefined::t))
        (realToFrac gamma) 
        (ci w1) (ci h1) (ci nchn1) 
        ptr1 (ci pad1) 
        ptr2 (ci pad2) 
  return bm2
  

powerlawGammaCorrectionChannel
  :: PixelComponent t 
  => Float           -- ^ gamma
  -> Bitmap t        -- ^ source image
  -> Int             -- ^ source channel index 
  -> Maybe Alignment -- ^ target image alignment
  -> IO (Bitmap t)   
powerlawGammaCorrectionChannel gamma bm1 ofs1 malign = do
  let nchn1 = bitmapNChannels bm1
      siz1  = bitmapSize bm1
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/powerlawGammaCorrectionChannel: invalid channel index"
  bm2 <- newBitmapUninitialized siz1 1 malign
  powerlawGammaCorrectionChannelInto gamma bm1 ofs1 bm2 0
  return bm2
  
powerlawGammaCorrectionChannelInto
  :: forall t. PixelComponent t 
  => Float        -- ^ gamma
  -> Bitmap t     -- ^ source image
  -> Int          -- ^ source channel index 
  -> Bitmap t     -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
powerlawGammaCorrectionChannelInto gamma bm1 ofs1 bm2 ofs2 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2@(w2,h2) = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  when (siz1 /= siz2)          $ error "bitmap/powerlawGammaCorrectionChannelInto: incompatible dimensions"
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/powerlawGammaCorrectionChannelInto: invalid source channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/powerlawGammaCorrectionChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      c_gamma_correct_channel 
        (c_type (undefined::t))
        (realToFrac gamma) 
        (ci w1) (ci h1) 
        ptr1 (ci nchn1) (ci pad1) (ci ofs1)
        ptr2 (ci nchn2) (ci pad2) (ci ofs2)

--------------------------------------------------------------------------------

-- | Blends two bitmaps with the given weights; that is, the result is
-- the specified linear combination. If the values are outside the allowed
-- range (this can happen with the Word8, Word16, Word32 types and weights
-- whose sum is bigger than 1, or with a negative weight), then they are
-- clipped. The clipping /does not/ happen with the Float component type.
blendBitmaps
  :: PixelComponent t 
  => Float           -- ^ weight1
  -> Float           -- ^ weight2
  -> Bitmap t        -- ^ source1 image 
  -> Bitmap t        -- ^ source2 image 
  -> Maybe Alignment -- ^ target alignment
  -> IO (Bitmap t)
-- this could be implemented more effectively by a specialized c routine
blendBitmaps weight1 weight2 bm1 bm2 malign = do 
  let nchn1 = bitmapNChannels bm1
      siz1  = bitmapSize bm1
  let nchn2 = bitmapNChannels bm2
      siz2  = bitmapSize bm2
  when (siz1  /= siz2 ) $ error "bitmap/blend: incompatible dimensions"    
  when (nchn1 /= nchn2) $ error "bitmap/blend: incompatible number of channels"
  bm3 <- newBitmapUninitialized siz1 nchn1 malign
  forM [0..nchn1-1] $ \ofs -> 
    blendChannelsInto weight1 weight2 bm1 ofs bm2 ofs bm3 ofs
  return bm3  
  
blendChannels
  :: PixelComponent t 
  => Float        -- ^ weight1
  -> Float        -- ^ weight2
  -> Bitmap t     -- ^ source1 image 
  -> Int          -- ^ source1 channel index  
  -> Bitmap t     -- ^ source2 image 
  -> Int          -- ^ source2 channel index  
  -> Maybe Alignment -- ^ target alignment
  -> IO (Bitmap t)
blendChannels weight1 weight2 bm1 ofs1 bm2 ofs2 malign = do
  let nchn1 = bitmapNChannels bm1
      siz1  = bitmapSize bm1
  let nchn2 = bitmapNChannels bm2
      siz2  = bitmapSize bm2
  when (siz1 /= siz2)          $ error "bitmap/blendChannels: incompatible dimensions"    
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/blendChannels: invalid channel index"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/blendChannels: invalid channel index"
  bm3 <- newBitmapUninitialized siz1 1 malign
  blendChannelsInto weight1 weight2 bm1 ofs1 bm2 ofs2 bm3 0
  return bm3  
  
blendChannelsInto
  :: forall t. PixelComponent t 
  => Float        -- ^ weight1
  -> Float        -- ^ weight2
  -> Bitmap t     -- ^ source1 image 
  -> Int          -- ^ source1 channel index  
  -> Bitmap t     -- ^ source2 image 
  -> Int          -- ^ source2 channel index  
  -> Bitmap t     -- ^ target image
  -> Int          -- ^ target channel index
  -> IO ()
blendChannelsInto weight1 weight2 bm1 ofs1 bm2 ofs2 bm3 ofs3 = do

  let nchn1 = bitmapNChannels bm1
      siz1@(w1,h1) = bitmapSize bm1
      pad1  = bitmapRowPadding bm1
      fptr1 = bitmapPtr bm1

  let nchn2 = bitmapNChannels bm2
      siz2@(w2,h2) = bitmapSize bm2
      pad2  = bitmapRowPadding bm2
      fptr2 = bitmapPtr bm2

  let nchn3 = bitmapNChannels bm3
      siz3@(w3,h3) = bitmapSize bm3
      pad3  = bitmapRowPadding bm3
      fptr3 = bitmapPtr bm3

  when (siz1 /= siz2)          $ error "bitmap/blendChannelInto: incompatible dimensions"
  when (siz2 /= siz3)          $ error "bitmap/blendChannelInto: incompatible dimensions"
  when (ofs1<0 || ofs1>=nchn1) $ error "bitmap/blendChannelInto: invalid source channel index 1"
  when (ofs2<0 || ofs2>=nchn2) $ error "bitmap/blendChannelInto: invalid source channel index 2"
  when (ofs3<0 || ofs3>=nchn3) $ error "bitmap/blendChannelInto: invalid target channel index"
  
  withForeignPtr fptr1 $ \ptr1 -> 
    withForeignPtr fptr2 $ \ptr2 ->     
      withForeignPtr fptr3 $ \ptr3 ->     
        c_linear_combine_channels 
          (c_type (undefined::t))
          (realToFrac weight1) (realToFrac weight2) 
          (ci w1) (ci h1) 
          ptr1 (ci nchn1) (ci pad1) (ci ofs1)
          ptr2 (ci nchn2) (ci pad2) (ci ofs2)
          ptr3 (ci nchn3) (ci pad3) (ci ofs3)
        
--------------------------------------------------------------------------------

-- | The data is copied, not shared. Note that the resulting ByteString is
-- encoded using the host machine's endianness, so it may be not compatible
-- across different architectures!
copyBitmapToByteString :: PixelComponent t => Bitmap t -> IO ByteString
copyBitmapToByteString bm = do
  let n = bitmapSizeInBytes bm
  newfp <- B.mallocByteString n
  withBitmap bm $ \_ _ _ src -> 
    withForeignPtr newfp $ \tgt -> do
      c_memcpy (castPtr src) tgt n
  return $ B.fromForeignPtr (castForeignPtr newfp) 0 n

-- | The data is copied, not shared.
-- Note that we expect the ByteString to be encoded
-- encoded using the host machine's endianness.
copyBitmapFromByteString :: forall t. PixelComponent t => ByteString -> Size -> NChn -> Padding -> IO (Bitmap t)
copyBitmapFromByteString bs siz nchn pad = do
  let (bsfptr0,ofs,len) = B.toForeignPtr bs
      bm = Bitmap 
        { bitmapSize = siz
        , bitmapNChannels = nchn
        , bitmapPtr = undefined 
        , bitmapRowPadding = pad
        , bitmapRowAlignment = 1
        } :: Bitmap t
      n = bitmapSizeInBytes bm
  if n > len-ofs
    then error "copyBitmapFromByteString: ByteString is too short"
    else do
      newfptr <- mallocForeignPtrBytes n
      withForeignPtr bsfptr0 $ \src0 -> do
        let src = src0 `myPlusPtr` ofs
        withForeignPtr newfptr $ \tgt ->
          c_memcpy src tgt n
      return $ bm { bitmapPtr = castForeignPtr newfptr } 
  
--------------------------------------------------------------------------------

-- no multiplication
{-# SPECIALIZE advancePtr1 :: Ptr Word8 -> Ptr Word8 #-}
{-# SPECIALIZE advancePtr1 :: Ptr Float -> Ptr Float #-}
advancePtr1 :: forall a. Storable a => Ptr a -> Ptr a
advancePtr1 p = p `plusPtr` (sizeOf (undefined::a))

-- restricted type
{-# SPECIALIZE myPlusPtr :: Ptr Word8 -> Int -> Ptr Word8 #-}
{-# SPECIALIZE myPlusPtr :: Ptr Float -> Int -> Ptr Float #-}
myPlusPtr :: Ptr a -> Int -> Ptr a
myPlusPtr = plusPtr

ci :: Int -> CInt
ci = fromIntegral

-- @c_memset target count fill@.
-- Note that we use /nonstandard/ argument order!
foreign import ccall unsafe "bm.h c_memset" 
  c_memset :: Ptr Word8 -> Int -> Word8 -> IO ()

-- @c_memcpy from to cnt@.
-- Note that we use /nonstandard/ argument order!
foreign import ccall unsafe "bm.h c_memcpy" 
  c_memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

--------------------

{-
void c_extract_channel(
  ( int k_type
  , int width, int height 
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  );
-}

-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_extract_channel"
  c_extract_channel 
    :: CInt              -- ^ component type
    -> CInt -> CInt      -- ^ width, height
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ target, nchn, pad, offset
    -> IO ()

--------------------
 
{-
void c_bilinear_resample_channel
  ( int k_type
  , int width1, int height1, void *p1, int nchn1, int pad1, int ofs1 
  , int width2, int height2, void *p2, int nchn2, int pad2, int ofs2 
  );       
-}

-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_bilinear_resample_channel"
  c_bilinear_resample_channel 
    :: CInt                                           -- ^ component type
    -> CInt -> CInt -> Ptr a -> CInt -> CInt -> CInt  -- ^ width, height, source, nchn, pad, offset
    -> CInt -> CInt -> Ptr a -> CInt -> CInt -> CInt  -- ^ width, height, target, nchn, pad, offset
    -> IO ()

--------------------

{-
void c_gamma_correct_channel
  ( int k_type
  , float gamma
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  );
  
void c_gamma_correct_all_channels
  ( int k_type
  , float gamma
  , int width, int height, int nchn
  , void *p1, int pad1 
  , void *p2, int pad2 
  );
-}
 
-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_gamma_correct_channel"
  c_gamma_correct_channel 
    :: CInt              -- ^ component type
    -> CFloat            -- ^ gamma
    -> CInt -> CInt      -- ^ width, height
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ target, nchn, pad, offset
    -> IO ()

foreign import ccall unsafe "bm.h c_gamma_correct_all_channels"
  c_gamma_correct_all_channels 
    :: CInt                 -- ^ component type
    -> CFloat               -- ^ gamma
    -> CInt -> CInt -> CInt -- ^ width, height, nchn
    -> Ptr a -> CInt        -- ^ source, pad
    -> Ptr a -> CInt        -- ^ target, pad
    -> IO ()

--------------------
    
{-
void c_linear_combine_channels
  ( int k_type
  , float weight1, float weight2
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  , void *p3, int nchn3, int pad3, int ofs3 
  );    
-}

-- offset is measured in components, not bytes!
foreign import ccall unsafe "bm.h c_linear_combine_channels"
  c_linear_combine_channels 
    :: CInt              -- ^ component type
    -> CFloat -> CFloat  -- ^ weight1, weight2 
    -> CInt -> CInt      -- ^ width, height
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source1, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ source2, nchn, pad, offset
    -> Ptr a -> CInt -> CInt -> CInt  -- ^ target,  nchn, pad, offset
    -> IO ()

--------------------------------------------------------------------------------

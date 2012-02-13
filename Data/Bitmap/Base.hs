
--------------------------------------------------------------------------------
-- Module      : Data.Bitmap.Base
-- Version     : 0.0.1
-- License     : BSD3
-- Copyright   : (c) 2009 Balazs Komuves
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : requires FFI, CPP and ScopedTypeVariables
-- Tested with : GHC 6.10.1
--------------------------------------------------------------------------------

--
-- Terminology: 
-- Pixels are made out of one or more \"components\". These components 
-- are also referred as \"channels\"; for example a color image could be made out 
-- of three channels, the red, green and blue one. The components can be unsigned
-- bytes, words, dwords, or floats. The pixels are stored in horizontal order,
-- and the channels are interleaved: That is, the structure of an RGB image is 
-- @R0 G0 B0 R1 G1 B1 ...@. 
-- Most of the library is indifferent to the meaning of different channels.
-- The fixed point types (byte, word and dword) are interpreted as numbers
-- between 0 and 1; the floating point type can exceed the [0..1] interval.
--
-- \"Padding\" refers to unused bytes at the end of each row. This is sometimes
-- necessary because other software components want the rows aligned to machine
-- word boundary, for example. 
--

{-# LANGUAGE CPP, ForeignFunctionInterface, ScopedTypeVariables #-}
module Data.Bitmap.Base 
  ( 
    -- * Types
    PixelComponent
  , PixelComponentType(..)
  , pixelComponentSize
  , pixelComponentType
    --
  , Word8
  , Word16
  , Word32
    --
  , Size
  , Offset
  , NChn
  , Alignment
  , Padding
    --
  , Bitmap
  , bitmapSize 
  , bitmapNChannels
  , bitmapRowPadding 
  , bitmapRowAlignment 
  , bitmapAspect
    --
  , bitmapComponentSizeInBytes
  , bitmapPixelSizeInBytes
  , bitmapPaddedRowSizeInBytes
  , bitmapUnpaddedRowSizeInBytes
  , bitmapSizeInBytes
  ) 
  where

--------------------------------------------------------------------------------

import Data.Word
import Data.Bitmap.Internal

--------------------------------------------------------------------------------

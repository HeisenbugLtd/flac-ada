------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

------------------------------------------------------------------------------
--  FLAC/Ada
--
--  Types
--
--  Defines basic types used in decoding/encoding FLAC files.
------------------------------------------------------------------------------

with Ada.Streams;
with Interfaces;
with System;

private package Flac.Types with
  Pure       => True,
  SPARK_Mode => On
is

   use type Ada.Streams.Stream_Element_Offset;
   use type Interfaces.Unsigned_32;
   use type System.Bit_Order;

   --  FLAC uses (mostly) big endian format, so we need to juggle with bits
   --  occasionally.  Create a compile time constant for swapping code.
   Needs_Swap : constant Boolean :=
     System.Default_Bit_Order = System.Low_Order_First;

   --  Basic types.

   type Length_16 is new Interfaces.Unsigned_16
     with
       Annotate => (GNATprove, No_Wrap_Around);

   function BE_Swap (Arg : in Length_16) return Length_16;

   subtype Block_Size is Length_16 range 16 .. Length_16'Last;

   type Length_24 is new --  24 bit
     Interfaces.Unsigned_32 range 0 .. 2 ** 24 - 1
       with
         Annotate => (GNATprove, No_Wrap_Around);

   function BE_Swap (Arg : in Length_24) return Length_24
     with
       Inline => True;

   type Length_36 is mod 2 ** 36 with --  36 bit
     Annotate => (GNATprove, No_Wrap_Around);

   type Count_64 is mod 2 ** 64
     with
       Annotate => (GNATprove, No_Wrap_Around);

   type Offset_64 is mod 2 ** 64
     with
       Annotate => (GNATprove, No_Wrap_Around);

   type Length_20 is mod 2 ** 20
     with
       Annotate => (GNATprove, No_Wrap_Around);

   subtype Sample_Rate is Length_20 range 1 .. 65535 * 10; --  20 bit

   type Channel_Count is range 1 .. 8;
   --  3 bit
   --
   --  Biased representation, 0 .. 7 maps to logical 1 .. 8

   type Bits_Per_Sample is range 1 .. 32;
   --  5 bits
   --
   --  Careful, this is starting at 1 to avoid a wrong biased representation
   --  (0 .. 31 maps to 1 .. 32), but officially the minimum number of bits
   --  per sample is 4.

   --  BLOCK_TYPE
   --
   --      0 : STREAMINFO
   --      1 : PADDING
   --      2 : APPLICATION
   --      3 : SEEKTABLE
   --      4 : VORBIS_COMMENT
   --      5 : CUESHEET
   --      6 : PICTURE
   --      7-126 : reserved
   --      127 : invalid, to avoid confusion with a frame sync code
   type Block_Type is (Stream_Info,
                       Padding,
                       Application,
                       Seek_Table,
                       Vorbis_Comment,
                       Cue_Sheet,
                       Picture,
                       Reserved_7,
                       --  And everything inbetween.
                       Reserved_126,
                       Invalid)
     with
       Size        => 7,
       Object_Size => 8;
   for Block_Type use (Stream_Info    => 0,
                       Padding        => 1,
                       Application    => 2,
                       Seek_Table     => 3,
                       Vorbis_Comment => 4,
                       Cue_Sheet      => 5,
                       Picture        => 6,
                       Reserved_7     => 7,
                       Reserved_126   => 126,
                       Invalid        => 127);

   subtype MD5_Sum is Ada.Streams.Stream_Element_Array (0 .. 128 / 8 - 1);

private

   function BE_Swap (Arg : in Length_16) return Length_16 is
     (if Needs_Swap
      then Shift_Left (Arg, 8) or Shift_Right (Arg, 8)
      else Arg);

   function BE_Swap (Arg : in Length_24) return Length_24 is
     (if Needs_Swap
        then (Shift_Left (Arg and 16#0000FF#, 16) or
              (Arg and 16#00FF00#)                or
                Shift_Right (Arg and 16#FF0000#, 16))
      else Arg);

end Flac.Types;

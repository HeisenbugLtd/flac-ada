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

package Flac.Types with
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

   subtype Block_Size is Length_16 range 16 .. Length_16'Last;

   type Length_24 is new --  24 bit
     Interfaces.Unsigned_32 range 0 .. 2 ** 24 - 1
       with
         Annotate => (GNATprove, No_Wrap_Around);

   type Sample_Count is mod 2 ** 36 --  36 bit
     with
       Annotate => (GNATprove, No_Wrap_Around);

   type Frame_Count is mod 2 ** 31 --  36 bit
     with
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

   subtype Sample_Rate is Length_20 range 1 .. 65535 * 10;

   type Channel_Count is range 1 .. 8;
   --  3 bit

   type Bits_Per_Sample is range 4 .. 32;
   --  5 bits

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

end Flac.Types;

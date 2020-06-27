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
--  Headers
--
--  Defines various file/frame headers of FLAC files.
------------------------------------------------------------------------------

with Flac.Types;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Interfaces;
with System;

private package Flac.Headers with
  Pure       => True,
  SPARK_Mode => On
is

   use type Ada.Streams.Stream_Element_Offset;

   --  Stream header definitions.
   --  STREAM
   --  <32> 	"fLaC", the FLAC stream marker in ASCII, meaning byte 0 of the stream is 0x66, followed by 0x4C 0x61 0x43
   --  METADATA_BLOCK 	This is the mandatory STREAMINFO metadata block that has the basic properties of the stream
   --  METADATA_BLOCK* 	Zero or more metadata blocks
   --  FRAME+ 	One or more audio frames    

   subtype Four_CC is Ada.Streams.Stream_Element_Array (1 .. 4);

   Stream : constant Four_CC := (Character'Pos ('f'),
                                 Character'Pos ('L'),
                                 Character'Pos ('a'),
                                 Character'Pos ('C'));

   type Padding is new Ada.Streams.Stream_Element_Array;

   type Application is
      record
         Id : Ada.Streams.Stream_Element_Array (1 .. 4);
         --  Unspecified (application specific)
      end record;
   for Application use
      record
         Id at 0 range 0 .. 31;
      end record;

   type Seek_Point is
      record
         Sample_Number : Types.Count_64;
         Sample_Offset : Types.Offset_64; --  Careful, these are modular types!
         Sample_Count  : Types.Length_16;
      end record
     with
       Size      => 144,
       Bit_Order => System.Low_Order_First;
   for Seek_Point use
      record
         Sample_Number at  0 range 0 .. 63;
         Sample_Offset at  8 range 0 .. 63;
         Sample_Count  at 16 range 0 .. 15;
      end record;

   type Seek_Table is array (Natural range <>) of Seek_Point;

end Flac.Headers;

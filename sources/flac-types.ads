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

with System.Storage_Elements;

private package Flac.Types with
  Pure       => True,
  SPARK_Mode => On
is

   package SSE renames System.Storage_Elements;
   use type SSE.Storage_Offset;

   --  Basic types.

   type Length_16 is range 0 .. 2 ** 16 - 1; --  16 bit

   subtype Block_Size is Length_16
     with
       Static_Predicate => Block_Size > 15;

   type Length_24 is range 0 .. 2 ** 24 - 1; --  24 bit

   type Length_36 is range 0 .. 2 ** 36 - 1; --  36 bit

   type Count_64 is mod 2 ** 64
     with
       Annotate => (GNATprove, No_Wrap_Around);

   type Offset_64 is mod 2 ** 64
     with
       Annotate => (GNATprove, No_Wrap_Around);

   type Sample_Rate is range 1 .. 65535 * 10; --  20 bit

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
                       Reserved,
                       Invalid);
   for Block_Type use (Stream_Info => 0,
                       Padding     => 1,
                       Application => 2,
                       Seek_Table  => 3,
                       Vorbis_Comment => 4,
                       Cue_Sheet      => 5,
                       Picture        => 6,
                       Reserved       => 7,
                       Invalid        => 127);

   subtype MD5_Sum is SSE.Storage_Array (0 .. 128 / 8 - 1);

end Flac.Types;

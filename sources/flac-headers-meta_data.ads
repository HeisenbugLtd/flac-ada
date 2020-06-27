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
--  The meta data block.
------------------------------------------------------------------------------

package FLAC.Headers.Meta_Data with
  Pure       => True,
  SPARK_Mode => On
is

   --  METADATA_BLOCK_HEADER
   --  <1> 	Last-metadata-block flag: '1' if this block is the last metadata block before the audio blocks, '0' otherwise.
   --  <7> 	BLOCK_TYPE
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
   --  
   --  <24> 	Length (in bytes) of metadata to follow (does not include the size of the METADATA_BLOCK_HEADER)
   type T is
       record
         Last       : Boolean;
         Block_Type : Types.Block_Type;
         Length     : Types.Length_24;
      end record
     with
       Size        => 32,
       Object_Size => 32,
       Bit_Order   => System.Low_Order_First;
   for T use
      record
         Last       at 0 range 7 .. 7;
         Block_Type at 0 range 0 .. 6;
         Length     at 0 range 8 .. 31;
      end record;

   subtype Raw_T is Ada.Streams.Stream_Element_Array (1 .. 4)
     with
       Object_Size => 32;

   procedure Convert (Source           : in     Raw_T;
                      Target           :    out T;
                      Conversion_Error :    out Boolean)
     with
       Depends => (Target           => Source,
                   Conversion_Error => Source);

end FLAC.Headers.Meta_Data;

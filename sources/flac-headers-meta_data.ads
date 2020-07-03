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

with Ada.Streams.Stream_IO;
with SPARK_Stream_IO;

package FLAC.Headers.Meta_Data with
  Preelaborate => True,
  SPARK_Mode   => On
is

   --  METADATA_BLOCK_HEADER
   --  <1> Last-metadata-block flag: '1' if this block is the last metadata
   --      block before the audio blocks, '0' otherwise.
   --  <7> BLOCK_TYPE
   --      0 : STREAMINFO
   --      1 : PADDING
   --      2 : APPLICATION
   --      3 : SEEKTABLE
   --      4 : VORBIS_COMMENT
   --      5 : CUESHEET
   --      6 : PICTURE
   --      7-126 : reserved
   --      127 : invalid, to avoid confusion with a frame sync code
   --  <24> Length (in bytes) of metadata to follow (does not include the size
   --       of the METADATA_BLOCK_HEADER)
   type T is
       record
         Last       : Boolean;
         Block_Type : Types.Block_Type;
         Length     : Types.Length_24;
      end record;

   Meta_Data_Length : constant := 32 / Ada.Streams.Stream_Element'Size;

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File  : in     Ada.Streams.Stream_IO.File_Type;
                   Item  :    out T;
                   Error :    out Boolean)
     with
       Relaxed_Initialization => Item,
       Pre     => SPARK_Stream_IO.Is_Open (File => File),
       Post    => (if not Error then Item'Initialized),
       Depends => ((Error, Item) => File);

end FLAC.Headers.Meta_Data;

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
--  The STREAMINFO block.
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;
with SPARK_Stream_IO;

package FLAC.Headers.Stream_Info with
  Preelaborate => True,
  SPARK_Mode   => On
is

   --  METADATA_BLOCK_STREAMINFO
   --  <16> The minimum block size (in samples) used in the stream.
   --  <16> The maximum block size (in samples) used in the stream. (Minimum
   --       blocksize == maximum blocksize) implies a fixed-blocksize stream.
   --  <24> The minimum frame size (in bytes) used in the stream. May be 0 to
   --       imply the value is not known.
   --  <24> The maximum frame size (in bytes) used in the stream. May be 0 to
   --       imply the value is not known.
   --  <20> Sample rate in Hz. Though 20 bits are available, the maximum sample
   --       rate is limited by the structure of frame headers to 655350 Hz.
   --       Also, a value of 0 is invalid.
   --  <3> 	(number of channels)-1. FLAC supports from 1 to 8 channels
   --  <5> 	(bits per sample)-1. FLAC supports from 4 to 32 bits per sample.
   --       Currently the reference encoder and decoders only support up to 24
   --       bits per sample.
   --  <36> Total samples in stream. 'Samples' means inter-channel sample, i.e.
   --       one second of 44.1 kHz audio will have 44100 samples regardless of
   --       the number of channels. A value of zero here means the number of
   --       total samples is unknown.
   --  <128> MD5 signature of the unencoded audio data. This allows the decoder
   --        to determine if an error exists in the audio data even when the
   --        error does not result in an invalid bitstream.
   --  NOTES
   --    FLAC specifies a minimum block size of 16 and a maximum block size of
   --    65535, meaning the bit patterns corresponding to the numbers 0-15 in
   --    the minimum blocksize and maximum blocksize fields are invalid.

   type T is
      record
         Min_Block_Size  : Types.Block_Size; --  samples
         Max_Block_Size  : Types.Block_Size; --  samples
         Min_Frame_Size  : Types.Length_24; --  bytes
         Max_Frame_Size  : Types.Length_24; --  bytes
         Sample_Rate     : Types.Sample_Rate;
         Num_Channels    : Types.Channel_Count;
         Bits_Per_Sample : Types.Bits_Per_Sample;
         Total_Samples   : Types.Sample_Count;
         MD5_Signature   : Types.MD5_Sum;
      end record;

   Stream_Info_Length : constant := 272 / Ada.Streams.Stream_Element'Size;

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

end FLAC.Headers.Stream_Info;

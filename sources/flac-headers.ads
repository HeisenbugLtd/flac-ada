------------------------------------------------------------------------------
--  FLAC/Ada
--
--  Headers
--
--  Defines various file/frame headers of FLAC files.
------------------------------------------------------------------------------

with Flac.Types;
with Ada.Streams;

private package Flac.Headers with
  Pure       => True,
  SPARK_Mode => On
is

   --  Stream header definitions.
   --  STREAM
   --  <32> 	"fLaC", the FLAC stream marker in ASCII, meaning byte 0 of the stream is 0x66, followed by 0x4C 0x61 0x43
   --  METADATA_BLOCK 	This is the mandatory STREAMINFO metadata block that has the basic properties of the stream
   --  METADATA_BLOCK* 	Zero or more metadata blocks
   --  FRAME+ 	One or more audio frames    

   Stream : constant Ada.Streams.Stream_Element_Array := (Character'Pos ('f'),
                                                          Character'Pos ('L'),
                                                          Character'Pos ('a'),
                                                          Character'Pos ('C'));

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
   type Meta_Data_Block is
       record
         Last       : Boolean;
         Block_Type : Types.Block_Type;
         Length     : Types.Length_24;
      end record
     with
       Size => 32;
   for Meta_Data_Block use
      record
         Last       at 0 range 0 ..  0;
         Block_Type at 0 range 1 ..  7;
         Length     at 1 range 0 .. 23;
      end record;

   --  METADATA_BLOCK_STREAMINFO
   --  <16> 	The minimum block size (in samples) used in the stream.
   --  <16> 	The maximum block size (in samples) used in the stream. (Minimum blocksize == maximum blocksize) implies a fixed-blocksize stream.
   --  <24> 	The minimum frame size (in bytes) used in the stream. May be 0 to imply the value is not known.
   --  <24> 	The maximum frame size (in bytes) used in the stream. May be 0 to imply the value is not known.
   --  <20> 	Sample rate in Hz. Though 20 bits are available, the maximum sample rate is limited by the structure of frame headers to 655350Hz. Also, a value of 0 is invalid.
   --  <3> 	(number of channels)-1. FLAC supports from 1 to 8 channels
   --  <5> 	(bits per sample)-1. FLAC supports from 4 to 32 bits per sample. Currently the reference encoder and decoders only support up to 24 bits per sample.
   --  <36> 	Total samples in stream. 'Samples' means inter-channel sample, i.e. one second of 44.1Khz audio will have 44100 samples regardless of the number of channels. A value of zero here means the number of total samples is unknown.
   --  <128> 	MD5 signature of the unencoded audio data. This allows the decoder to determine if an error exists in the audio data even when the error does not result in an invalid bitstream.
   --  	NOTES
   --  
   --      FLAC specifies a minimum block size of 16 and a maximum block size of 65535, meaning the bit patterns corresponding to the numbers 0-15 in the minimum blocksize and maximum blocksize fields are invalid.

   type Stream_Info is
      record
         Min_Block_Size  : Types.Block_Size; --  samples
         Max_Block_Size  : Types.Block_Size; --  samples!
         Min_Frame_Size  : Types.Length_24; --  bytes
         Max_Frame_Size  : Types.Length_24; --  bytes
         Sample_Rate     : Types.Sample_Rate;
         Num_Channels    : Types.Channel_Count; --  0 .. 7 => 1 .. 8
         Bits_Per_Sample : Types.Bits_Per_Sample; --  3 .. 31 => 4 .. 32
         Total_Samples   : Types.Length_36;
         MD5_Signature   : Types.MD5_Sum;
      end record
     with
       Size => 272;
   pragma Warnings (Off, "component clause forces biased representation for ""Bits_Per_Sample""");
   pragma Warnings (Off, "component clause forces biased representation for ""Num_Channels""");
   for Stream_Info use
      record
         Min_Block_Size  at  0 range  0 ..  15;
         Max_Block_Size  at  2 range  0 ..  15;
         Min_Frame_Size  at  4 range  0 ..  23;
         Max_Frame_Size  at  7 range  0 ..  23;
         Sample_Rate     at 10 range  0 ..  19;
         Num_Channels    at 10 range 20 ..  22;
         Bits_Per_Sample at 10 range 23 ..  27;
         Total_Samples   at 10 range 28 ..  63;
         MD5_Signature   at 18 range  0 .. 127;
      end record;
   pragma Warnings (On, "component clause forces biased representation for ""Num_Channels""");
   pragma Warnings (On, "component clause forces biased representation for ""Bits_Per_Sample""");

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
       Size => 144;
   for Seek_Point use
      record
         Sample_Number at  0 range 0 .. 63;
         Sample_Offset at  8 range 0 .. 63;
         Sample_Count  at 16 range 0 .. 15;
      end record;

   type Seek_Table is array (Natural range <>) of Seek_Point;

end Flac.Headers;

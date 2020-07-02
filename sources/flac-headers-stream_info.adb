------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with System;

package body FLAC.Headers.Stream_Info with
  Pure       => True,
  SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File  : in     Ada.Streams.Stream_IO.File_Type;
                   Item  :    out T;
                   Error :    out Boolean)
   is
      Raw_Data : Ada.Streams.Stream_Element_Array (1 .. Stream_Info_Length);

      Min_Block_Raw       : Types.Block_Size'Base;
      Max_Block_Raw       : Types.Block_Size'Base;
      Sample_Rate_Raw     : Types.Sample_Rate'Base;
      Bits_Per_Sample_Raw : Types.Bits_Per_Sample'Base;
      use type Ada.Streams.Stream_Element;
      use type Types.Bits_Per_Sample'Base;
      use type Types.Block_Size'Base;
      use type Types.Length_24'Base;
      use type Types.Sample_Count'Base;
      use type Types.Sample_Rate'Base;
   begin
      SPARK_Stream_IO.Read (File  => File,
                            Item  => Raw_Data,
                            Error => Error);

      if Error then
         return;
      end if;

      --  <16> The minimum block size (in samples) used in the stream.
      Min_Block_Raw :=
        Types.Block_Size'Base (Raw_Data (1)) * 2 ** 8 +
        Types.Block_Size'Base (Raw_Data (2));

      --  <16> The maximum block size (in samples) used in the stream. (Minimum blocksize == maximum blocksize) implies a fixed-blocksize stream.
      Max_Block_Raw :=
        Types.Block_Size'Base (Raw_Data (3)) * 2 ** 8 +
        Types.Block_Size'Base (Raw_Data (4));

      Error :=
        Min_Block_Raw not in Types.Block_Size or
        Max_Block_Raw not in Types.Block_Size;

      if Error then
         return;
      end if;

      Item.Min_Block_Size := Min_Block_Raw;
      Item.Max_Block_Size := Max_Block_Raw;

      --  <24> The minimum frame size (in bytes) used in the stream. May be 0 to imply the value is not known.
      Item.Min_Frame_Size :=
        Types.Length_24 (Raw_Data (5)) * 2 ** 16 +
        Types.Length_24 (Raw_Data (6)) * 2 ** 8 +
        Types.Length_24 (Raw_Data (7));

      --  <24> The maximum frame size (in bytes) used in the stream. May be 0 to imply the value is not known.
      Item.Max_Frame_Size :=
        Types.Length_24 (Raw_Data (8)) * 2 ** 16 +
        Types.Length_24 (Raw_Data (9)) * 2 ** 8 +
        Types.Length_24 (Raw_Data (10));

      --  <20> Sample rate in Hz. Though 20 bits are available, the maximum sample rate is limited by the structure of frame headers to 655350Hz. Also, a value of 0 is invalid.
      Sample_Rate_Raw :=
        Types.Sample_Rate'Base (Raw_Data (11)) * 2 ** 12 + --  bits 19 .. 12
        Types.Sample_Rate'Base (Raw_Data (12)) * 2 ** 4 +  --  bits 11 .. 4
        Types.Sample_Rate'Base (Raw_Data (13) and 2#1111_0000#) / 2 ** 4; -- bits 3 .. 0

      Error := Sample_Rate_Raw not in Types.Sample_Rate;

      if Error then
         return;
      end if;

      Item.Sample_Rate := Sample_Rate_Raw;

      --  <3> 	(number of channels)-1. FLAC supports from 1 to 8 channels
      Item.Num_Channels := Types.Channel_Count ((Raw_Data (13) and 2#0000_1110#) / 2 + 1);

      --  <5> 	(bits per sample)-1. FLAC supports from 4 to 32 bits per sample. Currently the reference encoder and decoders only support up to 24 bits per sample.
      Bits_Per_Sample_Raw :=
        Types.Bits_Per_Sample'Base ((Raw_Data (13) and 2#0000_0001#) * 2 ** 4) +
        Types.Bits_Per_Sample'Base ((Raw_Data (14) and 2#1111_0000#) / 2 ** 4) + 1;

      Error := Bits_Per_Sample_Raw not in Types.Bits_Per_Sample;

      if Error then
         return;
      end if;

      Item.Bits_Per_Sample := Bits_Per_Sample_Raw;

      --  <36> Total samples in stream. 'Samples' means inter-channel sample, i.e. one second of 44.1Khz audio will have 44100 samples regardless of the number of channels. A value of zero here means the number of total samples is unknown.
      Item.Total_Samples :=
        Types.Sample_Count (Raw_Data (14) and 2#0000_1111#) * 2 ** 32 +
        Types.Sample_Count (Raw_Data (15))                  * 2 ** 24 +
        Types.Sample_Count (Raw_Data (16))                  * 2 ** 16 +
        Types.Sample_Count (Raw_Data (17))                  * 2 ** 8 +
        Types.Sample_Count (Raw_Data (18));

      --  <128> MD5 signature of the unencoded audio data. This allows the decoder to determine if an error exists in the audio data even when the error does not result in an invalid bitstream.
      Item.MD5_Signature := Raw_Data (19 .. Raw_Data'Last);
   end Read;

end FLAC.Headers.Stream_Info;

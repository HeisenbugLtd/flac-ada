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
--  Frames
--
--  Defines various frame headers of FLAC files.
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;
with Flac.CRC;
with Flac.Headers.Stream_Info;
with Flac.Types;
with SPARK_Stream_IO;

package Flac.Frames with
  SPARK_Mode => On
is

   --  <14> Sync code '11_1111_1111_1110'
   type Sync_Code is mod 2 ** 14;

   --  <1> Blocking strategy:
   --      0 : fixed-blocksize stream; frame header encodes the frame number
   --      1 : variable-blocksize stream; frame header encodes the sample number
   type Blocking_Strategy is (Fixed, Variable);
   for Blocking_Strategy use (Fixed    => 0,
                              Variable => 1);

   --  <4> Block size in inter-channel samples:
   --      0000      : reserved
   --      0001      : 192 samples
   --      0010-0101 : 576 * (2^(n-2)) samples, i.e. 576/1152/2304/4608
   --      0110      : get 8 bit (blocksize-1) from end of header
   --      0111      : get 16 bit (blocksize-1) from end of header
   --      1000-1111 : 256 * (2^(n-8)) samples, i.e. 256/512/1024/2048/4096/
   --                  8192/16384/32768
   type Block_Size is (Reserved,
                       Samples_192,
                       Samples_576,
                       Samples_1152,
                       Samples_2304,
                       Samples_4608,
                       Samples_8_Bit_From_EOH,
                       Samples_16_Bit_From_EOH,
                       Samples_256,
                       Samples_512,
                       Samples_1024,
                       Samples_2048,
                       Samples_4096,
                       Samples_8192,
                       Samples_16384,
                       Samples_32768);
   for Block_Size use (Reserved                => 2#0000#,
                       Samples_192             => 2#0001#,
                       Samples_576             => 2#0010#,
                       Samples_1152            => 2#0011#,
                       Samples_2304            => 2#0100#,
                       Samples_4608            => 2#0101#,
                       Samples_8_Bit_From_EOH  => 2#0110#,
                       Samples_16_Bit_From_EOH => 2#0111#,
                       Samples_256             => 2#1000#,
                       Samples_512             => 2#1001#,
                       Samples_1024            => 2#1010#,
                       Samples_2048            => 2#1011#,
                       Samples_4096            => 2#1100#,
                       Samples_8192            => 2#1101#,
                       Samples_16384           => 2#1110#,
                       Samples_32768           => 2#1111#);
                       
   --  <4> Sample rate:
   --      0000 : get from STREAMINFO metadata block
   --      0001 : 88.2kHz
   --      0010 : 176.4kHz
   --      0011 : 192kHz
   --      0100 : 8kHz
   --      0101 : 16kHz
   --      0110 : 22.05kHz
   --      0111 : 24kHz
   --      1000 : 32kHz
   --      1001 : 44.1kHz
   --      1010 : 48kHz
   --      1011 : 96kHz
   --      1100 : get 8 bit sample rate (in kHz) from end of header
   --      1101 : get 16 bit sample rate (in Hz) from end of header
   --      1110 : get 16 bit sample rate (in tens of Hz) from end of header
   --      1111 : invalid, to prevent sync-fooling string of 1s
   type Sample_Rate is (Get_From_Stream_Info,
                        kHz_88_2,
                        kHz_176_4,
                        kHz_192,
                        kHz_8,
                        kHz_16,
                        kHz_22_05,
                        kHz_24,
                        kHz_32,
                        kHz_44_1,
                        kHz_48,
                        kHz_96,
                        Get_8_Bit_kHz_From_EOH,
                        Get_16_Bit_kHz_From_EOH,
                        Get_16_Bit_Ten_kHz_From_EOH,
                        Invalid);
   for Sample_Rate use (Get_From_Stream_Info        => 2#0000#,
                        kHz_88_2                    => 2#0001#,
                        kHz_176_4                   => 2#0010#,
                        kHz_192                     => 2#0011#,
                        kHz_8                       => 2#0100#,
                        kHz_16                      => 2#0101#,
                        kHz_22_05                   => 2#0110#,
                        kHz_24                      => 2#0111#,
                        kHz_32                      => 2#1000#,
                        kHz_44_1                    => 2#1001#,
                        kHz_48                      => 2#1010#,
                        kHz_96                      => 2#1011#,
                        Get_8_Bit_kHz_From_EOH      => 2#1100#,
                        Get_16_Bit_kHz_From_EOH     => 2#1101#,
                        Get_16_Bit_Ten_kHz_From_EOH => 2#1110#,
                        Invalid                     => 2#1111#);

   --  <4> Channel assignment
   --      0000-0111 : (number of independent channels)-1. Where defined, the
   --                  channel order follows SMPTE/ITU-R recommendations. The
   --                  assignments are as follows:
   --                  1 channel : mono
   --                  2 channels: left, right
   --                  3 channels: left, right, center
   --                  4 channels: front left, front right, back left,
   --                              back right
   --                  5 channels: front left, front right, front center,
   --                              back/surround left, back/surround right
   --                  6 channels: front left, front right, front center, LFE,
   --                              back/surround left, back/surround right
   --                  7 channels: front left, front right, front center, LFE,
   --                              back center, side left, side right
   --                  8 channels: front left, front right, front center, LFE,
   --                              back left, back right, side left, side right
   --      1000      : left/side stereo: channel 0 is the left channel, channel
   --                                    1 is the side (difference) channel
   --      1001      : right/side stereo: channel 0 is the side (difference)
   --                                     channel, channel 1 is the right
   --                                     channel
   --      1010      : mid/side stereo: channel 0 is the mid (average) channel,
   --                                   channel 1 is the side (difference)
   --                                   channel
   --      1011-1111 : reserved
   type Channel_Assignment is (Mono,
                               L_R,
                               L_R_C,
                               FL_FR_BL_BR,
                               FL_FR_FC_BL_BR,
                               FL_FR_FC_LFE_BL_BR,
                               FL_FR_FC_LFE_BC_SL_SR,
                               FL_FR_FC_LFE_BL_BR_SL_SR,
                               Left_Side,
                               Right_Side,
                               Mid_Side,
                               Reserved_1011,
                               Reserved_1100,
                               Reserved_1101,
                               Reserved_1110,
                               Reserved_1111);
   for Channel_Assignment use (Mono                      => 2#0000#,
                               L_R                       => 2#0001#,
                               L_R_C                     => 2#0010#,
                               FL_FR_BL_BR               => 2#0011#,
                               FL_FR_FC_BL_BR            => 2#0100#,
                               FL_FR_FC_LFE_BL_BR        => 2#0101#,
                               FL_FR_FC_LFE_BC_SL_SR     => 2#0110#,
                               FL_FR_FC_LFE_BL_BR_SL_SR  => 2#0111#,
                               Left_Side                 => 2#1000#,
                               Right_Side                => 2#1001#,
                               Mid_Side                  => 2#1010#,
                               Reserved_1011             => 2#1011#,
                               Reserved_1100             => 2#1100#,
                               Reserved_1101             => 2#1101#,
                               Reserved_1110             => 2#1110#,
                               Reserved_1111             => 2#1111#);
  
   --  <3> Sample size in bits:
   --      000 : get from STREAMINFO metadata block
   --      001 : 8 bits per sample
   --      010 : 12 bits per sample
   --      011 : reserved
   --      100 : 16 bits per sample
   --      101 : 20 bits per sample
   --      110 : 24 bits per sample
   --      111 : reserved
   type Sample_Size is (Get_From_Stream_Info,
                        Bits_8,
                        Bits_12,
                        Reserved_011,
                        Bits_16,
                        Bits_20,
                        Bits_24,
                        Reserved_111);
   for Sample_Size use (Get_From_Stream_Info => 2#000#,
                        Bits_8               => 2#001#,
                        Bits_12              => 2#010#,
                        Reserved_011         => 2#011#,
                        Bits_16              => 2#100#,
                        Bits_20              => 2#101#,
                        Bits_24              => 2#110#,
                        Reserved_111         => 2#111#);

   --  FRAME_HEADER
   --  <14> Sync code '11_1111_1111_1110'
   --  <1> Reserved: [1]
   --      0 : mandatory value
   --      1 : reserved for future use
   --  <1> Blocking strategy:
   --      0 : fixed-blocksize stream; frame header encodes the frame number
   --      1 : variable-blocksize stream; frame header encodes the sample number
   --  <4> Block size in inter-channel samples:
   --      0000      : reserved
   --      0001      : 192 samples
   --      0010-0101 : 576 * (2^(n-2)) samples, i.e. 576/1152/2304/4608
   --      0110      : get 8 bit (blocksize-1) from end of header
   --      0111      : get 16 bit (blocksize-1) from end of header
   --      1000-1111 : 256 * (2^(n-8)) samples, i.e. 256/512/1024/2048/4096/
   --                  8192/16384/32768
   --  <4> Sample rate:
   --      0000 : get from STREAMINFO metadata block
   --      0001 : 88.2kHz
   --      0010 : 176.4kHz
   --      0011 : 192kHz
   --      0100 : 8kHz
   --      0101 : 16kHz
   --      0110 : 22.05kHz
   --      0111 : 24kHz
   --      1000 : 32kHz
   --      1001 : 44.1kHz
   --      1010 : 48kHz
   --      1011 : 96kHz
   --      1100 : get 8 bit sample rate (in kHz) from end of header
   --      1101 : get 16 bit sample rate (in Hz) from end of header
   --      1110 : get 16 bit sample rate (in tens of Hz) from end of header
   --      1111 : invalid, to prevent sync-fooling string of 1s
   --  <4> Channel assignment
   --      0000-0111 : (number of independent channels)-1. Where defined, the
   --                  channel order follows SMPTE/ITU-R recommendations. See
   --                  above for the assignments.
   --  <3> Sample size in bits:
   --      000 : get from STREAMINFO metadata block
   --      001 : 8 bits per sample
   --      010 : 12 bits per sample
   --      011 : reserved
   --      100 : 16 bits per sample
   --      101 : 20 bits per sample
   --      110 : 24 bits per sample
   --      111 : reserved
   --  <1> 	Reserved:
   --      0 : mandatory value
   --      1 : reserved for future use
   --  <?> if (variable blocksize)
   --        <8-56>:"UTF-8" coded sample number (decoded number is 36 bits)
   --      else
   --        <8-48>:"UTF-8" coded frame number (decoded number is 31 bits)
   --  <?> if (blocksize bits == 011x)
   --        8/16 bit (blocksize-1)
   --  <?> if (sample rate bits == 11xx)
   --        8/16 bit sample rate
   --  <8> CRC-8 (polynomial = x^8 + x^2 + x^1 + x^0, initialized with 0) of
   --      everything before the crc, including the sync code
   type T (Blocking_Strategy : Frames.Blocking_Strategy := Fixed) is
      record
         Block_Size         : Types.Block_Size;
         Sample_Rate        : Types.Sample_Rate;
         Channel_Assignment : Types.Channel_Count;
         Sample_Size        : Types.Bits_Per_Sample;
         CRC_16             : Flac.CRC.Checksum_16;

         case Blocking_Strategy is
            when Variable =>
               Sample_Number : Types.Sample_Count; --  36 bits
            when Fixed =>
               Frame_Number : Types.Frame_Count; --  31 bits
         end case;
      end record;
  
   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File        : in     Ada.Streams.Stream_IO.File_Type;
                   Sample_Rate : in     Types.Sample_Rate;
                   Sample_Size : in     Types.Bits_Per_Sample;
                   Item        :    out T;
                   Error       :    out Boolean)
     with
       Relaxed_Initialization => Item,
       Global  => (Input => Flac.CRC.Constant_State),
       Pre     => (SPARK_Stream_IO.Is_Open (File => File) and
                   not Item'Constrained),
       Post    => (if not Error then Item'Initialized),
       Depends => (Error => (File, Flac.CRC.Constant_State),
                   Item  => (File,
                             Sample_Rate,
                             Sample_Size,
                             Item,
                             Flac.CRC.Constant_State));

end Flac.Frames;

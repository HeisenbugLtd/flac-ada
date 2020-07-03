------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Interfaces;
with SPARK_Stream_IO;

package body Flac.Frames with
  SPARK_Mode => On
is

   type From_EOH is (None, Eight_Bits, Sixteen_Bits, Sixteen_Bits_In_Tens);

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File        : in     Ada.Streams.Stream_IO.File_Type;
                   Sample_Rate : in     Types.Sample_Rate;
                   Sample_Size : in     Types.Bits_Per_Sample;
                   Item        :    out T;
                   Error       :    out Boolean)
   is
      One_Byte  : Ada.Streams.Stream_Element_Array (1 .. 1);
      Two_Bytes : Ada.Streams.Stream_Element_Array (1 .. 2);
      Sync_Code : Frames.Sync_Code;
      CRC       : Flac.CRC.Checksum_8 := 0;
      use type Flac.CRC.Checksum_8;
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      subtype Bits_3 is Ada.Streams.Stream_Element range 0 .. 2 ** 3 - 1;
      subtype Bits_4 is Ada.Streams.Stream_Element range 0 .. 2 ** 4 - 1;

      --  Data dependent stuff.
      Block_Size_From_EOH  : From_EOH range None .. Sixteen_Bits := None;
      Sample_Rate_From_EOH : From_EOH := None;
   begin
      SPARK_Stream_IO.Read (File  => File,
                            Item  => Two_Bytes,
                            Error => Error);

      if Error then
         return;
      end if;

      Flac.CRC.CRC8 (CRC  => CRC,
                     Data => Two_Bytes);

      case Blocking_Strategy'Val (Two_Bytes (2) and 2#0000_0001#) is
         when Variable =>
            Item := T'(Blocking_Strategy  => Variable,
                       Block_Size         => 16,
                       Sample_Rate        => 1,
                       Channel_Assignment => 1,
                       Sample_Size        => 4,
                       Sample_Number      => 0);
         when Fixed =>
            Item := T'(Blocking_Strategy => Fixed,
                       Block_Size         => 16,
                       Sample_Rate        => 1,
                       Channel_Assignment => 1,
                       Sample_Size        => 4,
                       Frame_Number       => 0);
      end case;

      Sync_Code :=
        Frames.Sync_Code (Two_Bytes (1)) * 2 ** 6 +
        Frames.Sync_Code (Two_Bytes (2) and 2#1111_1100#) / 2 ** 2;

      Error := Sync_Code /= 2#11_1111_1111_1110#;

      if Error then
         return;
      end if;

      Error := (Two_Bytes (2) and 2#0000_0010#) /= 0;

      if Error then
         return;
      end if;

      SPARK_Stream_IO.Read (File  => File,
                            Item  => One_Byte,
                            Error => Error);

      if Error then
         return;
      end if;

      Flac.CRC.CRC8 (CRC  => CRC,
                     Data => One_Byte);

      --  Upper four bits are block size, lower four bits are sample rate.
      declare
         BS_Designator : constant Bits_4 :=
           (One_Byte (1) and 2#1111_0000#) / 2 ** 4;
         SR_Designator : constant Bits_4 :=
           (One_Byte (1) and 2#0000_1111#);
         use type Types.Block_Size'Base;
      begin
         case BS_Designator is
            --      0001      : 192 samples
            --      0010-0101 : 576 * (2^(n-2)) samples, i.e. 576/1152/2304/4608
            --      0110      : get 8 bit (blocksize-1) from end of header
            --      0111      : get 16 bit (blocksize-1) from end of header
            --      1000-1111 : 256 * (2^(n-8)) samples, i.e. 256/512/1024/2048/4096/
            --                  8192/16384/32768
            when 2#0000# =>
               Error := True; -- Reserved
            when 2#0001# =>
               Item.Block_Size := 192;
            when 2#0010# =>
               Item.Block_Size := 576 * 1;
            when 2#0011# =>
               Item.Block_Size := 576 * 2;
            when 2#0100# =>
               Item.Block_Size := 576 * 4;
            when 2#0101# =>
               Item.Block_Size := 576 * 8;
            when 2#0110# =>
               Block_Size_From_EOH := Eight_Bits;
            when 2#0111# =>
               Block_Size_From_EOH := Sixteen_Bits;
            when 2#1000# =>
               Item.Block_Size := 256 * 1;
            when 2#1001# =>
               Item.Block_Size := 256 * 2;
            when 2#1010# =>
               Item.Block_Size := 256 * 4;
            when 2#1011# =>
               Item.Block_Size := 256 * 8;
            when 2#1100# =>
               Item.Block_Size := 256 * 16;
            when 2#1101# =>
               Item.Block_Size := 256 * 32;
            when 2#1110# =>
               Item.Block_Size := 256 * 64;
            when 2#1111# =>
               Item.Block_Size := 256 * 128;
         end case;

         if Error then
            return;
         end if;

         case SR_Designator is
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
            when 2#0000# =>
               Item.Sample_Rate := Sample_Rate;
            when 2#0001# =>
               Item.Sample_Rate := 88_200;
            when 2#0010# =>
               Item.Sample_Rate := 176_400;
            when 2#0011# =>
               Item.Sample_Rate := 192_000;
            when 2#0100# =>
               Item.Sample_Rate := 8_000;
            when 2#0101# =>
               Item.Sample_Rate := 16_000;
            when 2#0110# =>
               Item.Sample_Rate := 22_050;
            when 2#0111# =>
               Item.Sample_Rate := 24_000;
            when 2#1000# =>
               Item.Sample_Rate := 32_000;
            when 2#1001# =>
               Item.Sample_Rate := 44_100;
            when 2#1010# =>
               Item.Sample_Rate := 48_000;
            when 2#1011# =>
               Item.Sample_Rate := 96_000;
            when 2#1100# =>
               Sample_Rate_From_EOH := Eight_Bits;
            when 2#1101# =>
               Sample_Rate_From_EOH := Sixteen_Bits;
            when 2#1110# =>
               Sample_Rate_From_EOH := Sixteen_Bits_In_Tens;
            when 2#1111# =>
               Error := True;
         end case;
      end;

      if Error then
         return;
      end if;

      SPARK_Stream_IO.Read (File  => File,
                            Item  => One_Byte,
                            Error => Error);

      if Error then
         return;
      end if;

      Flac.CRC.CRC8 (CRC  => CRC,
                     Data => One_Byte);

      --  <4> Channel assignment
      --      0000-0111 : (number of independent channels)-1. Where defined, the
      --                  channel order follows SMPTE/ITU-R recommendations. See
      --                  above for the assignments.
      declare
         use type Types.Channel_Count'Base;
         Num_Channels : constant Types.Channel_Count'Base :=
           Types.Channel_Count'Base ((One_Byte (1) and 2#1111_0000#) / 2 ** 4) + 1;
      begin
         Error := Num_Channels not in Types.Channel_Count;

         if Error then
            return;
         end if;

         Item.Channel_Assignment := Num_Channels;
      end;

      pragma Assert_And_Cut (Item'Initialized); --  Needed for prover.

      --  <3> Sample size in bits:
      --      000 : get from STREAMINFO metadata block
      --      001 : 8 bits per sample
      --      010 : 12 bits per sample
      --      011 : reserved
      --      100 : 16 bits per sample
      --      101 : 20 bits per sample
      --      110 : 24 bits per sample
      --      111 : reserved
      declare
         Sample_Size_Designator : constant Bits_3 :=
           (One_Byte (1) and 2#0000_1110#) / 2;
      begin
         case Sample_Size_Designator is
            when 2#000# =>
               Item.Sample_Size := Sample_Size;
            when 2#001# =>
               Item.Sample_Size := 8;
            when 2#010# =>
               Item.Sample_Size := 12;
            when 2#100# =>
               Item.Sample_Size := 16;
            when 2#101# =>
               Item.Sample_Size := 20;
            when 2#110# =>
               Item.Sample_Size := 24;
            when 2#011# | 2#111# =>
               Error := True;
         end case;
      end;

      if Error then
         return;
      end if;

      pragma Assert_And_Cut (Item'Initialized); --  Needed for prover.

      --  <1> 	Reserved:
      --      0 : mandatory value
      --      1 : reserved for future use
      Error := (One_Byte (1) and 2#0000_0001#) /= 0;

      if Error then
         return;
      end if;

      --  <?> if (variable blocksize)
      --        <8-56>:"UTF-8" coded sample number (decoded number is 36 bits)
      --      else
      --        <8-48>:"UTF-8" coded frame number (decoded number is 31 bits)
      declare
         First_Byte          : Ada.Streams.Stream_Element_Array (1 .. 1);
         Num_Bytes_To_Follow : Ada.Streams.Stream_Element_Count := 0;
         First_Byte_Mask     : Ada.Streams.Stream_Element       := 0;
         Raw_Number          : Interfaces.Unsigned_64;
      begin
         --  Read the first byte to figure out how many are supposed to follow.
         SPARK_Stream_IO.Read (File  => File,
                               Item  => First_Byte,
                               Error => Error);

         if Error then
            return;
         end if;

         Flac.CRC.CRC8 (CRC  => CRC,
                        Data => First_Byte);

         case First_Byte (1) is
            when 2#0000_0000# .. 2#0111_1111# =>
               Num_Bytes_To_Follow := 0;        --  0 * 6 = 0
               First_Byte_Mask := 2#0111_1111#; --  +7    = 7

            when 2#1000_0000# .. 2#1011_1111# =>
               Error := True; --  Invalid code point

            when 2#1100_0000# .. 2#1101_1111# =>
               Num_Bytes_To_Follow := 1;        --  1 * 6 = 6
               First_Byte_Mask := 2#0001_1111#; -- +5     = 11

            when 2#1110_0000# .. 2#1110_1111# =>
               Num_Bytes_To_Follow := 2;        --  2 * 6 = 12
               First_Byte_Mask := 2#0000_1111#; --  +4    = 16

            when 2#1111_0000# .. 2#1111_0111# =>
               Num_Bytes_To_Follow := 3;        --  3 * 6 = 18
               First_Byte_Mask := 2#0000_0111#; --  +3    = 21

            when 2#1111_1000# .. 2#1111_1011# =>
               Num_Bytes_To_Follow := 4;        --  4 * 6 = 24
               First_Byte_Mask := 2#0000_0011#; --  +2    = 26

            when 2#1111_1100# .. 2#1111_1101# =>
               Num_Bytes_To_Follow := 5;        --  5 * 6 = 30
               First_Byte_Mask := 2#0000_0001#; --  +1    = 31

            when 2#1111_1110# =>
               Num_Bytes_To_Follow := 6;        --  6 * 6 = 36
               First_Byte_Mask := 2#0000_0000#; --  +0    = 36

            when 2#1111_1111# =>
               Error := True; --  Invalid code point.
         end case;

         if Error then
            return;
         end if;

         Raw_Number :=
           Interfaces.Unsigned_64 (First_Byte (1) and First_Byte_Mask);

         for I in 1 .. Num_Bytes_To_Follow loop
            pragma Loop_Invariant (not Error and Item'Initialized);

            declare
               Six_Bit_Number : Ada.Streams.Stream_Element_Array (1 .. 1);
               use type Interfaces.Unsigned_64;
            begin
               SPARK_Stream_IO.Read (File  => File,
                                     Item  => Six_Bit_Number,
                                     Error => Error);

               if Error then
                  return;
               end if;

               Flac.CRC.CRC8 (CRC  => CRC,
                              Data => Six_Bit_Number);

               Raw_Number := Interfaces.Shift_Left (Value  => Raw_Number,
                                                    Amount => 6);
               Raw_Number :=
                 Raw_Number or
                 Interfaces.Unsigned_64 (Six_Bit_Number (1) and 2#0011_1111#);
            end;
         end loop;

         case Item.Blocking_Strategy is
            when Variable =>
               Error :=
                 Raw_Number not in
                   Interfaces.Unsigned_64 (Types.Sample_Count'First) ..
                   Interfaces.Unsigned_64 (Types.Sample_Count'Last);

               if Error then
                  return;
               end if;

               Item.Sample_Number := Types.Sample_Count (Raw_Number);

            when Fixed =>
               Error :=
                 Raw_Number not in
                   Interfaces.Unsigned_64 (Types.Frame_Count'First) ..
                   Interfaces.Unsigned_64 (Types.Frame_Count'Last);

               if Error then
                  return;
               end if;

               Item.Frame_Number := Types.Frame_Count (Raw_Number);
         end case;
      end;

      case Block_Size_From_EOH is

         when None =>
            null;

         when Eight_Bits =>
            declare
               BS_Bits : Ada.Streams.Stream_Element_Array (1 .. 1);
               BS_Raw  : Types.Block_Size'Base;
               use type Types.Length_16;
            begin
               SPARK_Stream_IO.Read (File  => File,
                                     Item  => BS_Bits,
                                     Error => Error);

               if Error then
                  return;
               end if;

               Flac.CRC.CRC8 (CRC  => CRC,
                              Data => BS_Bits);

               BS_Raw := Types.Block_Size'Base (BS_Bits (1)) + 1;

               Error := BS_Raw not in Types.Block_Size;

               if Error then
                  return;
               end if;

               Item.Block_Size := BS_Raw;
            end;

         when Sixteen_Bits =>
            declare
               BS_Bits : Ada.Streams.Stream_Element_Array (1 .. 2);
               BS_Raw  : Types.Block_Size'Base;
               use type Types.Length_16;
            begin
               SPARK_Stream_IO.Read (File  => File,
                                     Item  => BS_Bits,
                                     Error => Error);

               if Error then
                  return;
               end if;

               Flac.CRC.CRC8 (CRC  => CRC,
                              Data => BS_Bits);

               BS_Raw :=
                 Types.Block_Size'Base (BS_Bits (1)) * 2 ** 8 +
                 Types.Block_Size'Base (BS_Bits (2));

               if BS_Raw = Types.Block_Size'Last then
                  Error := True;
               else
                  BS_Raw := BS_Raw + 1;
                  Error := BS_Raw not in Types.Block_Size;
               end if;

               if Error then
                  return;
               end if;

               Item.Block_Size := BS_Raw;
            end;

      end case;

      case Sample_Rate_From_EOH is

         when None =>
            null;

         when Eight_Bits =>
            declare
               SR_Bits : Ada.Streams.Stream_Element_Array (1 .. 1);
               SR_Raw  : Types.Sample_Rate'Base;
               use type Types.Sample_Rate'Base;
            begin
               SPARK_Stream_IO.Read (File  => File,
                                     Item  => SR_Bits,
                                     Error => Error);

               if Error then
                  return;
               end if;

               Flac.CRC.CRC8 (CRC  => CRC,
                              Data => SR_Bits);

               SR_Raw := Types.Sample_Rate'Base (SR_Bits (1)) + 1;

               Error := SR_Raw not in Types.Sample_Rate;

               if Error then
                  return;
               end if;

               Item.Sample_Rate := SR_Raw;
            end;

         when Sixteen_Bits | Sixteen_Bits_In_Tens =>
            declare
               SR_Bits : Ada.Streams.Stream_Element_Array (1 .. 2);
               SR_Raw  : Types.Sample_Rate'Base;
               use type Types.Sample_Rate'Base;
            begin
               SPARK_Stream_IO.Read (File  => File,
                                     Item  => SR_Bits,
                                     Error => Error);

               if Error then
                  return;
               end if;

               Flac.CRC.CRC8 (CRC  => CRC,
                              Data => SR_Bits);

               SR_Raw :=
                 Types.Sample_Rate'Base (SR_Bits (1)) * 2 ** 8 +
                 Types.Sample_Rate'Base (SR_Bits (2)) + 1;

               if Sample_Rate_From_EOH = Sixteen_Bits_In_Tens then
                  SR_Raw := SR_Raw * 10;
               end if;

               Error := SR_Raw not in Types.Sample_Rate;

               if Error then
                  return;
               end if;

               Item.Sample_Rate := SR_Raw;
            end;

      end case;

      declare
         CRC_Bits : Ada.Streams.Stream_Element_Array (1 .. 1);
      begin
         SPARK_Stream_IO.Read (File  => File,
                               Item  => CRC_Bits,
                               Error => Error);

         if Error then
            return;
         end if;

         Flac.CRC.CRC8 (CRC  => CRC,
                        Data => CRC_Bits);
      end;

      pragma Assert (not Error and Item'Initialized);
      --  Ensure that post condition holds.

      --  CRC check at last.
      Error := CRC /= 0;
   end Read;

end Flac.Frames;

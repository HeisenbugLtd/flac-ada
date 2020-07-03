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

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   type From_EOH is (None, Eight_Bits, Sixteen_Bits, Sixteen_Bits_In_Tens);

   subtype Bits_3 is Ada.Streams.Stream_Element range 0 .. 2 ** 3 - 1;
   subtype Bits_4 is Ada.Streams.Stream_Element range 0 .. 2 ** 4 - 1;

   ---------------------------------------------------------------------------
   --  Read_With_CRC
   ---------------------------------------------------------------------------
   procedure Read_With_CRC (File   : in     Ada.Streams.Stream_IO.File_Type;
                            CRC_8  : in out Flac.CRC.Checksum_8;
                            CRC_16 : in out Flac.CRC.Checksum_16;
                            Item   :    out Ada.Streams.Stream_Element_Array;
                            Error  :    out Boolean)
     with
       Relaxed_Initialization => Item,
       Global  => (Input => Flac.CRC.Constant_State),
       Pre     => SPARK_Stream_IO.Is_Open (File => File),
       Post    => (if not Error then Item'Initialized),
       Depends => (CRC_8  => (CRC_8, File, Item, Flac.CRC.Constant_State),
                   CRC_16 => (CRC_16, File, Item, Flac.CRC.Constant_State),
                   Item   => (Item, File),
                   Error  => (File, Item));

   ---------------------------------------------------------------------------
   --  Read_With_CRC
   ---------------------------------------------------------------------------
   procedure Read_With_CRC (File   : in     Ada.Streams.Stream_IO.File_Type;
                            CRC_8  : in out Flac.CRC.Checksum_8;
                            CRC_16 : in out Flac.CRC.Checksum_16;
                            Item   :    out Ada.Streams.Stream_Element_Array;
                            Error  :    out Boolean) is
   begin
      SPARK_Stream_IO.Read (File  => File,
                            Item  => Item,
                            Error => Error);

      if Error then
         return;
      end if;

      Flac.CRC.CRC8 (CRC  => CRC_8,
                     Data => Item);
      Flac.CRC.CRC16 (CRC  => CRC_16,
                      Data => Item);
   end Read_With_CRC;

   ---------------------------------------------------------------------------
   --  Decode_Block_Size
   ---------------------------------------------------------------------------
   procedure Decode_Block_Size (BS_Designator : in     Bits_4;
                                Block_Size    :    out Types.Block_Size;
                                Get_From_EOH  :    out From_EOH;
                                Error         :    out Boolean)
     with
       Post => Get_From_EOH in None .. Sixteen_Bits;

   ---------------------------------------------------------------------------
   --  Decode_Sample_Rate
   ---------------------------------------------------------------------------
   procedure Decode_Sample_Rate (SR_Designator   : in     Bits_4;
                                 Hdr_Sample_Rate : in     Types.Sample_Rate;
                                 Sample_Rate     :    out Types.Sample_Rate;
                                 Get_From_EOH    :    out From_EOH;
                                 Error           :    out Boolean);

   ---------------------------------------------------------------------------
   --  Decode_Sample_Size
   ---------------------------------------------------------------------------
   procedure Decode_Sample_Size (SS_Designator   : in     Bits_3;
                                 Hdr_Sample_Size : in     Types.Bits_Per_Sample;
                                 Sample_Size     :    out Types.Bits_Per_Sample;
                                 Error           :    out Boolean);

   ---------------------------------------------------------------------------
   --  Validate_Sync_Block
   ---------------------------------------------------------------------------
   procedure Validate_Sync_Block
     (Item  : in     Ada.Streams.Stream_Element_Array;
      Error :    out Boolean)
     with
       Pre =>
         --  restrict possible range, otherwise 'Length may fail
         Item'First >= 0         and then
         Item'Last < 2 ** 63 - 1 and then
         Item'Length = 2;

   ---------------------------------------------------------------------------
   --  Decode_Block_Size
   ---------------------------------------------------------------------------
   procedure Decode_Block_Size (BS_Designator : in     Bits_4;
                                Block_Size    :    out Types.Block_Size;
                                Get_From_EOH  :    out From_EOH;
                                Error         :    out Boolean)
   is
      use type Types.Block_Size;
   begin
      case BS_Designator is
         --      0001      : 192 samples
         --      0010-0101 : 576 * (2^(n-2)) samples, i.e. 576/1152/2304/4608
         --      0110      : get 8 bit (blocksize-1) from end of header
         --      0111      : get 16 bit (blocksize-1) from end of header
         --      1000-1111 : 256 * (2^(n-8)) samples, i.e. 256/512/1024/2048/4096/
         --                  8192/16384/32768
         when 2#0000# => -- Reserved
            Block_Size   := Types.Block_Size'First;
            Get_From_EOH := None;
            Error        := True;

         when 2#0001# =>
            Block_Size   := 192;
            Get_From_EOH := None;
            Error        := False;

         when 2#0010# =>
            Block_Size   := 576 * 1;
            Get_From_EOH := None;
            Error        := False;

         when 2#0011# =>
            Block_Size   := 576 * 2;
            Get_From_EOH := None;
            Error        := False;

         when 2#0100# =>
            Block_Size   := 576 * 4;
            Get_From_EOH := None;
            Error        := False;

         when 2#0101# =>
            Block_Size   := 576 * 8;
            Get_From_EOH := None;
            Error        := False;

         when 2#0110# =>
            Block_Size   := Types.Block_Size'First;
            Get_From_EOH := Eight_Bits;
            Error        := False;

         when 2#0111# =>
            Block_Size   := Types.Block_Size'First;
            Get_From_EOH := Sixteen_Bits;
            Error        := False;

         when 2#1000# =>
            Block_Size   := 256 * 1;
            Get_From_EOH := None;
            Error        := False;

         when 2#1001# =>
            Block_Size   := 256 * 2;
            Get_From_EOH := None;
            Error        := False;

         when 2#1010# =>
            Block_Size   := 256 * 4;
            Get_From_EOH := None;
            Error        := False;

         when 2#1011# =>
            Block_Size   := 256 * 8;
            Get_From_EOH := None;
            Error        := False;

         when 2#1100# =>
            Block_Size   := 256 * 16;
            Get_From_EOH := None;
            Error        := False;

         when 2#1101# =>
            Block_Size   := 256 * 32;
            Get_From_EOH := None;
            Error        := False;

         when 2#1110# =>
            Block_Size   := 256 * 64;
            Get_From_EOH := None;
            Error        := False;

         when 2#1111# =>
            Block_Size   := 256 * 128;
            Get_From_EOH := None;
            Error        := False;

      end case;
   end Decode_Block_Size;

   ---------------------------------------------------------------------------
   --  Decode_Sample_Rate
   ---------------------------------------------------------------------------
   procedure Decode_Sample_Rate (SR_Designator   : in     Bits_4;
                                 Hdr_Sample_Rate : in     Types.Sample_Rate;
                                 Sample_Rate     :    out Types.Sample_Rate;
                                 Get_From_EOH    :    out From_EOH;
                                 Error           :    out Boolean)
   is
      use type Types.Block_Size'Base;
   begin
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
            Sample_Rate  := Hdr_Sample_Rate;
            Get_From_EOH := None;
            Error        := False;

         when 2#0001# =>
            Sample_Rate  := 88_200;
            Get_From_EOH := None;
            Error        := False;

         when 2#0010# =>
            Sample_Rate  := 176_400;
            Get_From_EOH := None;
            Error        := False;

         when 2#0011# =>
            Sample_Rate  := 192_000;
            Get_From_EOH := None;
            Error        := False;

         when 2#0100# =>
            Sample_Rate  := 8_000;
            Get_From_EOH := None;
            Error        := False;

         when 2#0101# =>
            Sample_Rate  := 16_000;
            Get_From_EOH := None;
            Error        := False;

         when 2#0110# =>
            Sample_Rate  := 22_050;
            Get_From_EOH := None;
            Error        := False;

         when 2#0111# =>
            Sample_Rate  := 24_000;
            Get_From_EOH := None;
            Error        := False;

         when 2#1000# =>
            Sample_Rate  := 32_000;
            Get_From_EOH := None;
            Error        := False;

         when 2#1001# =>
            Sample_Rate  := 44_100;
            Get_From_EOH := None;
            Error        := False;

         when 2#1010# =>
            Sample_Rate  := 48_000;
            Get_From_EOH := None;
            Error        := False;

         when 2#1011# =>
            Sample_Rate  := 96_000;
            Get_From_EOH := None;
            Error        := False;

         when 2#1100# =>
            Sample_Rate  := Types.Sample_Rate'First;
            Get_From_EOH := Eight_Bits;
            Error        := False;

         when 2#1101# =>
            Sample_Rate  := Types.Sample_Rate'First;
            Get_From_EOH := Sixteen_Bits;
            Error        := False;

         when 2#1110# =>
            Sample_Rate  := Types.Sample_Rate'First;
            Get_From_EOH := Sixteen_Bits_In_Tens;
            Error        := False;

         when 2#1111# => --  Invalid
            Sample_Rate  := Types.Sample_Rate'First;
            Get_From_EOH := None;
            Error        := True;

      end case;
   end Decode_Sample_Rate;

   ---------------------------------------------------------------------------
   --  Decode_Sample_Size
   ---------------------------------------------------------------------------
   procedure Decode_Sample_Size (SS_Designator   : in     Bits_3;
                                 Hdr_Sample_Size : in     Types.Bits_Per_Sample;
                                 Sample_Size     :    out Types.Bits_Per_Sample;
                                 Error           :    out Boolean) is
   begin
      --  <3> Sample size in bits:
      --      000 : get from STREAMINFO metadata block
      --      001 : 8 bits per sample
      --      010 : 12 bits per sample
      --      011 : reserved
      --      100 : 16 bits per sample
      --      101 : 20 bits per sample
      --      110 : 24 bits per sample
      --      111 : reserved
      case SS_Designator is
         when 2#000# =>
            Sample_Size := Hdr_Sample_Size;
            Error       := False;

         when 2#001# =>
            Sample_Size := 8;
            Error       := False;

         when 2#010# =>
            Sample_Size := 12;
            Error       := False;

         when 2#100# =>
            Sample_Size := 16;
            Error       := False;

         when 2#101# =>
            Sample_Size := 20;
            Error       := False;

         when 2#110# =>
            Sample_Size := 24;
            Error       := False;

         when 2#011# | 2#111# =>
            Sample_Size := Types.Bits_Per_Sample'First;
            Error       := True;

      end case;
   end Decode_Sample_Size;

   ---------------------------------------------------------------------------
   --  Validate_Sync_Block
   ---------------------------------------------------------------------------
   procedure Validate_Sync_Block
     (Item  : in     Ada.Streams.Stream_Element_Array;
      Error :    out Boolean)
   is
      Sync_Code : Frames.Sync_Code :=
        Frames.Sync_Code (Item (Item'First)) * 2 ** 6 +
        Frames.Sync_Code (Item (Item'Last) and 2#1111_1100#) / 2 ** 2;
   begin
      Error :=
        Sync_Code /= 2#11_1111_1111_1110# or
        (Item (Item'Last) and 2#0000_0010#) /= 0;
   end Validate_Sync_Block;

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File        : in     Ada.Streams.Stream_IO.File_Type;
                   Sample_Rate : in     Types.Sample_Rate;
                   Sample_Size : in     Types.Bits_Per_Sample;
                   Item        :    out T;
                   Error       :    out Boolean)
   is
      Fixed_Block : Ada.Streams.Stream_Element_Array (1 .. 4);
      --  First 32 bits are fixed size, so read them at once.
      CRC_8  : Flac.CRC.Checksum_8  := 0;
      CRC_16 : Flac.CRC.Checksum_16 := 0;

      use type Flac.CRC.Checksum_8;
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      --  Data dependent stuff.
      Block_Size_From_EOH  : From_EOH range None .. Sixteen_Bits;
      Sample_Rate_From_EOH : From_EOH;

      --  Temporary results.
      Rd_Blocking_Strategy   : Frames.Blocking_Strategy;
      Rd_Block_Size          : Types.Block_Size;
      Rd_Sample_Rate         : Types.Sample_Rate;
      Rd_Channel_Assignment  : Types.Channel_Count;
      Rd_Sample_Size         : Types.Bits_Per_Sample;

      type Counter (BS : Blocking_Strategy := Fixed) is
         record
            case BS is
               when Variable =>
                  Sample_Number : Types.Sample_Count;
               when Fixed =>
                  Frame_Number : Types.Frame_Count;
            end case;
         end record;

      Rd_Count : Counter;
   begin
      Read_With_CRC (File   => File,
                     CRC_8  => CRC_8,
                     CRC_16 => CRC_16,
                     Item   => Fixed_Block,
                     Error  => Error);

      if Error then
         return;
      end if;

      Validate_Sync_Block (Item  => Fixed_Block (1 .. 2),
                           Error => Error);

      if Error then
         return;
      end if;

      Rd_Blocking_Strategy :=
        Frames.Blocking_Strategy'Val (Fixed_Block (2) and 2#0000_0001#);

      case Rd_Blocking_Strategy is
         when Variable =>
            Rd_Count := Counter'(BS            => Variable,
                                 Sample_Number => 0);
         when Fixed    =>
            Rd_Count := Counter'(BS           => Fixed,
                                 Frame_Number => 0);
      end case;

      --  Upper four bits are block size
      Decode_Block_Size
        (BS_Designator => (Fixed_Block (3) and 2#1111_0000#) / 2 ** 4,
         Block_Size    => Rd_Block_Size,
         Get_From_EOH  => Block_Size_From_EOH,
         Error         => Error);

      if Error then
         return;
      end if;

      --  Lower four bits are sample rate.
      Decode_Sample_Rate
        (SR_Designator   => Fixed_Block (3) and 2#0000_1111#,
         Hdr_Sample_Rate => Sample_Rate,
         Sample_Rate     => Rd_Sample_Rate,
         Get_From_EOH    => Sample_Rate_From_EOH,
         Error           => Error);

      if Error then
         return;
      end if;

      --  <4> Channel assignment
      --      0000-0111 : (number of independent channels)-1. Where defined, the
      --                  channel order follows SMPTE/ITU-R recommendations. See
      --                  above for the assignments.
      declare
         use type Types.Channel_Count'Base;
         Num_Channels : constant Types.Channel_Count'Base :=
           Types.Channel_Count'Base ((Fixed_Block (4) and 2#1111_0000#) / 2 ** 4) + 1;
      begin
         Error := Num_Channels not in Types.Channel_Count;

         if Error then
            return;
         end if;

         Rd_Channel_Assignment := Num_Channels;
      end;

      Decode_Sample_Size
        (SS_Designator   => (Fixed_Block (4) and 2#0000_1110#) / 2,
         Hdr_Sample_Size => Sample_Size,
         Sample_Size     => Rd_Sample_Size,
         Error           => Error);

      if Error then
         return;
      end if;

      --  <1> 	Reserved:
      --      0 : mandatory value
      --      1 : reserved for future use
      Error := (Fixed_Block (4) and 2#0000_0001#) /= 0;

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
         Read_With_CRC (File   => File,
                        CRC_8  => CRC_8,
                        CRC_16 => CRC_16,
                        Item   => First_Byte,
                        Error  => Error);

         if Error then
            return;
         end if;

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
            pragma Loop_Invariant (not Error);

            declare
               Six_Bit_Number : Ada.Streams.Stream_Element_Array (1 .. 1);
               use type Interfaces.Unsigned_64;
            begin
               Read_With_CRC (File   => File,
                              CRC_8  => CRC_8,
                              CRC_16 => CRC_16,
                              Item   => Six_Bit_Number,
                              Error  => Error);

               if Error then
                  return;
               end if;

               Raw_Number :=
                 Interfaces.Shift_Left (Value  => Raw_Number,
                                        Amount => 6) or
                 Interfaces.Unsigned_64 (Six_Bit_Number (1) and 2#0011_1111#);
            end;
         end loop;

         case Rd_Blocking_Strategy is
            when Variable =>
               Error :=
                 Raw_Number not in
                   Interfaces.Unsigned_64 (Types.Sample_Count'First) ..
                   Interfaces.Unsigned_64 (Types.Sample_Count'Last);

               if Error then
                  return;
               end if;

               Rd_Count.Sample_Number := Types.Sample_Count (Raw_Number);

            when Fixed =>
               Error :=
                 Raw_Number not in
                   Interfaces.Unsigned_64 (Types.Frame_Count'First) ..
                   Interfaces.Unsigned_64 (Types.Frame_Count'Last);

               if Error then
                  return;
               end if;

               Rd_Count.Frame_Number := Types.Frame_Count (Raw_Number);
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
               Read_With_CRC (File   => File,
                              CRC_8  => CRC_8,
                              CRC_16 => CRC_16,
                              Item   => BS_Bits,
                              Error  => Error);

               if Error then
                  return;
               end if;

               BS_Raw := Types.Block_Size'Base (BS_Bits (1)) + 1;

               Error := BS_Raw not in Types.Block_Size;

               if Error then
                  return;
               end if;

               Rd_Block_Size := BS_Raw;
            end;

         when Sixteen_Bits =>
            declare
               BS_Bits : Ada.Streams.Stream_Element_Array (1 .. 2);
               BS_Raw  : Types.Block_Size'Base;
               use type Types.Length_16;
            begin
               Read_With_CRC (File   => File,
                              CRC_8  => CRC_8,
                              CRC_16 => CRC_16,
                              Item   => BS_Bits,
                              Error  => Error);

               if Error then
                  return;
               end if;

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

               Rd_Block_Size := BS_Raw;
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
               Read_With_CRC (File   => File,
                              CRC_8  => CRC_8,
                              CRC_16 => CRC_16,
                              Item   => SR_Bits,
                              Error  => Error);

               if Error then
                  return;
               end if;

               SR_Raw := Types.Sample_Rate'Base (SR_Bits (1)) + 1;

               Error := SR_Raw not in Types.Sample_Rate;

               if Error then
                  return;
               end if;

               Rd_Sample_Rate := SR_Raw;
            end;

         when Sixteen_Bits | Sixteen_Bits_In_Tens =>
            declare
               SR_Bits : Ada.Streams.Stream_Element_Array (1 .. 2);
               SR_Raw  : Types.Sample_Rate'Base;
               use type Types.Sample_Rate'Base;
            begin
               Read_With_CRC (File   => File,
                              CRC_8  => CRC_8,
                              CRC_16 => CRC_16,
                              Item   => SR_Bits,
                              Error  => Error);

               if Error then
                  return;
               end if;

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

               Rd_Sample_Rate := SR_Raw;
            end;

      end case;

      declare
         Dummy_CRC_Bits : Ada.Streams.Stream_Element_Array (1 .. 1);
      begin
         Read_With_CRC (File   => File,
                        CRC_8  => CRC_8,
                        CRC_16 => CRC_16,
                        Item   => Dummy_CRC_Bits,
                        Error  => Error);

         if Error then
            return;
         end if;
      end;

      --  CRC check at last.
      Error := CRC_8 /= 0;

      if Error then
         return;
      end if;

      --  Assign the result.
      case Rd_Blocking_Strategy is
         when Variable =>
            Item := Frames.T'(Blocking_Strategy  => Variable,
                              Block_Size         => Rd_Block_Size,
                              Sample_Rate        => Rd_Sample_Rate,
                              Channel_Assignment => Rd_Channel_Assignment,
                              Sample_Size        => Rd_Sample_Size,
                              Sample_Number      => Rd_Count.Sample_Number,
                              CRC_16             => CRC_16);
         when Fixed =>
            Item := Frames.T'(Blocking_Strategy  => Fixed,
                              Block_Size         => Rd_Block_Size,
                              Sample_Rate        => Rd_Sample_Rate,
                              Channel_Assignment => Rd_Channel_Assignment,
                              Sample_Size        => Rd_Sample_Size,
                              Frame_Number       => Rd_Count.Frame_Number,
                              CRC_16             => CRC_16);
      end case;
   end Read;

end Flac.Frames;

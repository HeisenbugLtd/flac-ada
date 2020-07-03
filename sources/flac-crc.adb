------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body Flac.CRC with
  SPARK_Mode    => On,
  Refined_State => (Constant_State => (CRC_8_Table, CRC_16_Table))
is

   CRC_8_Table : array (Ada.Streams.Stream_Element) of Checksum_8 with
     Constant_After_Elaboration => True;

   CRC_16_Table : array (Ada.Streams.Stream_Element) of Checksum_16 with
     Constant_After_Elaboration => True;

   ---------------------------------------------------------------------------
   --  CRC8
   ---------------------------------------------------------------------------
   procedure CRC8 (CRC  : in out Checksum_8;
                   Data : in     Ada.Streams.Stream_Element_Array) is
      use type Ada.Streams.Stream_Element;
   begin
      for Byte of Data loop
         CRC := CRC_8_Table (Ada.Streams.Stream_Element (CRC) xor Byte);
      end loop;
   end CRC8;

   ---------------------------------------------------------------------------
   --  CRC16
   ---------------------------------------------------------------------------
   procedure CRC16 (CRC  : in out Checksum_16;
                    Data : in     Ada.Streams.Stream_Element_Array) is
      use type Ada.Streams.Stream_Element;
   begin
      for Byte of Data loop
         CRC :=
           CRC_16_Table
             (Ada.Streams.Stream_Element (CRC and 16#00FF#) xor Byte);
      end loop;
   end CRC16;

begin
   for Byte in CRC_8_Table'Range loop
      declare
         CRC : Checksum_8 := Checksum_8 (Byte);
      begin
         for Bit in 0 .. 7 loop
            if (CRC and 2#1000_0000#) /= 0 then
               CRC := Shift_Left (Value  => CRC,
                                  Amount => 1) xor 16#07#;
            else
               CRC := Shift_Left (Value  => CRC,
                                  Amount => 1);
            end if;
         end loop;

         CRC_8_Table (Byte) := CRC;
      end;
   end loop;

   for Byte in CRC_16_Table'Range loop
      declare
         CRC : Checksum_16 := Checksum_16 (Byte);
      begin
         for Bit in 0 .. 15 loop
            if (CRC and 2#1000_0000_0000_0000#) /= 0 then
               CRC := Shift_Left (Value  => CRC,
                                  Amount => 1) xor 2#1000_0000_0000_0101#;
            else
               CRC := Shift_Left (Value  => CRC,
                                  Amount => 1);
            end if;
         end loop;

         CRC_16_Table (Byte) := CRC;
      end;
   end loop;
end Flac.CRC;

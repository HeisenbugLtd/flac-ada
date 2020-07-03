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
  Refined_State => (Constant_State => CRC_8_Table)
is

   CRC_8_Table : array (Checksum_8) of Checksum_8 with
     Constant_After_Elaboration => True;

   ---------------------------------------------------------------------------
   --  CRC8
   ---------------------------------------------------------------------------
   procedure CRC8 (CRC  : in out Checksum_8;
                   Data : in     Ada.Streams.Stream_Element_Array) is
   begin
      for Byte of Data loop
         CRC := CRC_8_Table (CRC xor Checksum_8 (Byte));
      end loop;
   end CRC8;

begin
   for Byte in CRC_8_Table'Range loop
      declare
         CRC : Checksum_8 := Byte;
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
end Flac.CRC;

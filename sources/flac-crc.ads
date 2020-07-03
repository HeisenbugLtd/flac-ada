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
--  CRC implementations
------------------------------------------------------------------------------

with Ada.Streams;
with Interfaces;

package Flac.CRC with
  SPARK_Mode     => On,
  Abstract_State => (Constant_State)
is

   type Checksum_8 is new Interfaces.Unsigned_8;

   ---------------------------------------------------------------------------
   --  CRC8
   ---------------------------------------------------------------------------
   procedure CRC8 (CRC  : in out Checksum_8;
                   Data : in     Ada.Streams.Stream_Element_Array) with
     Global  => (Input => Constant_State),
     Depends => (CRC => (CRC, Data, Constant_State));

end Flac.CRC;

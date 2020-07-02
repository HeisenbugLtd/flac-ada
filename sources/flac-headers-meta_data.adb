------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Streams.Stream_IO;
with SPARK_Stream_IO;
with System;

package body FLAC.Headers.Meta_Data with
  SPARK_Mode => On
is

   type Unsigned_7 is range 0 .. 127
     with
       Size        => 7,
       Object_Size => 8;

   ---------------------------------------------------------------------------
   --  To_Block_Type
   ---------------------------------------------------------------------------
   procedure To_Block_Type (Source : in     Unsigned_7;
                            Target :    out Types.Block_Type;
                            Error  :    out Boolean)
     with
       Relaxed_Initialization => Target,
       Depends => (Error  => Source,
                   Target => Source),
       Post    => (if not Error then Target'Initialized);

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File  : in     Ada.Streams.Stream_IO.File_Type;
                   Item  :    out T;
                   Error :    out Boolean)
   is
      Raw_Data : Ada.Streams.Stream_Element_Array (1 .. Meta_Data_Length);
      BT       : Types.Block_Type;
      use type Ada.Streams.Stream_Element;
      use type Types.Length_24;
   begin
      SPARK_Stream_IO.Read (File  => File,
                            Item  => Raw_Data,
                            Error => Error);

      if Error then
         return;
      end if;

      To_Block_Type (Source => Unsigned_7 (Raw_Data (1) and 2#0111_1111#),
                     Target => BT,
                     Error  => Error);

      if Error then
         return;
      end if;

      Item.Last := (Raw_Data (1) and 2#1000_0000#) /= 0;
      Item.Block_Type := BT;
      Item.Length :=
        Types.Length_24 (Raw_Data (2)) * 256 ** 2 +
        Types.Length_24 (Raw_Data (3)) * 256 +
        Types.Length_24 (Raw_Data (4));
   end Read;

   ---------------------------------------------------------------------------
   --  To_Block_Type
   ---------------------------------------------------------------------------
   procedure To_Block_Type (Source : in     Unsigned_7;
                            Target :    out Types.Block_Type;
                            Error  :    out Boolean)
     with
       SPARK_Mode => Off
   is
      function Raw_Convert is
        new Ada.Unchecked_Conversion (Source => Unsigned_7,
                                      Target => Types.Block_Type);

      use type Types.Block_Type;
   begin
      Target := Raw_Convert (S => Source);

      if not Target'Valid then
         Target := Types.Padding; --  We ignore all of those, so pretend
                                  --  they are just padding.
      end if;

      --  This one we should never read.  If we do the file is corrupt.
      Error := Target = Types.Invalid;
   end To_Block_Type;

end FLAC.Headers.Meta_Data;

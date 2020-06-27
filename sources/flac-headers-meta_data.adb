------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Unchecked_Conversion;
with System;

package body FLAC.Headers.Meta_Data with
  SPARK_Mode => On
is

   type Unsigned_7 is range 0 .. 127
     with
       Size        => 7,
       Object_Size => 8;

   type Full_T is
      record
         Last       : Boolean;
         Block_Type : Unsigned_7;
         Length     : Types.Length_24;
      end record
     with
       Size        => 32,
       Object_Size => 32,
       Bit_Order   => System.Low_Order_First;
   for Full_T use
      record
         Last       at 0 range  7 .. 7;
         Block_Type at 0 range  0 .. 6;
         Length     at 0 range  8 .. 31;
      end record;

   function To_Full is new Ada.Unchecked_Conversion (Source => Raw_T,
                                                     Target => Full_T);

   procedure Convert (Source           : in     Raw_T;
                      Target           :    out T;
                      Conversion_Error :    out Boolean)
   is

      procedure To_Block_Type (Source : in     Unsigned_7;
                               Target :    out Types.Block_Type)
        with
          Relaxed_Initialization => Target,
          Global  => (Output => Conversion_Error),
          Depends => (Conversion_Error => Source,
                      Target           => Source),
          Post    => (if not Conversion_Error then Target'Initialized);

      procedure To_Block_Type (Source : in     Unsigned_7;
                               Target :    out Types.Block_Type)
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
         Conversion_Error := Target = Types.Invalid;
      end To_Block_Type;

      Full_View  : Full_T := To_Full (S => Source);
      Block_Type : Types.Block_Type := Types.Invalid;
   begin
      To_Block_Type (Source => Full_View.Block_Type,
                     Target => Block_Type);
      Target := T'(Last       => Full_View.Last,
                   Block_Type => Block_Type,
                   Length     => Types.BE_Swap (Full_View.Length));
   end Convert;

end FLAC.Headers.Meta_Data;

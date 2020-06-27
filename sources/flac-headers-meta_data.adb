------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Unchecked_Conversion;

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
       Object_Size => 32;
   for Full_T use
      record
         Last       at 0 range 0 ..  0;
         Block_Type at 0 range 1 ..  7;
         Length     at 1 range 0 .. 23;
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
      begin
         Target := Raw_Convert (S => Source);

         Conversion_Error := not Target'Valid;
      end To_Block_Type;

      Full_View  : Full_T := To_Full (S => Source);
      Block_Type : Types.Block_Type;
   begin
      To_Block_Type (Source => Full_View.Block_Type,
                     Target => Block_Type);
      Target := T'(Last       => Full_View.Last,
                   Block_Type => Block_Type,
                   Length     => Full_View.Length);
   end Convert;

end FLAC.Headers.Meta_Data;

------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body Flac.Headers with
  Pure       => True,
  SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  BE_Swap
   ---------------------------------------------------------------------------
   procedure BE_Swap (Arg : in out Stream_Info_Raw) is
      use type Ada.Streams.Stream_Element;

      function Low_Nibble
        (Arg : in Ada.Streams.Stream_Element) return Ada.Streams.Stream_Element
      is
        (Arg and 16#0F#);

      function High_Nibble
        (Arg : in Ada.Streams.Stream_Element) return Ada.Streams.Stream_Element
      is
        ((Arg and 16#F0#) / 16);
   begin
      if Types.Needs_Swap then
         --  First two 16 bit value for minimum and maximum block size.
         Arg (1 .. 4) :=
           Ada.Streams.Stream_Element_Array'(Arg (2),
                                             Arg (1),
                                             Arg (4),
                                             Arg (3));
         --  Minimum and maximum frame size are 24 bit values.
         Arg (5 .. 10) :=
           Ada.Streams.Stream_Element_Array'(Arg (7),
                                             Arg (6),
                                             Arg (5),
                                             Arg (10),
                                             Arg (9),
                                             Arg (8));

         --  Now the fun part begins. Let's make a drawing:
         --                         .----------- Sample rate
         --                         |   .------- # channels
         --                         |   |     .- # bits/sample
         --   .----------------------. .-..----.
         --   |                      | | ||    |
         --  |....|....|....|....|....|....|....|....|...  ...|
         --  '---------'---------'---------'---------'---  ---'
         --   b11       b12       b13       b14       b15 ..
         Arg (11 .. 14) :=
           Ada.Streams.Stream_Element_Array'
             (11 => (16 * Low_Nibble (Arg (12))   or
                       High_Nibble (Arg (13))),
              12 => (16 * Low_Nibble (Arg (11)) or
                       High_Nibble (Arg (12))),
              13 => (High_Nibble (Arg (11))              or
                       (  8 * (Arg (13) and 16#0E#)) or
                       (128 * (High_Nibble (Arg (14)) and 16#01#))),
              14 => ((8 * Low_Nibble (Arg (13)) and 16#01#) or
                       (High_Nibble (Arg (14)) / 2)));
      end if;
   end BE_Swap;

end Flac.Headers;

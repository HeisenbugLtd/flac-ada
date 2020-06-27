------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body FLAC.Headers.Stream_Info with
  Pure       => True,
  SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  BE_Swap
   ---------------------------------------------------------------------------
   function BE_Swap (Arg : in Raw_T) return Raw_T;

   ---------------------------------------------------------------------------
   --  BE_Swap
   ---------------------------------------------------------------------------
   function BE_Swap (Arg : in Raw_T) return Raw_T is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Array;

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
         return Raw_T'((1  => Arg (2),
                        2  => Arg (1), --  Min_Block_Size
                        3  => Arg (4),
                        4  => Arg (3), --  Max_Block_Size
                        --  Minimum and maximum frame size are 24 bit values.
                        5  => Arg (7),
                        6  => Arg (6),
                        7  => Arg (5), --  Min_Frame_Size
                        8  => Arg (10),
                        9  => Arg (9),
                        10 => Arg (8), --  Max_Frame_Size
                        --  Now the fun part begins. Let's make a drawing:
                        --                         .----------- Sample rate
                        --                         |   .------- # channels
                        --                         |   |     .- # bits/sample
                        --   .----------------------. .-..----.
                        --   |                      | | ||    |
                        --  |....|....|....|....|....|....|....|....|...  ...|
                        --  '---------'---------'---------'---------'---  ---'
                        --   b11       b12       b13       b14       b15 ..
                        11 => (16 * Low_Nibble (Arg (12)) or
                                 High_Nibble (Arg (13))),
                        12 => (16 * Low_Nibble (Arg (11)) or
                                 High_Nibble (Arg (12))),
                        13 => (High_Nibble (Arg (11))          or
                                 (  8 * (Arg (13) and 16#0E#)) or
                                 (128 * (High_Nibble (Arg (14)) and 16#01#))),
                        14 => ((8 * Low_Nibble (Arg (13)) and 16#01#) or
                                 (High_Nibble (Arg (14)) / 2) or
                                 16 * Low_Nibble (Arg (18))),
                        15 => ((16 * Low_Nibble (Arg (17))) or
                                 High_Nibble (Arg (18))),
                        16 => (16 * Low_Nibble (Arg (16)) or
                                 High_Nibble (Arg (17))),
                        17 => (16 * Low_Nibble (Arg (15)) or
                                 High_Nibble (Arg (16))),
                        18 => (16 * Low_Nibble (Arg (15))))
                       & Arg (19 .. 34)); --  MD5 checksum
      else
         return Arg;
      end if;
   end BE_Swap;

   type Full_T is
      record
         Min_Block_Size  : Types.Length_16; --  samples
         Max_Block_Size  : Types.Length_16; --  samples!
         Min_Frame_Size  : Types.Length_24; --  bytes
         Max_Frame_Size  : Types.Length_24; --  bytes

         --  Ok, whoever came up with these bit assignment should get whipped.
         --  These are not even divisable by eight, so simple byte swapping
         --  won't help.

         Sample_Rate     : Types.Length_20;       --  20 bits, i.e. 2.5 bytes
         Num_Channels    : Types.Channel_Count;   --  0 .. 7 => 1 .. 8
         --  Another two nibbles crossing a byte boundary.
         Bits_Per_Sample : Types.Bits_Per_Sample; --  3 .. 31 => 4 .. 32
         Total_Samples   : Types.Sample_Count;    --  4.5 bytes.

         --  And we're back in alignment.
         MD5_Signature   : Types.MD5_Sum;
      end record
     with
       Size        => 272,
       Object_Size => 272;
   pragma Warnings (Off, "component clause forces biased representation for ""Bits_Per_Sample""");
   pragma Warnings (Off, "component clause forces biased representation for ""Num_Channels""");
   for Full_T use
      record
         Min_Block_Size  at  0 range  0 ..  15;
         Max_Block_Size  at  2 range  0 ..  15;
         Min_Frame_Size  at  4 range  0 ..  23;
         Max_Frame_Size  at  7 range  0 ..  23;
         Sample_Rate     at 10 range  0 ..  19;
         Num_Channels    at 10 range 20 ..  22;
         Bits_Per_Sample at 10 range 23 ..  27;
         Total_Samples   at 10 range 28 ..  63;
         MD5_Signature   at 18 range  0 .. 127;
      end record;
   pragma Warnings (On, "component clause forces biased representation for ""Bits_Per_Sample""");
   pragma Warnings (On, "component clause forces biased representation for ""Num_Channels""");

   function To_Full is new Ada.Unchecked_Conversion (Source => Raw_T,
                                                     Target => Full_T);
   pragma Annotate (GNATprove,
                    Intentional,
                    "type with constraints on bit representation",
                    "We need this for now.");

   procedure Convert (Source           : in     Raw_T;
                      Target           :    out T;
                      Conversion_Error :    out Boolean)
   is
      Full_View : Full_T := To_Full (S => BE_Swap (Arg => Source));
   begin
      Conversion_Error :=
        Full_View.Min_Block_Size not in Types.Block_Size or else
        Full_View.Max_Block_Size not in Types.Block_Size or else
        Full_View.Sample_Rate    not in Types.Sample_Rate;

      if not Conversion_Error then
         Target := T'(Min_Block_Size  => Full_View.Min_Block_Size,
                      Max_Block_Size  => Full_View.Max_Block_Size,
                      Min_Frame_Size  => Full_View.Min_Frame_Size,
                      Max_Frame_Size  => Full_View.Max_Frame_Size,
                      Sample_Rate     => Full_View.Sample_Rate,
                      Num_Channels    => Full_View.Num_Channels,
                      Bits_Per_Sample => Full_View.Bits_Per_Sample,
                      Total_Samples   => Full_View.Total_Samples,
                      MD5_Signature   => Full_View.MD5_Signature);
      end if;
   end Convert;

end FLAC.Headers.Stream_Info;

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
--  Very simple test program to check basic functionality.
------------------------------------------------------------------------------

with GNAT.IO;
with Flac.Reader;

procedure Simple_Test with
  SPARK_Mode => On
is
   Test_File : Flac.Reader.File_Handle;
   use type Flac.Reader.Error_Type;
   pragma Assertion_Policy (Check);
begin
   Flac.Reader.Open (File      => "doesnotexist.flac",
                     Flac_File => Test_File);

   pragma
     Assume
       (Flac.Reader.Get_Error (Handle => Test_File) = Flac.Reader.Open_Error);
   --  Expected result is an external dependency outside of SPARK.

   Flac.Reader.Open (File      => "notaflac.flac",
                     Flac_File => Test_File);

   pragma
     Assume
       (Flac.Reader.Get_Error (Handle => Test_File) = Flac.Reader.Not_A_Flac_File);
   --  Expected result is an external dependency outside of SPARK.

   Flac.Reader.Open (File      => "Unnamed.flac",
                     Flac_File => Test_File);

   pragma
     Assume
       (Flac.Reader.Get_Error (Handle => Test_File) = Flac.Reader.None);
   --  Expected result is an external dependency outside of SPARK.

   pragma Warnings (Off, "no Global contract available for ""Put_Line""");
   GNAT.IO.Put_Line
     (S => "C:   " & Flac.Reader.Num_Channels (Handle => Test_File)'Image);

   GNAT.IO.Put_Line
     (S => "BPS: " & Flac.Reader.Bits_Per_Sample (Handle => Test_File)'Image);

   GNAT.IO.Put_Line
     (S => "SR:  " & Flac.Reader.Sample_Rate (Handle => Test_File)'Image);

   GNAT.IO.Put_Line
     (S => "SC:  " & Flac.Reader.Num_Samples (Handle => Test_File)'Image);
   pragma Warnings (On, "no Global contract available for ""Put_Line""");

end Simple_Test;

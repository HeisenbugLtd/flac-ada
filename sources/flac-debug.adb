------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with GNAT.IO;

package body Flac.Debug is

   ---------------------------------------------------------------------------
   --  Print_Stream_Info
   ---------------------------------------------------------------------------
   procedure Print_Stream_Info (Handle : Flac.Reader.File_Handle)
     with
       SPARK_Mode => Off
   is
      use Flac.Reader;
   begin
      GNAT.IO.Put_Line
        (S => "C:   " & Num_Channels (Handle => Handle)'Image);

      GNAT.IO.Put_Line
        (S => "BPS: " & Bits_Per_Sample (Handle => Handle)'Image);

      GNAT.IO.Put_Line
        (S => "SR:  " & Sample_Rate (Handle => Handle)'Image);

      GNAT.IO.Put_Line
        (S => "SC:  " & Num_Samples (Handle => Handle)'Image);
   end Print_Stream_Info;

end Flac.Debug;

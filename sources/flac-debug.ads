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
--  Debug
--
--  Non-SPARK debugging stuff.
------------------------------------------------------------------------------

with Flac.Reader;

package Flac.Debug with 
  SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Print_Stream_Info
   --
   --  Print info about stream.
   ---------------------------------------------------------------------------
   procedure Print_Stream_Info (Handle : Flac.Reader.File_Handle) with
     Pre => Flac.Reader.Is_Open (Handle);

end Flac.Debug;

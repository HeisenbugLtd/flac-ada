------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

------------------------------------------------------------------------------
--  FLAC/Ada root package
------------------------------------------------------------------------------
package Flac with
  Pure       => True,
  SPARK_Mode => On
is

   type Main_Error_Type is (None, Open_Error, Not_A_Flac_File);
   --  General kind of error.

   type Sub_Error_Type is
     (None,                --  No error.
      Header_Not_Found,    --  No valid flac header
      Corrupt_Meta_Data,   --  Unexpected length of meta data
      Invalid_Meta_Data,   --  Meta data does not pan out
      Corrupt_Stream_Info, --  Expected stream info block not found
      Invalid_Stream_Info  --  Stream info block does not pan out
     );
   --  More specific error (if applicable).

   type Error_Type is
      record
         Main : Main_Error_Type;
         Sub  : Sub_Error_Type;
      end record;

   No_Error : constant Error_Type := Error_Type'(Main => None,
                                                 Sub  => None);

end Flac;

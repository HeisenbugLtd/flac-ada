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
--  Reader
--
--  Reads FLAC files.
------------------------------------------------------------------------------

private with Ada.Streams.Stream_IO;

package Flac.Reader with
  SPARK_Mode => On
is

   type Error_Type is (None, Open_Error, Not_A_Flac_File);

   type File_Handle is limited private
     with
       Default_Initial_Condition =>
         (not Is_Valid (File_Handle) and
          not Is_Open (File_Handle) and
          Get_Error (File_Handle) = None);

   ---------------------------------------------------------------------------
   --  Is_Valid
   ---------------------------------------------------------------------------
   function Is_Valid (Handle : in File_Handle) return Boolean
     with
       Ghost => True;

   ---------------------------------------------------------------------------
   --  Open
   ---------------------------------------------------------------------------
   procedure Open (File      : in     String;
                   Flac_File : in out File_Handle) with
     Pre  => not Is_Open (Handle => Flac_File),
     Post => (Is_Valid (Handle => Flac_File) and then
                  (case Get_Error (Flac_File) is
                         when None | Not_A_Flac_File =>
                           Is_Open (Handle => Flac_File),
                         when Open_Error =>
                           not Is_Open (Handle => Flac_File)));
   --  Opens a given file in FLAC format. Errors will be communicated via the
   --  returned File_Type.

   ---------------------------------------------------------------------------
   --  Close
   ---------------------------------------------------------------------------
   procedure Close (Flac_File : in out File_Handle)
     with
       Pre  =>
         Is_Valid (Handle => Flac_File) and Is_Open (Handle => Flac_File),
       Post =>
         Is_Valid (Handle => Flac_File) and not Is_Open (Handle => Flac_File);

   ---------------------------------------------------------------------------
   --  Is_Open
   ---------------------------------------------------------------------------
   function Is_Open (Handle : in File_Handle) return Boolean;

   ---------------------------------------------------------------------------
   --  Get_Error
   ---------------------------------------------------------------------------
   function Get_Error (Handle : in File_Handle) return Error_Type
     with
       Depends => (Get_Error'Result => Handle);

private
   
   type File_Handle is
      record
         File  : Ada.Streams.Stream_IO.File_Type;
         Error : Error_Type := None;
         Valid : Boolean    := False;
         Open  : Boolean    := False;
      end record;

   ---------------------------------------------------------------------------
   --  Is_Open
   ---------------------------------------------------------------------------
   function Is_Open (Handle : in File_Handle) return Boolean is
     (Handle.Open);

   ---------------------------------------------------------------------------
   --  Is_Valid
   ---------------------------------------------------------------------------
   function Is_Valid (Handle : in File_Handle) return Boolean is
     (Handle.Valid);

   ---------------------------------------------------------------------------
   --  Get_Error
   ---------------------------------------------------------------------------
   function Get_Error (Handle : in File_Handle) return Error_Type is
     (Handle.Error);

end Flac.Reader;

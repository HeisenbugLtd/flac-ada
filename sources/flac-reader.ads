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
with Interfaces;

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
                         when None =>
                           Is_Open (Handle => Flac_File),
                         when Open_Error | Not_A_Flac_File =>
                           not Is_Open (Handle => Flac_File)));
   --  FIXME: Properties need to be mentioned in the post condition.
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

   ---------------------------------------------------------------------------
   --  Num_Channels
   ---------------------------------------------------------------------------
   function Num_Channels (Handle : in File_Handle) return Natural
     with
       Depends => (Num_Channels'Result => Handle),
       Pre     => Is_Valid (Handle => Handle) and Is_Open (Handle => Handle);

   ---------------------------------------------------------------------------
   --  Bits_Per_Sample
   ---------------------------------------------------------------------------
   function Bits_Per_Sample (Handle : in File_Handle) return Natural
     with
       Depends => (Bits_Per_Sample'Result => Handle),
       Pre     => Is_Valid (Handle => Handle) and Is_Open (Handle => Handle);

   ---------------------------------------------------------------------------
   --  Sample_Rate
   ---------------------------------------------------------------------------
   function Sample_Rate (Handle : in File_Handle) return Natural
     with
       Depends => (Sample_Rate'Result => Handle),
       Pre     => Is_Valid (Handle => Handle) and Is_Open (Handle => Handle);

   ---------------------------------------------------------------------------
   --  Num_Samples
   ---------------------------------------------------------------------------
   function Num_Samples (Handle : in File_Handle) return Interfaces.Unsigned_64
     with
       Depends => (Num_Samples'Result => Handle),
       Pre     => Is_Valid (Handle => Handle) and Is_Open (Handle => Handle);

private

   type Stream_Properties is
      record
         Num_Channels    : Natural := 0; -- 1 .. 8
         Bits_Per_Sample : Natural := 0; -- 4 .. 32
         Sample_Rate     : Natural := 0; -- 1 .. 655350
         Num_Samples     : Interfaces.Unsigned_64 := 0; --  actually 36 bits
      end record;

   type File_Handle is
      record
         File       : Ada.Streams.Stream_IO.File_Type;
         --  The associated file.
         Error      : Error_Type := None;
         Valid      : Boolean    := False;
         Open       : Boolean    := False;
         --  Status information.
         Properties : Stream_Properties;
         --  FLAC properties read from the file.
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

   ---------------------------------------------------------------------------
   --  Num_Channels
   ---------------------------------------------------------------------------
   function Num_Channels (Handle : in File_Handle) return Natural is
      (Handle.Properties.Num_Channels);

   ---------------------------------------------------------------------------
   --  Bits_Per_Sample
   ---------------------------------------------------------------------------
   function Bits_Per_Sample (Handle : in File_Handle) return Natural is
     (Handle.Properties.Bits_Per_Sample);

   ---------------------------------------------------------------------------
   --  Sample_Rate
   ---------------------------------------------------------------------------
   function Sample_Rate (Handle : in File_Handle) return Natural is
     (Handle.Properties.Sample_Rate);

   ---------------------------------------------------------------------------
   --  Num_Samples
   ---------------------------------------------------------------------------
   function Num_Samples
     (Handle : in File_Handle) return Interfaces.Unsigned_64
   is
     (Handle.Properties.Num_Samples);

end Flac.Reader;

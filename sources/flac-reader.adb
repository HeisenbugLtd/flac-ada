------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Streams.Stream_IO;
with Flac.Headers;

package body Flac.Reader with
  SPARK_Mode => On
is

   package ASS renames Ada.Streams.Stream_IO;

   package IO_Wrapper with
     SPARK_Mode => On
   is
      ------------------------------------------------------------------------
      --  Is_Open
      ------------------------------------------------------------------------
      function Is_Open
        (File : in Ada.Streams.Stream_IO.File_Type) return Boolean
        with
          Ghost => True;

      ------------------------------------------------------------------------
      --  Open
      ------------------------------------------------------------------------
      procedure Open (File  :    out Ada.Streams.Stream_IO.File_Type;
                      Name  : in     String;
                      Error :    out Error_Type)
        with
          Post    => (if Error = None then Is_Open (File => File)),
          Depends => (File  => Name,
                      Error => Name);

      ------------------------------------------------------------------------
      --  Close
      ------------------------------------------------------------------------
      procedure Close (File : in out Ada.Streams.Stream_IO.File_Type)
        with
          Post    => (not Is_Open (File => File)),
          Depends => (File  => File);

      ------------------------------------------------------------------------
      --  Read
      ------------------------------------------------------------------------
      procedure Read (File : in     Ada.Streams.Stream_IO.File_Type;
                      Item :    out Ada.Streams.Stream_Element_Array;
                      Last :    out Ada.Streams.Stream_Element_Count)
        with
          Pre     => Is_Open (File => File),
          Post    => Is_Open (File => File),
          Depends => (Last => (File, Item),
                      Item => (File, Item));

   end IO_Wrapper;

   package body IO_Wrapper with
     SPARK_Mode => On
   is

      ------------------------------------------------------------------------
      --  Is_Open
      ------------------------------------------------------------------------
      function Is_Open
        (File : in Ada.Streams.Stream_IO.File_Type) return Boolean
        with
          SPARK_Mode => Off
      is
      begin
         --  function body doesn't really matter, it is just there to prove that
         --  error checks have been done before calling other stuff.
         return Ada.Streams.Stream_IO.Is_Open (File => File);
      end Is_Open;

      ------------------------------------------------------------------------
      --  Open
      ------------------------------------------------------------------------
      procedure Open (File  :    out Ada.Streams.Stream_IO.File_Type;
                      Name  : in     String;
                      Error :    out Error_Type)
        with
          SPARK_Mode => Off is
      begin
         Ada.Streams.Stream_IO.Open (File => File,
                                     Mode => Ada.Streams.Stream_IO.In_File,
                                     Name => Name);
         Error := None;
      exception
         when others =>
            Error := Open_Error;
      end Open;

      ------------------------------------------------------------------------
      --  Close
      ------------------------------------------------------------------------
      procedure Close (File : in out Ada.Streams.Stream_IO.File_Type)
        with
          SPARK_Mode => Off is
      begin
         Ada.Streams.Stream_IO.Close (File => File);
      end Close;

      ------------------------------------------------------------------------
      --  Read
      ------------------------------------------------------------------------
      procedure Read (File : in     Ada.Streams.Stream_IO.File_Type;
                      Item :    out Ada.Streams.Stream_Element_Array;
                      Last :    out Ada.Streams.Stream_Element_Count)
        with
          SPARK_Mode => Off is
      begin
         Ada.Streams.Stream_IO.Read (File => File,
                                     Item => Item,
                                     Last => Last);
      end Read;

   end IO_Wrapper;

   ---------------------------------------------------------------------------
   --  Close
   ---------------------------------------------------------------------------
   procedure Close (Flac_File : in out File_Handle) is
   begin
      IO_Wrapper.Close (Flac_File.File);
      Flac_File.Open := False;
   end Close;

   ---------------------------------------------------------------------------
   --  Open
   ---------------------------------------------------------------------------
   procedure Open (File      : in     String;
                   Flac_File : in out File_Handle) is
      Header : Ada.Streams.Stream_Element_Array (1 .. 4);
      Last   : Ada.Streams.Stream_Element_Count;

      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      --  Assume successful operation.
      IO_Wrapper.Open (File  => Flac_File.File,
                       Name  => File,
                       Error => Flac_File.Error);

      if Flac_File.Error = None then
         IO_Wrapper.Read (File => Flac_File.File,
                          Item => Header,
                          Last => Last);

         if Last /= 4 or else Header /= Flac.Headers.Stream then
            Flac_File.Error := Not_A_Flac_File;
         end if;
      end if;

      Flac_File.Open := Flac_File.Error in None | Not_A_Flac_File;
      Flac_File.Valid := True;
   end Open;

end Flac.Reader;

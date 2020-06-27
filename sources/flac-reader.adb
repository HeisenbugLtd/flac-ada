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
with Flac.Types;

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
                   Flac_File : in out File_Handle)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Header          : Headers.Four_CC;
      Last            : Ada.Streams.Stream_Element_Count;
      Meta_Data_Raw   : Headers.Meta_Data_Block_Raw;
      Stream_Info_Raw : Headers.Stream_Info_Raw;

      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Flac_File.Valid := True;

      --  Assume successful operation.
      IO_Wrapper.Open (File  => Flac_File.File,
                       Name  => File,
                       Error => Flac_File.Error);

      if Flac_File.Error = None then
         Flac_File.Open := True; --  For precondition of "Close" below.
         IO_Wrapper.Read (File => Flac_File.File,
                          Item => Header,
                          Last => Last);

         --  Check header.
         if Last /= Header'Length or else Header /= Headers.Stream then
            Close (Flac_File => Flac_File);
            Flac_File.Error := Not_A_Flac_File;
            return;
         end if;
      end if;

      if Flac_File.Error = None then
         --  Header check went fine, now we should go for the first Stream_Info
         --  meta data block.  This is mandatory according to the spec.
         IO_Wrapper.Read (File => Flac_File.File,
                          Item => Meta_Data_Raw,
                          Last => Last);

         if Last /= Meta_Data_Raw'Length then
            Close (Flac_File => Flac_File);
            Flac_File.Error := Not_A_Flac_File;
            return;
         end if;

         --  The dangerous part. We define an address overlay and figure out
         --  if the data makes sense.
         declare
            Meta_Data : Headers.Meta_Data_Block
              with
                Address => Meta_Data_Raw'Address,
                Import  => True;
            pragma Annotate (GNATprove,
                             Intentional,
                             "object with constraints on bit representation",
                             "We have to convert it somehow.");
            use type Types.Block_Type;
            use type Types.Length_24;
         begin
            if
              Meta_Data.Block_Type'Valid and then
              Meta_Data.Block_Type = Types.Stream_Info and then
              Types.BE_Swap (Meta_Data.Length) = Headers.Stream_Info_Raw'Length
            then
               IO_Wrapper.Read (File => Flac_File.File,
                                Item => Stream_Info_Raw,
                                Last => Last);

               if Last /= Stream_Info_Raw'Length then
                  Close (Flac_File => Flac_File);
                  Flac_File.Error := Not_A_Flac_File;
                  return;
               end if;

               declare
                  Stream_Info : Headers.Stream_Info
                    with
                      Address => Stream_Info_Raw'Address,
                      Import  => True;
                  pragma Annotate (GNATprove,
                                   Intentional,
                                   "object with constraints on bit representation",
                                   "We have to convert it somehow.");
               begin
                  Headers.BE_Swap (Arg => Stream_Info_Raw);
                  if
                    Stream_Info.Num_Channels'Valid    and then
                    Stream_Info.Bits_Per_Sample'Valid and then
                    Stream_Info.Sample_Rate'Valid
                  then
                     Flac_File.Num_Channels    := Positive (Stream_Info.Num_Channels);
                     Flac_File.Bits_Per_Sample := Positive (Stream_Info.Bits_Per_Sample);
                     Flac_File.Sample_Rate     := Positive (Stream_Info.Sample_Rate);
                  else
                     Close (Flac_File => Flac_File);
                     Flac_File.Error := Not_A_Flac_File;
                     return;
                  end if;
               end;
            else
               Close (Flac_File => Flac_File);
               Flac_File.Error := Not_A_Flac_File;
               return;
            end if;
         end;
      end if;

      Flac_File.Open := Flac_File.Error = None;
   end Open;

end Flac.Reader;

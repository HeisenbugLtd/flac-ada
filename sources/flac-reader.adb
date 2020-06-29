------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Streams.Stream_IO;
with Flac.Headers.Meta_Data;
with Flac.Headers.Stream_Info;
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
      function Is_Open (File : in ASS.File_Type) return Boolean
        with
          Ghost => True;

      ------------------------------------------------------------------------
      --  Open
      ------------------------------------------------------------------------
      procedure Open (File  :    out ASS.File_Type;
                      Name  : in     String;
                      Error :    out Error_Type)
        with
          Post    => (if Error = None then Is_Open (File => File)),
          Depends => (File  => Name,
                      Error => Name);

      ------------------------------------------------------------------------
      --  Close
      ------------------------------------------------------------------------
      procedure Close (File : in out ASS.File_Type)
        with
          Post    => (not Is_Open (File => File)),
          Depends => (File  => File);

      ------------------------------------------------------------------------
      --  Read
      ------------------------------------------------------------------------
      procedure Read (File : in     ASS.File_Type;
                      Item :    out Ada.Streams.Stream_Element_Array;
                      Last :    out Ada.Streams.Stream_Element_Count)
        with
          Pre     => Is_Open (File => File),
          Post    => Is_Open (File => File),
          Depends => (Last => (File, Item),
                      Item => (File, Item));

      ------------------------------------------------------------------------
      --  Skip
      ------------------------------------------------------------------------
      procedure Skip (File         : in ASS.File_Type;
                      Num_Elements : in ASS.Count)
        with
          Pre     => Is_Open (File => File),
          Post    => Is_Open (File => File);

   end IO_Wrapper;

   package body IO_Wrapper with
     SPARK_Mode => On
   is

      ------------------------------------------------------------------------
      --  Is_Open
      ------------------------------------------------------------------------
      function Is_Open (File : in ASS.File_Type) return Boolean
        with
          SPARK_Mode => Off
      is
      begin
         --  function body doesn't really matter, it is just there to prove that
         --  error checks have been done before calling other stuff.
         return ASS.Is_Open (File => File);
      end Is_Open;

      ------------------------------------------------------------------------
      --  Open
      ------------------------------------------------------------------------
      procedure Open (File  :    out ASS.File_Type;
                      Name  : in     String;
                      Error :    out Error_Type)
        with
          SPARK_Mode => Off is
      begin
         ASS.Open (File => File,
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
      procedure Close (File : in out ASS.File_Type)
        with
          SPARK_Mode => Off is
      begin
         ASS.Close (File => File);
      end Close;

      ------------------------------------------------------------------------
      --  Read
      ------------------------------------------------------------------------
      procedure Read (File : in     ASS.File_Type;
                      Item :    out Ada.Streams.Stream_Element_Array;
                      Last :    out Ada.Streams.Stream_Element_Count)
        with
          SPARK_Mode => Off is
      begin
         ASS.Read (File => File,
                   Item => Item,
                   Last => Last);
      end Read;

      ------------------------------------------------------------------------
      --  Skip
      ------------------------------------------------------------------------
      procedure Skip (File         : in ASS.File_Type;
                      Num_Elements : in ASS.Count)
        with
          SPARK_Mode => Off
      is
         use type Ada.Streams.Stream_IO.Count;
      begin
         ASS.Set_Index (File => File,
                        To   => ASS.Index (File => File) + Num_Elements);
      end Skip;

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
      Meta_Data_Raw   : Headers.Meta_Data.Raw_T;
      Stream_Info_Raw : Headers.Stream_Info.Raw_T;

      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Flac_File.Valid := True;

      --  Assume successful operation.
      IO_Wrapper.Open (File  => Flac_File.File,
                       Name  => File,
                       Error => Flac_File.Error);

      if Flac_File.Error /= None then
         return;
      end if;

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

      --  Let's figure out if the data makes sense.
      declare
         Meta_Data        : Headers.Meta_Data.T;
         Conversion_Error : Boolean;
         use type Types.Block_Type;
         use type Types.Length_24;
      begin
         Headers.Meta_Data.Convert (Source           => Meta_Data_Raw,
                                    Target           => Meta_Data,
                                    Conversion_Error => Conversion_Error);

         if
           not Conversion_Error and then
           Meta_Data.Block_Type = Types.Stream_Info and then
           Meta_Data.Length = Headers.Stream_Info.Raw_T'Length
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
               Stream_Info : Headers.Stream_Info.T;
            begin
               Headers.Stream_Info.Convert
                 (Source           => Stream_Info_Raw,
                  Target           => Stream_Info,
                  Conversion_Error => Conversion_Error);

               if Conversion_Error then
                  Close (Flac_File => Flac_File);
                  Flac_File.Error := Not_A_Flac_File;
                  return;
               end if;

               Flac_File.Properties :=
                 Stream_Properties'
                   (Num_Channels    => Positive (Stream_Info.Num_Channels),
                    Bits_Per_Sample => Positive (Stream_Info.Bits_Per_Sample),
                    Sample_Rate     => Positive (Stream_Info.Sample_Rate),
                    Num_Samples     => Interfaces.Unsigned_64 (Stream_Info.Total_Samples));
            end;
         else
            Close (Flac_File => Flac_File);
            Flac_File.Error := Not_A_Flac_File;
            return;
         end if;

         --   There may be more meta data blocks.  For now, we just skip them.
         Skip_All_Meta_Data :
         declare
            use type Ada.Streams.Stream_IO.Count;
         begin
            while not Meta_Data.Last loop
               pragma Loop_Invariant (Get_Error (Handle => Flac_File) = None);
               IO_Wrapper.Read (File => Flac_File.File,
                                Item => Meta_Data_Raw,
                                Last => Last);

               if Last /= Meta_Data_Raw'Length then
                  Close (Flac_File => Flac_File);
                  Flac_File.Error := Not_A_Flac_File;
                  return;
               end if;

               Headers.Meta_Data.Convert (Source           => Meta_Data_Raw,
                                          Target           => Meta_Data,
                                          Conversion_Error => Conversion_Error);

               if
                 not Conversion_Error and then
                 Meta_Data.Block_Type /= Types.Invalid
               then
                  IO_Wrapper.Skip (File         => Flac_File.File,
                                   Num_Elements => ASS.Count (Meta_Data.Length));
               else
                  Close (Flac_File => Flac_File);
                  Flac_File.Error := Not_A_Flac_File;
                  return;
               end if;
            end loop;
         end Skip_All_Meta_Data;
      end;

      Flac_File.Open := Flac_File.Error = None;
   end Open;

end Flac.Reader;

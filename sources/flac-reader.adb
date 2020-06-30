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
with SPARK_Stream_IO;

package body Flac.Reader with
  SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Local helper subroutines.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Validate_Header
   --
   --  Tries reading the first block of data from the file stream and checks
   --  if a valid flac file signature could be found.
   ---------------------------------------------------------------------------
   procedure Validate_Header (Flac_File : in out File_Handle)
     with
       Pre  => (Is_Open (Handle => Flac_File) and then
                Flac_File.Error = No_Error),
       Post => (case Flac_File.Error.Main is
                  when None => Is_Open (Handle => Flac_File),
                  when others => not Is_Open (Handle => Flac_File)),
       Depends => (Flac_File => Flac_File);

   ---------------------------------------------------------------------------
   --  Read_Metadata_Block
   ---------------------------------------------------------------------------
   procedure Read_Metadata_Block (Flac_File : in out File_Handle;
                                  Meta_Data :    out Headers.Meta_Data.T)
     with
       Relaxed_Initialization => Meta_Data,
       Pre     => (Is_Open (Handle => Flac_File) and then
                   Flac_File.Error = No_Error),
       Post    => (case Flac_File.Error.Main is
                     when None =>
                       Is_Open (Handle => Flac_File) and then
                       Meta_Data'Initialized,
                     when others =>
                       not Is_Open (Handle => Flac_File)),
       Depends => (Flac_File => Flac_File,
                   Meta_Data => Flac_File);

   ---------------------------------------------------------------------------
   --  Read_Stream_Info
   --
   --  Reads basic stream info from current position.
   --  Requires to have read a meta data block with Info = Stream_Info before.
   ---------------------------------------------------------------------------
   procedure Read_Stream_Info (Flac_File : in out File_Handle;
                               Meta_Data :    out Headers.Meta_Data.T)
     with
       Relaxed_Initialization => Meta_Data,
       Pre     => (Is_Open (Handle => Flac_File) and then
                   Flac_File.Error = No_Error),
       Post    => (case Flac_File.Error.Main is
                     when None =>
                       Is_Open (Handle => Flac_File) and then
                       Meta_Data'Initialized,
                     when others =>
                       not Is_Open (Handle => Flac_File)),
       Depends => (Flac_File => Flac_File,
                   Meta_Data => Flac_File);

   ---------------------------------------------------------------------------
   --  Read_Metadata_Block
   ---------------------------------------------------------------------------
   procedure Read_Metadata_Block (Flac_File : in out File_Handle;
                                  Meta_Data :    out Headers.Meta_Data.T)
   is
      Meta_Data_Raw : Headers.Meta_Data.Raw_T;
      Error         : Boolean;
   begin
      SPARK_Stream_IO.Read (File  => Flac_File.File,
                            Item  => Meta_Data_Raw,
                            Error => Error);

      if Error then
         Close (Flac_File => Flac_File);
         Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                        Sub  => Corrupt_Meta_Data);
         return;
      end if;

      --  Let's figure out if the data makes sense.
      Headers.Meta_Data.Convert (Source           => Meta_Data_Raw,
                                 Target           => Meta_Data,
                                 Conversion_Error => Error);

      if Error then
         Close (Flac_File => Flac_File);
         Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                        Sub  => Invalid_Meta_Data);
      end if;
   end Read_Metadata_Block;

   ---------------------------------------------------------------------------
   --  Read_Stream_Info
   ---------------------------------------------------------------------------
   procedure Read_Stream_Info (Flac_File : in out File_Handle;
                               Meta_Data :    out Headers.Meta_Data.T)
   is
      Stream_Info_Raw : Headers.Stream_Info.Raw_T;
      Stream_Info     : Headers.Stream_Info.T;
      Error           : Boolean;
      use type Types.Block_Type;
      use type Types.Length_24;
   begin
      Read_Metadata_Block (Flac_File => Flac_File,
                           Meta_Data => Meta_Data);

      if Flac_File.Error.Main /= None then
         return;
      end if;

      if
        Meta_Data.Block_Type /= Types.Stream_Info or else
        Meta_Data.Length /= Headers.Stream_Info.Raw_T'Length
      then
         Close (Flac_File => Flac_File);
         Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                        Sub  => Invalid_Meta_Data);
         return;
      end if;

      SPARK_Stream_IO.Read (File  => Flac_File.File,
                            Item  => Stream_Info_Raw,
                            Error => Error);

      if Error then
         Close (Flac_File => Flac_File);
         Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                        Sub  => Corrupt_Stream_Info);
         return;
      end if;

      Headers.Stream_Info.Convert
        (Source           => Stream_Info_Raw,
         Target           => Stream_Info,
         Conversion_Error => Error);

      if Error then
         Close (Flac_File => Flac_File);
         Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                        Sub  => Invalid_Stream_Info);
         return;
      end if;

      Flac_File.Properties :=
        Stream_Properties'
          (Num_Channels    => Positive (Stream_Info.Num_Channels),
           Bits_Per_Sample => Positive (Stream_Info.Bits_Per_Sample),
           Sample_Rate     => Positive (Stream_Info.Sample_Rate),
           Num_Samples     => Interfaces.Unsigned_64 (Stream_Info.Total_Samples));
   end Read_Stream_Info;

   ---------------------------------------------------------------------------
   --  Validate_Header
   ---------------------------------------------------------------------------
   procedure Validate_Header (Flac_File : in out File_Handle)
   is
      Header : Headers.Four_CC;
      Error  : Boolean;
      use type Ada.Streams.Stream_Element_Array;
   begin
      SPARK_Stream_IO.Read (File  => Flac_File.File,
                            Item  => Header,
                            Error => Error);

      --  Check header.
      if Error or else Header /= Headers.Stream then
         Close (Flac_File => Flac_File);
         Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                        Sub  => Header_Not_Found);
      end if;
   end Validate_Header;

   ---------------------------------------------------------------------------
   --  Close
   ---------------------------------------------------------------------------
   procedure Close (Flac_File : in out File_Handle) is
   begin
      SPARK_Stream_IO.Close (Flac_File.File);
      Flac_File.Open := False;
   end Close;

   ---------------------------------------------------------------------------
   --  Open
   ---------------------------------------------------------------------------
   procedure Open (File      : in     String;
                   Flac_File : in out File_Handle)
   is
      Meta_Data : Headers.Meta_Data.T;
      Error     : Boolean;
   begin
      --  Try opening the actual file.
      SPARK_Stream_IO.Open (File  => Flac_File.File,
                            Name  => File,
                            Error => Error);

      if Error then
         Flac_File.Error := Error_Type'(Main => Open_Error,
                                        Sub  => None);
         return;
      end if;

      Flac_File.Open  := True; --  For precondition of "Close" below.
      Flac_File.Error := No_Error;

      Validate_Header (Flac_File => Flac_File);

      if Flac_File.Error /= No_Error then
         return;
      end if;

      --  Header check went fine, now we should go for the first Stream_Info
      --  meta data block.  This is mandatory according to the spec.
      Read_Stream_Info (Flac_File => Flac_File,
                        Meta_Data => Meta_Data);

      if Flac_File.Error /= No_Error then
         return;
      end if;

      --  There may be more meta data blocks.  For now, we just skip them.
      Skip_All_Meta_Data :
      declare
         use type Ada.Streams.Stream_IO.Count;
         use type Types.Block_Type;
      begin
         while not Meta_Data.Last loop
            pragma
              Loop_Invariant
                (Get_Error (Handle => Flac_File) = No_Error and then
                 Is_Open (Handle => Flac_File));

            Read_Metadata_Block (Flac_File => Flac_File,
                                 Meta_Data => Meta_Data);

            if Flac_File.Error /= No_Error then
               return;
            end if;

            if Meta_Data.Block_Type = Types.Invalid then
               Close (Flac_File => Flac_File);
               Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                              Sub  => Invalid_Meta_Data);
               return;
            end if;

            SPARK_Stream_IO.Skip
              (File         => Flac_File.File,
               Num_Elements => Ada.Streams.Stream_IO.Count (Meta_Data.Length),
               Error        => Error);

            if Error then
               Close (Flac_File => Flac_File);
               Flac_File.Error := Error_Type'(Main => Not_A_Flac_File,
                                              Sub  => Corrupt_Meta_Data);
               return;
            end if;
         end loop;
      end Skip_All_Meta_Data;
   end Open;

end Flac.Reader;

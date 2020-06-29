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
      use type Ada.Streams.Stream_Element_Offset;

      Header          : Headers.Four_CC;
      Meta_Data_Raw   : Headers.Meta_Data.Raw_T;
      Stream_Info_Raw : Headers.Stream_Info.Raw_T;
      Error           : Boolean;

      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Flac_File.Valid := True;

      --  Assume successful operation.
      SPARK_Stream_IO.Open (File  => Flac_File.File,
                            Name  => File,
                            Error => Error);

      if Error then
         Flac_File.Error := Open_Error;
         return;
      end if;

      Flac_File.Open  := True; --  For precondition of "Close" below.
      Flac_File.Error := None;
      SPARK_Stream_IO.Read (File  => Flac_File.File,
                            Item  => Header,
                            Error => Error);

      --  Check header.
      if Error or else Header /= Headers.Stream then
         Close (Flac_File => Flac_File);
         Flac_File.Error := Not_A_Flac_File;
         return;
      end if;

      --  Header check went fine, now we should go for the first Stream_Info
      --  meta data block.  This is mandatory according to the spec.
      SPARK_Stream_IO.Read (File  => Flac_File.File,
                            Item  => Meta_Data_Raw,
                            Error => Error);

      if Error then
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
            SPARK_Stream_IO.Read (File  => Flac_File.File,
                                  Item  => Stream_Info_Raw,
                                  Error => Error);

            if Error then
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

         --  There may be more meta data blocks.  For now, we just skip them.
         Skip_All_Meta_Data :
         declare
            use type Ada.Streams.Stream_IO.Count;
         begin
            while not Meta_Data.Last loop
               pragma Loop_Invariant (Get_Error (Handle => Flac_File) = None);
               SPARK_Stream_IO.Read (File  => Flac_File.File,
                                     Item  => Meta_Data_Raw,
                                     Error => Error);

               if Error then
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
                  SPARK_Stream_IO.Skip
                    (File         => Flac_File.File,
                     Num_Elements =>
                       Ada.Streams.Stream_IO.Count (Meta_Data.Length),
                     Error        => Error);

                  if Error then
                     Close (Flac_File => Flac_File);
                     Flac_File.Error := Not_A_Flac_File;
                     return;
                  end if;
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

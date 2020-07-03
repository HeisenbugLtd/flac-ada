------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Text_IO;

package body Flac.Debug with
  SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Print_Stream_Info
   ---------------------------------------------------------------------------
   procedure Print_Stream_Info (Handle : Flac.Reader.File_Handle)
     with
       SPARK_Mode => Off
   is
      use Flac.Reader;
   begin
      Ada.Text_IO.Put_Line
        (Item => "Channels(S)    :" & Num_Channels (Handle => Handle)'Image);
      Ada.Text_IO.Put_Line
        (Item => "Sample_Size(S) :" & Bits_Per_Sample (Handle => Handle)'Image);
      Ada.Text_IO.Put_Line
        (Item => "Sample_Rate(S) :" & Sample_Rate (Handle => Handle)'Image);
      Ada.Text_IO.Put_Line
        (Item => "Sample_Count(S):" & Num_Samples (Handle => Handle)'Image);
   end Print_Stream_Info;

   ---------------------------------------------------------------------------
   --  Print_Frame_Info
   --
   --  Print info about frame.
   ---------------------------------------------------------------------------
   procedure Print_Frame_Info (Frame : in Frames.T)
     with
       SPARK_Mode => Off
   is
   begin
      Ada.Text_IO.Put_Line
        (Item => "Blocking_Strategy(F) : " & Frame.Blocking_Strategy'Image);
      Ada.Text_IO.Put_Line
        (Item => "Block_Size(F)        :" & Frame.Block_Size'Image);
      Ada.Text_IO.Put_Line
        (Item => "Sample_Rate(F)       :" & Frame.Sample_Rate'Image);
      Ada.Text_IO.Put_Line
        (Item => "Channel_Assignment(F):" & Frame.Channel_Assignment'Image);
      Ada.Text_IO.Put_Line
        (Item => "Sample_Size(F)       :" & Frame.Sample_Size'Image);
   end Print_Frame_Info;

end Flac.Debug;

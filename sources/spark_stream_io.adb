------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+flacada@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPARK_Stream_IO with
  SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Is_Open
   ---------------------------------------------------------------------------
   function Is_Open (File : in ASS.File_Type) return Boolean
     with
       SPARK_Mode => Off
   is
   begin
      --  function body doesn't really matter, it is just there to prove that
      --  error checks have been done before calling other stuff.
      return ASS.Is_Open (File => File);
   end Is_Open;

   ---------------------------------------------------------------------------
   --  Open
   ---------------------------------------------------------------------------
   procedure Open (File  :    out ASS.File_Type;
                   Name  : in     String;
                   Error :    out Boolean)
     with
       SPARK_Mode => Off is
   begin
      ASS.Open (File => File,
                Mode => Ada.Streams.Stream_IO.In_File,
                Name => Name);
      Error := False;
   exception
      when others =>
         Error := True;
   end Open;

   ---------------------------------------------------------------------------
   --  Close
   ---------------------------------------------------------------------------
   procedure Close (File : in out ASS.File_Type)
     with
       SPARK_Mode => Off is
   begin
      ASS.Close (File => File);
   end Close;

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File  : in     ASS.File_Type;
                   Item  :    out Ada.Streams.Stream_Element_Array;
                   Error :    out Boolean)
     with
       SPARK_Mode => Off
   is
      Last : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      ASS.Read (File => File,
                Item => Item,
                Last => Last);
      
      Error := Last /= Item'Length;
   end Read;

   ---------------------------------------------------------------------------
   --  Skip
   ---------------------------------------------------------------------------
   procedure Skip (File         : in     ASS.File_Type;
                   Num_Elements : in     ASS.Count;
                   Error        :    out Boolean)
     with
       SPARK_Mode => Off
   is
      use type Ada.Streams.Stream_IO.Count;
   begin
      ASS.Set_Index (File => File,
                     To   => ASS.Index (File => File) + Num_Elements);
      Error := False;
   exception
      when others =>
         Error := True;
   end Skip;

end SPARK_Stream_IO;

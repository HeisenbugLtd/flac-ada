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
--  SPARK_Stream_IO
--
--  Very simple wrapper around Ada.Streams.Stream_IO.
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;

package SPARK_Stream_IO with
  SPARK_Mode => On
is

   package ASS renames Ada.Streams.Stream_IO;

   ---------------------------------------------------------------------------
   --  Is_Open
   ---------------------------------------------------------------------------
   function Is_Open (File : in ASS.File_Type) return Boolean;

   ---------------------------------------------------------------------------
   --  Open
   ---------------------------------------------------------------------------
   procedure Open (File  :    out ASS.File_Type;
                   Name  : in     String;
                   Error :    out Boolean)
     with
       Post    => (Is_Open (File => File) = not Error),
       Depends => (File  => Name,
                   Error => Name);

   ---------------------------------------------------------------------------
   --  Close
   ---------------------------------------------------------------------------
   procedure Close (File : in out ASS.File_Type)
     with
       Pre     => Is_Open (File => File),
       Post    => (not Is_Open (File => File)),
       Depends => (File => File);

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (File  : in     ASS.File_Type;
                   Item  :    out Ada.Streams.Stream_Element_Array;
                   Error :    out Boolean)
     with
       Relaxed_Initialization => Item,
       Pre     => Is_Open (File => File),
       Post    => (Is_Open (File => File) and then
                     (if not Error then Item'Initialized)),
       Depends => (Error => (File, Item),
                   Item  => (File, Item));

   ---------------------------------------------------------------------------
   --  Skip
   ---------------------------------------------------------------------------
   procedure Skip (File         : in     ASS.File_Type;
                   Num_Elements : in     ASS.Count;
                   Error        :    out Boolean)
     with
       Pre     => Is_Open (File => File),
       Post    => Is_Open (File => File),
       Depends => (Error => (File, Num_Elements));

end SPARK_Stream_IO;

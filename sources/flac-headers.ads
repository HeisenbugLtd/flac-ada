with Flac.Types;
with System.Storage_Elements;

package Flac.Headers is

   package SSE renames System.Storage_Elements;

   --  Stream header definitions.
   --  STREAM
   --  <32> 	"fLaC", the FLAC stream marker in ASCII, meaning byte 0 of the stream is 0x66, followed by 0x4C 0x61 0x43
   --  METADATA_BLOCK 	This is the mandatory STREAMINFO metadata block that has the basic properties of the stream
   --  METADATA_BLOCK* 	Zero or more metadata blocks
   --  FRAME+ 	One or more audio frames    

   Stream : constant SSE.Storage_Array := (Character'Pos ('f'),
                                           Character'Pos ('L'),
                                           Character'Pos ('a'),
                                           Character'Pos ('C'));

   type Meta_Data_Block is
      record
         Last       : Boolean;
         Block_Type : Types.Block_Type;
         Length     : Types.Length_24;
      end record
     with
       Size => 32;
   for Meta_Data_Block use
      record
         Last       at 0 range 0 .. 0;
         Block_Type at 0 range 1 .. 7;
         Length     at 1 range 0 .. 23;
      end record;

end Flac.Headers;

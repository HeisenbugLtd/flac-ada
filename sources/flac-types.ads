package Flac.Types is

   -- Basic types.

   type Length_24 is range 0 .. 2 ** 24 - 1
     with
       Size => 24;

   --  BLOCK_TYPE
   --
   --      0 : STREAMINFO
   --      1 : PADDING
   --      2 : APPLICATION
   --      3 : SEEKTABLE
   --      4 : VORBIS_COMMENT
   --      5 : CUESHEET
   --      6 : PICTURE
   --      7-126 : reserved
   --      127 : invalid, to avoid confusion with a frame sync code
   type Block_Type is (Stream_Info,
                       Padding,
                       Application,
                       Seek_Table,
                       Vorbis_Comment,
                       Cue_Sheet,
                       Picture,
                       Reserved,
                       Invalid);
   for Block_Type use (Stream_Info => 0,
                       Padding     => 1,
                       Application => 2,
                       Seek_Table  => 3,
                       Vorbis_Comment => 4,
                       Cue_Sheet      => 5,
                       Picture        => 6,
                       Reserved       => 7,
                       Invalid        => 127);

end Flac.Types;

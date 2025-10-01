structure Posix_IO_Extra =
struct
  open Posix.IO

  fun readVecText (fd, n) =
    let
      val TextPrimIO.RD reader =
        mkTextReader {fd = fd, name = "", initBlkMode = true}
    in
      Option.valOf (#readVec reader) n
    end

  fun readArrText (fd, x) =
    let
      val TextPrimIO.RD reader =
        mkTextReader {fd = fd, name = "", initBlkMode = true}
    in
      Option.valOf (#readArr reader) x
    end

  fun writeVecText (fd, x) =
    let
      val TextPrimIO.WR writer = mkTextWriter
        { fd = fd
        , name = ""
        , initBlkMode = true
        , appendMode = false
        , chunkSize = 0
        }
    in
      Option.valOf (#writeVec writer) x
    end

  fun writeArrText (fd, x) =
    let
      val TextPrimIO.WR writer = mkTextWriter
        { fd = fd
        , name = ""
        , initBlkMode = true
        , appendMode = false
        , chunkSize = 0
        }
    in
      Option.valOf (#writeArr writer) x
    end
end

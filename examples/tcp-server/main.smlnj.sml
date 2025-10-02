structure Main =
struct
  fun main _ =
    ( UnixSignals.setHandler (UnixSignals.sigPIPE, UnixSignals.IGNORE)
    ; RunCML.doit (Server.cmlMain, NONE)
    ; OS.Process.success
    )
end

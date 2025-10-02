val _ =
  let
    open MLton.Signal
  in
    setHandler (Posix.Signal.pipe, Handler.ignore);
    RunCML.doit (Server.cmlMain, NONE)
  end

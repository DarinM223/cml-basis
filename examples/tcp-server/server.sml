structure Server =
struct
  type passive_socket =
    (INetSock.inet, Socket.passive Socket.stream) Socket.sock

  fun connMain sock =
    CML.select
      [ CML.wrap (Socket.recvVecEvt (sock, 1000), fn vec =>
          let
            val _ = Socket.sendVec
              ( sock
              , Word8VectorSlice.full (Byte.stringToBytes "HTTP/1.1 200 OK\n\n")
              )
            val _ = Socket.sendVec (sock, Word8VectorSlice.full vec)
          in
            Socket.close sock
          end)
      , CML.wrap (CML.timeOutEvt (Time.fromSeconds 5), fn () =>
          Socket.close sock)
      ]
    handle _ => Socket.close sock

  fun acceptLoop sock =
    let val (sock', _) = CML.sync (Socket.acceptEvt sock)
    in CML.spawn (fn () => connMain sock'); acceptLoop sock
    end

  fun cmlMain () =
    let
      val sock: passive_socket = INetSock.TCP.socket ()
    in
      Socket.Ctl.setREUSEADDR (sock, true);
      Socket.bind (sock, INetSock.any 8989);
      Socket.listen (sock, 5);
      acceptLoop sock
    end
end

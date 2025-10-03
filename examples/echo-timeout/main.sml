(* Tests readArrEvt and readVecEvt from OSTextPrimIO *)

val
  TextPrimIO.RD
    {readArrEvt = readStdinArrEvt, readVecEvt = readStdinVecEvt, ...} =
  OSTextPrimIO.stdIn ()

fun echoOrTimeoutLoopVec () =
  let
    val () = TextIO.print "Start of loop:\n"
  in
    CML.select
      [ CML.wrap (readStdinVecEvt 1000, fn s =>
          TextIO.print ("Read string: " ^ s ^ "\n"))
      , CML.wrap (CML.timeOutEvt (Time.fromSeconds 3), fn () =>
          TextIO.print "Timed out\n")
      ];
    echoOrTimeoutLoopVec ()
  end

fun echoOrTimeoutLoopArr () =
  let
    val () = TextIO.print "Start of loop:\n"
    val slice = CharArraySlice.full (CharArray.array (1000, #"\n"))
  in
    CML.select
      [ CML.wrap (readStdinArrEvt slice, fn n =>
          let
            val s = CharArraySlice.vector
              (CharArraySlice.subslice (slice, 0, SOME n))
          in
            TextIO.print ("Read string: " ^ s ^ "\n")
          end)
      , CML.wrap (CML.timeOutEvt (Time.fromSeconds 3), fn () =>
          TextIO.print "Timed out\n")
      ];
    echoOrTimeoutLoopArr ()
  end

fun cmlMain () =
  echoOrTimeoutLoopArr ()
  handle e =>
    TextIO.print
      ("Exception name: " ^ exnName e ^ " message: " ^ exnMessage e ^ "\n")

val _ = RunCML.doit (cmlMain, NONE)

(* posix-text-prim-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This implements the UNIX version of the OS specific text primitive
 * IO structure.  It is implemented by a trivial translation of the
 * binary operations (see posix-bin-prim-io.sml).
 *)

structure PosixTextPrimIO : sig

    include OS_PRIM_IO

    val stdIn  : unit -> PrimIO.reader
    val stdOut : unit -> PrimIO.writer
    val stdErr : unit -> PrimIO.writer

    val strReader : string -> PrimIO.reader

  end = struct

    structure SV = SyncVar
    structure PF = Posix.FileSys
    structure BinPrimIO = PosixBinPrimIO
    structure PrimIO = TextPrimIO
		structure PIO = Posix_IO_Extra

    type file_desc = PF.file_desc

    val bufferSzB = 4096

 (* If Char.char is really Word8.word, then very efficient versions of
  * translateIn and translateOut are possible:
  *)
    (* val translateIn : BinPrimIO.PrimIO.reader -> PrimIO.reader = Unsafe.cast
    val translateOut : BinPrimIO.PrimIO.writer -> PrimIO.writer = Unsafe.cast

    fun openRd fname = translateIn(BinPrimIO.openRd fname)
    fun openWr fname = translateOut(BinPrimIO.openWr fname)
    fun openApp fname = translateOut(BinPrimIO.openApp fname)

    fun mkReader args = translateIn(BinPrimIO.mkReader args)
    fun mkWriter args = translateOut(BinPrimIO.mkWriter args) *)

    fun isRegFile fd = PF.ST.isReg(PF.fstat fd)

    fun posFns (closed, fd) = if (isRegFile fd)
	  then let
	    val pos = ref(Position.fromInt 0)
	    fun getPos () = !pos
	    fun setPos p = (
		  if !closed then raise IO.ClosedStream else ();
		  pos := PIO.lseek(fd,p,PIO.SEEK_SET))
	    fun endPos () = (
		  if !closed then raise IO.ClosedStream else ();
		  PF.ST.size(PF.fstat fd))
	    fun verifyPos () = let
		  val curPos = PIO.lseek(fd, Position.fromInt 0, PIO.SEEK_CUR)
		  in
		    pos := curPos; curPos
		  end
	    in
	      ignore (verifyPos());
	      { pos = pos,
		getPos = SOME getPos,
		setPos = SOME setPos,
		endPos = SOME endPos,
		verifyPos = SOME verifyPos
	      }
	    end
	  else {
	      pos = ref(Position.fromInt 0),
	      getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE
	    }

    fun mkReader {fd, name} = let
	  val iod = PF.fdToIOD fd
	  val lockMV = SV.mVarInit()
	  fun withLock f x = (
		SV.mTake lockMV;
		(Syscall.doSyscall f x) before SV.mPut(lockMV, ()))
		  handle ex => (SV.mPut(lockMV, ()); raise ex)
	  fun withLock' NONE = NONE
	    | withLock' (SOME f) = SOME(withLock f)
	  val closed = ref false
          val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, fd)
	  fun incPos k = pos := Position.+(!pos, Position.fromInt k)
	  fun blockWrap f x = (
		if !closed then raise IO.ClosedStream else ();
		f x)
	  val readEvt =
		IOManager.ioEvt(OS.IO.pollIn(Option.valOf(OS.IO.pollDesc iod)))
	  fun eventWrap f x = CML.withNack (fn nack => (
		if !closed then raise IO.ClosedStream else ();
		case (SV.mTakePoll lockMV)
		 of NONE => let
		      val replV = SV.iVar()
		      in
			CML.spawn(fn () => CML.select [
			    CML.wrap (readEvt, fn _ => SV.iPut(replV, ())),
			    nack
			  ]);
			CML.wrap(SV.iGetEvt replV, fn _ => f x)
		      end
		  | (SOME _) => CML.wrap (readEvt,
			fn _ => (SV.mPut(lockMV, ()); f x))
		(* end case *)))
	  fun readVec n = let
		val _ = CML.sync readEvt
		val v = PIO.readVecText(fd, n)
		in
		  incPos (String.size v); v
		end
	  fun readArr arg = let
		val _ = CML.sync readEvt
		val k = PIO.readArrText(fd, arg)
		in
		  incPos k; k
		end
	  fun close () = if !closed
		then ()
		else (closed:=true; PIO.close fd)
	  val isReg = isRegFile fd
	  fun avail () = if !closed
		  then SOME 0
		else if isReg
		  then SOME(PF.ST.size(PF.fstat fd) - !pos)
		  else NONE
	  in
	    TextPrimIO.RD{
		name		= name,
		chunkSize	= bufferSzB,
		readVec		= withLock (blockWrap readVec),
		readArr		= withLock (blockWrap readArr),
		readVecEvt	= eventWrap readVec,
		readArrEvt	= eventWrap readArr,
		avail		= withLock avail,
		getPos		= withLock' getPos,
		setPos		= withLock' setPos,
		endPos		= withLock' endPos,
		verifyPos	= withLock' verifyPos,
		close		= withLock close,
		ioDesc		= SOME iod
	      }
	  end

    fun openRd name = mkReader{
	    fd = PF.openf(name, PIO.O_RDONLY, PF.O.flags[]),
	    name = name
	  }

    fun mkWriter {fd, name, appendMode, chunkSize} = let
	  val iod = PF.fdToIOD fd
	  val lockMV = SV.mVarInit()
	  fun withLock f x = (
		SV.mTake lockMV;
		(Syscall.doSyscall f x) before SV.mPut(lockMV, ()))
		  handle ex => (SV.mPut(lockMV, ()); raise ex)
	  fun withLock' NONE = NONE
	    | withLock' (SOME f) = SOME(withLock f)
	  val closed = ref false
	  val appendFS = PIO.O.flags(if appendMode then [PIO.O.append] else [])
	  fun updateStatus() = PIO.setfl(fd, appendFS)
	  fun ensureOpen () = if !closed then raise IO.ClosedStream else ()
	  fun putV x = PIO.writeVecText x
	  fun putA x = PIO.writeArrText x
	  fun write put arg = (ensureOpen(); put(fd, arg))
	  val writeEvt =
		IOManager.ioEvt(OS.IO.pollOut(Option.valOf(OS.IO.pollDesc iod)))
	  fun eventWrap f x = CML.withNack (fn nack => (
		if !closed then raise IO.ClosedStream else ();
		case (SV.mTakePoll lockMV)
		 of NONE => let
		      val replV = SV.iVar()
		      in
			CML.spawn(fn () => CML.select [
			    CML.wrap (writeEvt, fn _ => SV.iPut(replV, ())),
			    nack
			  ]);
			CML.wrap(SV.iGetEvt replV, fn _ => f x)
		      end
		  | (SOME _) => CML.wrap (writeEvt,
			fn _ => (SV.mPut(lockMV, ()); f x))
		(* end case *)))
	  fun close () = if !closed
		then ()
		else (closed:=true; PIO.close fd)
          val {pos, getPos, setPos, endPos, verifyPos} = posFns (closed, fd)
	  in
	    TextPrimIO.WR{
		name		= name,
		chunkSize	= chunkSize,
		writeVec	= withLock (write putV),
		writeArr	= withLock (write putA),
		writeVecEvt	= eventWrap (write putV),
		writeArrEvt	= eventWrap (write putA),
		getPos		= withLock' getPos,
		setPos		= withLock' setPos,
		endPos		= withLock' endPos,
		verifyPos	= withLock' verifyPos,
		close		= withLock close,
		ioDesc		= SOME iod
	      }
	  end

    val standardMode = PF.S.flags[	(* mode 0666 *)
	    PF.S.irusr, PF.S.iwusr,
	    PF.S.irgrp, PF.S.iwgrp,
	    PF.S.iroth, PF.S.iwoth
	  ]
    fun createFile (name, mode, flags) =
	  PF.createf(name, mode, flags, standardMode)

    fun openWr name = mkWriter{
	    fd=createFile(name, PIO.O_WRONLY, PF.O.trunc),
	    name=name,
	    appendMode=false,
	    chunkSize=bufferSzB
	  }

    fun openApp name = mkWriter{
	    fd		= createFile(name, PIO.O_WRONLY, PF.O.append),
	    name	= name,
	    appendMode	= true,
	    chunkSize	= bufferSzB
	  }

		(* Extra PosixTextPrimIO stuff *)

    fun stdIn () = mkReader{
	    fd		= PF.stdin,
	    name	= "<stdIn>"
	  }

    fun stdOut () = mkWriter{
	    fd		= PF.stdout,
	    name	= "<stdOut>",
	    appendMode	= false, (* Bug!  Should check! *)
	    chunkSize	= bufferSzB
	  }

    fun stdErr () = mkWriter{
	    fd		= PF.stderr,
	    name	= "<stdErr>",
	    appendMode	= false, (* Bug!  Should check! *)
	    chunkSize	= bufferSzB
	  }

    fun strReader src = let
	  val lockMV = SV.mVarInit()
	  fun withLock f x = (
		SV.mTake lockMV;
		f x before SV.mPut(lockMV, ()))
		  handle ex => (SV.mPut(lockMV, ()); raise ex)
	  val pos = ref 0
	  val closed = ref false
	  fun checkClosed () = if !closed then raise IO.ClosedStream else ()
	  val len = String.size src
	  val plen = Position.fromInt len
	  fun avail () = Position.fromInt(len - !pos)
	  fun readV n = let
		val p = !pos
		val m = Int.min(n, len-p)
		in
		  checkClosed ();
		  pos := p+m;
(** NOTE: could use unchecked operations here **)
		  String.substring (src, p, m)
		end
	  fun readA asl = let
		val p = !pos
		val (buf, i, n) = CharArraySlice.base asl
		val m = Int.min (n, len - p)
		in
		  checkClosed ();
		  pos := p+m;
		  CharArraySlice.copyVec
		      { src = CharVectorSlice.slice (src, p, SOME m),
			dst = buf, di = i };
		  m
		end
	  fun getPos () = (checkClosed(); Position.fromInt (!pos))
	  fun setPos p =
	      (checkClosed ();
	       if p < 0 andalso p > plen then raise Subscript
	       else pos := Position.toInt p)
	  in
	    PrimIO.RD{
		name        = "<string>",
		chunkSize   = len,
		readVec     = withLock readV,
        	readArr     = withLock readA,
		readVecEvt  = withLock(CML.alwaysEvt o readV),
		readArrEvt  = withLock(CML.alwaysEvt o readA),
		avail       = SOME o avail,
		getPos      = SOME(withLock getPos),
		setPos	    = SOME(withLock setPos),
        	endPos      = SOME(withLock(fn () => (checkClosed(); plen))),
		verifyPos   = SOME(withLock getPos),
		close       = withLock(fn () => closed := true),
		ioDesc      = NONE
	      }
	  end

  end (* PosixTextPrimIO *)


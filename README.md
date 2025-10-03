cml-basis
=========

Structures in the Basis library with functions rebound to be thread safe and additional event-based functions. This library was ported from SML/NJ so that other compilers which support Concurrent ML like MLton can make use of them.
Currently the `Unix` structure and event-based process management is not
implemented because `ProcManager` has not been ported to MLton.

To add this library to your project with smlpkg, run:

```
smlpkg add github.com/DarinM223/cml-basis
```

The `examples/tcp-server/` directory contains an example for using this library. Using `cml-basis-unix.mlb`, the same code that uses event-based TCP sockets compiles with both SML/NJ and MLton.

import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as rtsConstants from "./rts.constants.mjs";
import { isI32 } from "./rts.typecheck.mjs";

/*
  The methods of this class are related to exception handling in Haskell.
 */
export class ExceptionHelper {
  constructor(memory, heapalloc, exports, info_tables, symbol_table) {
    this.memory = memory;
    this.heapAlloc = heapalloc;
    this.exports = exports;
    this.infoTables = info_tables;
    this.symbolTable = symbol_table;
    this.decoder = new TextDecoder("utf-8", { fatal: true });
    this.errorBuffer = "";
    Object.seal(this);
  }

  /*
    This implements a subset of `raiseExceptionHelper` in `rts/Schedule.c` of
    ghc rts. The function is called by `stg_raisezh` in `Exception.cmm` in rts.

    When a Haskell exception is raised, `stg_raisezh` is entered, and it calls
    `raiseExceptionHelper` to traverse the stack from the top. For each update
    frame, the thunk is updated with the "exception closure" (which throws when
    entered). It exits when a catch frame or stop frame is encountered.

    The stack pointer is rewritten to the head of last encountered frame, and
    the frame type is returned to `stg_raisezh` for further processing.
  */
  raiseExceptionHelper(reg, tso, exception) {
    isI32(reg);
    isI32(tso);
    isI32(exception);
    const raise_closure = this.heapAlloc.allocate(
      Math.ceil(rtsConstants.sizeof_StgThunk / 4) + 1
    );
    this.memory.i32Store(
      raise_closure,
      this.symbolTable.addressOf("stg_raise_info")
    );
    this.memory.i32Store(
      raise_closure + rtsConstants.offset_StgThunk_payload,
      exception
    );
    const stackobj = (
      this.memory.i32Load(tso + rtsConstants.offset_StgTSO_stackobj)
    );
    let p = (
      this.memory.i32Load(stackobj + rtsConstants.offset_StgStack_sp)
    );
    while (true) {
      const info = (this.memory.i32Load(p)),
        type = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_type
        ),
        raw_layout = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
      if (this.infoTables && !this.infoTables.has(info))
        throw new WebAssembly.RuntimeError(
          `Invalid info table 0x${info.toString(16)}`
        );
      switch (type) {
        case ClosureTypes.UPDATE_FRAME: {
          const p1 = (
            this.memory.i32Load(p + rtsConstants.offset_StgUpdateFrame_updatee)
          );
          this.exports.updateThunk(
            this.symbolTable.addressOf("MainCapability"),
            tso,
            p1,
            raise_closure
          );
          const size = (raw_layout & (0x1f));
          p += (1 + size) << 2;
          break;
        }
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.STOP_FRAME: {
          this.memory.i32Store(stackobj + rtsConstants.offset_StgStack_sp, p);
          return isI32(type);
        }
        case ClosureTypes.RET_SMALL: {
          const size = (raw_layout & (0x1f));
          p += (1 + size) << 2;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = (
            this.memory.i32Load(
              (raw_layout) + rtsConstants.offset_StgLargeBitmap_size
            )
          );
          p += (1 + size) << 2;
          break;
        }
        case ClosureTypes.RET_FUN: {
          const size = (
            this.memory.i32Load(p + rtsConstants.offset_StgRetFun_size)
          );
          p += rtsConstants.sizeof_StgRetFun + (size << 2);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError(
            `raiseExceptionHelper: unsupported stack frame ${type} at 0x${p.toString(
              16
            )}`
          );
      }
    }
  }

  /*
    This implements `barf` in `rts/RtsMessages.c` of ghc rts. The function is
    used to signal a fatal runtime error.

    The original `barf` is a varargs C function which takes a format string.
    Unfortunately, we don't implement handling for varargs yet, so we restrict
    our `barf` to take exactly 1 argument: a pointer to a NUL-terminated string
    which is the error message itself.

    There exists special `barf`-related logic in various parts of the asterius
    compiler:

    * In the rts builtins (`Asterius.Builtins`) module, we import `barf` as
      `__asterius_barf`, and make a `barf` function wrapper which handles the
      i64/f64 conversion workaround.

    * The rts cmm files call `barf` with either 0, 1, 2 arguments. In the
      backend we remove extra arguments, and if there isn't any, we use a
      `NULL` pointer as argument, which is interpreted as empty error message in
      our implementation.
   */
  barf(s) {
    if (s) {
      const v0 = this.memory.i8View.subarray(s),
        len = v0.indexOf(0),
        v1 = v0.subarray(0, len),
        r = this.decoder.decode(v1);
      throw new WebAssembly.RuntimeError(`barf: ${r}`);
    } else {
      throw new WebAssembly.RuntimeError("barf");
    }
  }

  /*
    The following two functions implement a variant of `barf` that is used by
    Asterius to report missing symbols. Instead of finding the error message to
    print in a data segment (like `barf` does), this approach accumulates it
    (character by character) into an internal buffer using `barf_push`. Then, a
    call to `barf_signal` reads this buffer and throws the error.

    The related logic can be found in two places in the Asterius compiler:

    * In the rts builtins (`Asterius.Builtins`) module, we import `barf_push`
      (and `barf_signal`) as `__asterius_barf_push` (and
      `__asterius_barf_signal`), and make a `barf_push` (and `barf_signal`)
      function wrapper which handles the i64/f64 conversion workaround.

    * In `Asterius.Internals.Barf` we implement `barf`, which converts a single
      `Barf` expression to a series of calls to `barf_push`, each taking (the
      ascii code of) a single character of the error message, followed by a
      call to `barf_signal`.

    In the backend (`Asterius.Backends.Binaryen*`), when we encounter an unresolved
    symbol `sym`, if @verbose_err@ is on, we insert a `barf` call there. So
    if an execution path leads to the unresolved symbol, we're likely to get
    the symbol name from the js error message.
  */
  barf_push(c) {
    this.errorBuffer += String.fromCodePoint(c);
  }

  barf_signal(f) {
    const buf = this.errorBuffer;
    this.errorBuffer = "";
    if (f) {
      throw new WebAssembly.RuntimeError(`barf_signal: ${buf}`);
    } else {
      console.error(`[DEBUG] ${buf}`);
    }
  }
}

import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

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
    Object.freeze(this);
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
    const raise_closure = this.heapAlloc.allocate(
      Math.ceil(rtsConstants.sizeof_StgThunk / 8) + 1
    );
    this.memory.i64Store(
      raise_closure,
      this.symbolTable.addressOf("stg_raise_info")
    );
    this.memory.i64Store(
      raise_closure + rtsConstants.offset_StgThunk_payload,
      exception
    );
    const stackobj = Number(
      this.memory.i64Load(tso + rtsConstants.offset_StgTSO_stackobj)
    );
    let p = Number(
      this.memory.i64Load(stackobj + rtsConstants.offset_StgStack_sp)
    );
    while (true) {
      const info = Number(this.memory.i64Load(p)),
        type = this.memory.i32Load(
          info + rtsConstants.offset_StgInfoTable_type
        ),
        raw_layout = this.memory.i64Load(
          info + rtsConstants.offset_StgInfoTable_layout
        );
      if (this.infoTables && !this.infoTables.has(info))
        throw new WebAssembly.RuntimeError(
          `Invalid info table 0x${info.toString(16)}`
        );
      switch (type) {
        case ClosureTypes.UPDATE_FRAME: {
          const p1 = Number(
            this.memory.i64Load(p + rtsConstants.offset_StgUpdateFrame_updatee)
          );
          this.exports.updateThunk(
            this.symbolTable.addressOf("MainCapability"),
            tso,
            p1,
            raise_closure
          );
          const size = Number(raw_layout & BigInt(0x3f));
          p += (1 + size) << 3;
          break;
        }
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.STOP_FRAME: {
          this.memory.i64Store(stackobj + rtsConstants.offset_StgStack_sp, p);
          return type;
        }
        case ClosureTypes.RET_SMALL: {
          const size = Number(raw_layout & BigInt(0x3f));
          p += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = Number(
            this.memory.i64Load(
              Number(raw_layout) + rtsConstants.offset_StgLargeBitmap_size
            )
          );
          p += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_FUN: {
          const size = Number(
            this.memory.i64Load(p + rtsConstants.offset_StgRetFun_size)
          );
          p += rtsConstants.sizeof_StgRetFun + (size << 3);
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

    There exist special `barf`-related logic in various parts of the asterius
    compiler:

    * In the rts builtins (`Asterius.Builtins`) module, we import `barf` as
      `__asterius_barf`, and make a `barf` function wrapper which handles the
      i64/f64 conversion workaround.

    * In the linker (`Asterius.Resolve`), when we encounter an unresolved
      symbol, we dynamically generate a small data segment which is the
      NUL-terminated error message containing the symbol itself. The data
      segment's own symbol is prefixed with `__asterius_barf_`.

    * In the backends (`Asterius.Backends.*`), when we encounter an unresolved
      symbol `sym`, we try to find `__asterius_barf_sym`, and if found, we
      insert a `barf` call there. So if an execution path leads to the
      unresolved symbol, we're likely to get the symbol name from the js error
      message.

    * The rts cmm files call `barf` with either 0, 1, 2 arguments. In the
      backends we remove extra arguments, and if there isn't any, we use a
      `NULL` pointer as argument, which is interpreted as empty error message in
      our implementation.
   */
  barf(s) {
    if (s) {
      const v0 = this.memory.i8View.subarray(Memory.unTag(s)),
        len = v0.indexOf(0),
        v1 = v0.subarray(0, len),
        r = this.decoder.decode(v1);
      throw new WebAssembly.RuntimeError(`barf: ${r}`);
    } else {
      throw new WebAssembly.RuntimeError("barf");
    }
  }
}

import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as rtsConstants from "./rts.constants.mjs";
import { Memory } from "./rts.memory.mjs";

export class ExceptionHelper {
  constructor(memory, heapalloc, info_tables, symbol_table) {
    this.memory = memory;
    this.heapAlloc = heapalloc;
    this.infoTables = info_tables;
    this.symbolTable = symbol_table;
    this.decoder = new TextDecoder("utf-8", { fatal: true });
    Object.freeze(this);
  }
  raiseExceptionHelper(reg, tso, exception) {
    const raise_closure = this.heapAlloc.allocate(
      Math.ceil(rtsConstants.sizeof_StgThunk / 8) + 1
    );
    this.memory.i64Store(raise_closure, this.symbolTable.stg_raise_info);
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
      if (!this.infoTables.has(info))
        throw new WebAssembly.RuntimeError(
          "raiseExceptionHelper: invalid info pointer"
        );
      switch (type) {
        case ClosureTypes.UPDATE_FRAME: {
          const p1 = Number(
            this.memory.i64Load(p + rtsConstants.offset_StgUpdateFrame_updatee)
          );
          this.memory.i64Store(p1, this.symbolTable.stg_BLACKHOLE_info);
          this.memory.i64Store(
            p1 + rtsConstants.offset_StgInd_indirectee,
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
            this.memory.i64Load(c + rtsConstants.offset_StgRetFun_size)
          );
          p += rtsConstants.sizeof_StgRetFun + (size << 3);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError(
            "raiseExceptionHelper: unsupported stack frame"
          );
      }
    }
  }
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

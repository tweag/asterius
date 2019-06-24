import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as rtsConstants from "./rts.constants.mjs";

export class ThreadPaused {
  constructor(memory, info_tables, symbol_table) {
    this.memory = memory;
    this.infoTables = info_tables;
    this.symbolTable = symbol_table;
    Object.freeze(this);
  }

  threadPaused(cap, tso) {
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
          "threadPaused: invalid info pointer"
        );
      switch (type) {
        case ClosureTypes.UPDATE_FRAME: {
          if (info === this.symbolTable.stg_marked_upd_frame_info) return;
          this.memory.i64Store(p, this.symbolTable.stg_marked_upd_frame_info);
          const bh = Number(
            this.memory.i64Load(p + rtsConstants.offset_StgUpdateFrame_updatee)
          );
          this.memory.i64Store(bh, this.symbolTable.stg_BLACKHOLE_info);
          this.memory.i64Store(bh + rtsConstants.offset_StgInd_indirectee, tso);
          const size = Number(raw_layout & BigInt(0x3f));
          p += (1 + size) << 3;
          break;
        }
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.RET_SMALL: {
          const size = Number(raw_layout & BigInt(0x3f));
          p += (1 + size) << 3;
          break;
        }
        case ClosureTypes.STOP_FRAME: {
          return;
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
            "threadPaused: unsupported stack frame"
          );
      }
    }
  }
}

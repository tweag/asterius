import * as ClosureTypes from "./rts.closuretypes.mjs";
import { Memory } from "./rts.memory.mjs";
import * as settings from "./rts.settings.mjs";

export class SanCheck {
  constructor(memory) {
    this.memory = memory;
    this.visitedClosures = new Set();
    Object.seal(this);
  }

  checkClosure(c) {
    if (this.visitedClosures.has(Memory.unTag(c))) return;
    this.visitedClosures.add(Memory.unTag(c));
  }

  checkStack(stackobj) {
    const stack_size = Number(
        this.memory.i64Load(stackobj + settings.offset_StgStack_stack_size) &
        BigInt(0xffffffff)),
          sp = Number(
              this.memory.i64Load(stackobj + settings.offset_StgStack_sp)),
          sp_lim =
              stackobj + settings.offset_StgStack_stack + (stack_size << 3);
    let c = sp;
    while (true) {
      if (c > sp_lim)
        throw new WebAssembly.RuntimeError(`Slipped through SpLim,
                                           SpLim: 0x${ sp_lim.toString(16) },
                                           Sp: 0x${ c.toString(16) }`);
      if (c == sp_lim) break;
      const p = Number(this.memory.i64Load(c)),
            type = Number(
                this.memory.i64Load(p + settings.offset_StgInfoTable_type) &
                BigInt(0xffffffff)),
            raw_layout =
                this.memory.i64Load(p + settings.offset_StgInfoTable_layout);
      switch (type) {
        case ClosureTypes.RET_SMALL:
        case ClosureTypes.UPDATE_FRAME:
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.UNDERFLOW_FRAME:
        case ClosureTypes.STOP_FRAME:
        case ClosureTypes.ATOMICALLY_FRAME:
        case ClosureTypes.CATCH_RETRY_FRAME:
        case ClosureTypes.CATCH_STM_FRAME: {
          const size = Number(raw_layout & BigInt(0x3f)),
                bitmap = raw_layout >> BigInt(6);
          for (let i = 0; i < size; ++i)
            if ((bitmap >> BigInt(i)) & BigInt(1))
              this.checkClosure(this.memory.i64Load(c + ((1 + i) << 3)));
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = Number(this.memory.i64Load(
              Number(raw_layout) + settings.offset_StgLargeBitmap_size));
          let bitmap = BigInt(0);
          for (let i = 0; i < Math.ceil(size / 64); ++i)
            bitmap |=
                memory.i64Load(Number(raw_layout) +
                               settings.offset_StgLargeBitmap_bitmap + (i << 3))
                << BigInt(i << 6);
          for (let i = 0; i < size; ++i)
            if ((bitmap >> BigInt(i)) & BigInt(1))
              this.checkClosure(this.memory.i64Load(c + ((1 + i) << 3)));
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_FUN: {
          const size =
              Number(this.memory.i64Load(c + settings.offset_StgRetFun_size)), fun_info = this.memory.i64Load(this.memory.i64Load(c + settings.offset_StgRetFun_fun));
          
          c += settings.sizeof_StgRetFun + (size << 3);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError(`Invalid frame type ${ tbl.type }`);
      }
    }
  }

  checkTSO(tso) {
    this.visitedClosures.clear();
    const stackobj =
        Number(this.memory.i64Load(tso + settings.offset_StgTSO_stackobj));
    this.checkStack(stackobj);
    console.log(`[EVENT] Live object count: ${ this.visitedClosures.size }`);
    this.visitedClosures.clear();
  }
}

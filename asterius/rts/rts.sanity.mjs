import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as FunTypes from "./rts.funtypes.mjs";
import { Memory } from "./rts.memory.mjs";
import * as settings from "./rts.settings.mjs";
import { stg_arg_bitmaps } from "./rts.autoapply.mjs";

export class SanCheck {
  constructor(memory) {
    this.memory = memory;
    this.visitedClosures = new Set();
    Object.freeze(this);
  }

  checkSmallBitmap(payload, bitmap, size) {
    for (let i = 0; i < size; ++i)
      if (!((bitmap >> BigInt(i)) & BigInt(1)))
        this.checkClosure(this.memory.i64Load(payload + (i << 3)));
  }

  checkLargeBitmap(payload, large_bitmap, size) {
    for (let j = 0; j < size; j += 64) {
      const bitmap = this.memory.i64Load(
          large_bitmap + settings.offset_StgLargeBitmap_bitmap + (j >> 3));
      for (let i = j; i - j < 64 && i < size; ++i)
        if (!((bitmap >> BigInt(i - j)) & BigInt(1)))
          this.checkClosure(this.memory.i64Load(payload + (i << 3)));
    }
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
          this.checkSmallBitmap(c + 8, bitmap, size);
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_BIG: {
          const size = Number(this.memory.i64Load(
              Number(raw_layout) + settings.offset_StgLargeBitmap_size));
          this.checkLargeBitmap(c + 8, Number(raw_layout), size);
          c += (1 + size) << 3;
          break;
        }
        case ClosureTypes.RET_FUN: {
          const size =
              Number(this.memory.i64Load(c + settings.offset_StgRetFun_size)),
                fun_info = Number(this.memory.i64Load(
                    this.memory.i64Load(c + settings.offset_StgRetFun_fun)));
          switch (Number(
              this.memory.i64Load(fun_info + settings.offset_StgFunInfoTable_f +
                                  settings.offset_StgFunInfoExtraFwd_fun_type) &
              BigInt(0xffffffff))) {
            case FunTypes.ARG_GEN:
              this.checkSmallBitmap(
                  c + settings.offset_StgRetFun_payload,
                  this.memory.i64Load(fun_info +
                                      settings.offset_StgFunInfoTable_f +
                                      settings.offset_StgFunInfoExtraFwd_b) >>
                      BigInt(6),
                  size);
              break;
            case FunTypes.ARG_GEN_BIG:
              break;
            default:
              this.checkSmallBitmap(
                  c + settings.offset_StgRetFun_payload,
                  BigInt(stg_arg_bitmaps
                             [this.memory.i64Load(
                                  fun_info + settings.offset_StgFunInfoTable_f +
                                  settings.offset_StgFunInfoExtraFwd_fun_type) &
                              BigInt(0xFFFFFFFF)]) >>
                      BigInt(6),
                  size);
              break;
          }
          c += settings.sizeof_StgRetFun + (size << 3);
          break;
        }
        default:
          throw new WebAssembly.RuntimeError(`Invalid frame type ${ tbl.type }`);
      }
    }
  }

  checkTSO(tso) {
    const stackobj =
        Number(this.memory.i64Load(tso + settings.offset_StgTSO_stackobj));
    this.checkStack(stackobj);
    console.log(`[EVENT] Live object count: ${ this.visitedClosures.size }`);
    this.visitedClosures.clear();
  }
}

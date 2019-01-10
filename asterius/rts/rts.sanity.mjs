import * as ClosureTypes from "./rts.closuretypes.mjs";
import { InfoTableParser } from "./rts.infotables.mjs";
import * as settings from "./rts.settings.mjs";

export class SanCheck {
  constructor(memory) {
    this.memory = memory;
    this.infoTableParser = new InfoTableParser(memory);
    Object.freeze(this);
  }

  checkTSO(tso) {
    const stackobj = Number(
        this.memory.i64Load(tso + settings.offset_StgTSO_stackobj)
      ),
      stack_size = Number(
        this.memory.i64Load(stackobj + settings.offset_StgStack_stack_size) &
          BigInt(0xffffffff)
      ),
      sp = Number(this.memory.i64Load(stackobj + settings.offset_StgStack_sp)),
      sp_lim = stackobj + settings.offset_StgStack_stack + (stack_size << 3);
    let p = sp,
      tbls = [];
    while (true) {
      if (p > sp_lim)
        throw new WebAssembly.RuntimeError("Slipped through SpLim");
      if (p == sp_lim) break;
      const tbl = this.infoTableParser.parse(Number(this.memory.i64Load(p)));
      switch (tbl.type) {
        case ClosureTypes.RET_SMALL:
        case ClosureTypes.UPDATE_FRAME:
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.UNDERFLOW_FRAME:
        case ClosureTypes.STOP_FRAME:
        case ClosureTypes.ATOMICALLY_FRAME:
        case ClosureTypes.CATCH_RETRY_FRAME:
        case ClosureTypes.CATCH_STM_FRAME:
        case ClosureTypes.RET_BIG:
          break;
        default:
          throw new WebAssembly.RuntimeError("Invalid frame type");
      }
      tbls.push(tbl);
      p += (tbl.layout.size + 1) << 3;
    }
    console.log(tbls);
  }
}

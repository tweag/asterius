import * as ClosureTypes from "./rts.closuretypes.mjs";
import * as settings from "./rts.settings.mjs";

class Layout {
  constructor(size, bitmap) {
    this.size = size;
    this.bitmap = bitmap;
    Object.freeze(this);
  }
  static fromPointersFirst(raw_layout) {
    const ptrs = Number(raw_layout & BigInt(0xffffffff)),
          nptrs = Number(raw_layout >> BigInt(32));
    return new Layout(ptrs + nptrs, (BigInt(1) << BigInt(ptrs)) - BigInt(1));
  }
  static fromSmallBitmap(raw_layout) {
    const size = Number(raw_layout & BigInt(0x3f)),
          bitmap = raw_layout >> BigInt(6);
    return new Layout(size, bitmap);
  }
  static fromLargeBitmap(memory, raw_layout) {
    const p = Number(raw_layout),
          size =
              Number(memory.i64Load(p + settings.offset_StgLargeBitmap_size));
    let bitmap = BigInt(0);
    for (let i = 0; i < Math.ceil(size / 64); ++i)
      bitmap |=
          memory.i64Load(p + settings.offset_StgLargeBitmap_bitmap + (i << 3))
          << BigInt(i << 6);
    return new Layout(size, bitmap);
  }
}

class InfoTable {
  constructor(type, layout) {
    this.type = type;
    this.layout = layout;
    Object.freeze(this);
  }
}

export class InfoTableParser {
  constructor(memory) {
    this.memory = memory;
    this.memoTable = new Map();
    Object.freeze(this);
  }

  parse(p) {
    let r = this.memoTable.get(p);
    if (!r) {
      const raw_layout =
          this.memory.i64Load(p + settings.offset_StgInfoTable_layout),
            type = Number(
                this.memory.i64Load(p + settings.offset_StgInfoTable_type) &
                BigInt(0xffffffff));
      switch (type) {
        case ClosureTypes.RET_SMALL:
        case ClosureTypes.UPDATE_FRAME:
        case ClosureTypes.CATCH_FRAME:
        case ClosureTypes.UNDERFLOW_FRAME:
        case ClosureTypes.STOP_FRAME:
        case ClosureTypes.ATOMICALLY_FRAME:
        case ClosureTypes.CATCH_RETRY_FRAME:
        case ClosureTypes.CATCH_STM_FRAME:
          r = new InfoTable(type, Layout.fromSmallBitmap(raw_layout));
          break;
        case ClosureTypes.RET_BIG:
          r = new InfoTable(type,
                            Layout.fromLargeBitmap(this.memory, raw_layout));
          break;
        default:
          if (type > ClosureTypes.INVALID_OBJECT &&
              type < ClosureTypes.N_CLOSURE_TYPES)
            r = new InfoTable(type, Layout.fromPointersFirst(raw_layout));
          else
            throw new WebAssembly.RuntimeError("Invalid closure type " + type +
                                               " at 0x" + p.toString(16));
      }
      this.memoTable.set(p, r);
    }
    return r;
  }
}

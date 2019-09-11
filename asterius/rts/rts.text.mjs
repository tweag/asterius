import { Memory } from "./rts.memory.mjs";

export class TextCBits {
  constructor(memory) {
    this.memory = memory;
    Object.seal(this);
  }

  _hs_text_memcpy(_dst, dst_off, _src, src_off, n) {
    const dst = Memory.unTag(_dst) + dst_off*2,
          src = Memory.unTag(_src) + src_off*2;
    this.memory.i8View.copyWithin(dst, src, src+n*2);
  }

  _hs_text_memcmp(_dst, dst_off, _src, src_off, n) {
    const dst = Memory.unTag(_dst) + dst_off*2,
          src = Memory.unTag(_src) + src_off*2;
    for (let i = 0; i < n*2; ++i) {
      const sgn = Math.sign(this.memory.i8View[dst + i] -
                            this.memory.i8View[src + i]);
      if (sgn) return sgn;
    }
    return 0;
  }

};

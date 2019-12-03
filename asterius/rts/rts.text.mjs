import { Memory } from "./rts.memory.mjs";

export class TextCBits {
  constructor(memory) {
    this.memory = memory;
    Object.seal(this);
  }

  _hs_text_memcpy(_dst, dst_off, _src, src_off, n) {
    const dst = Memory.unTag(_dst) + dst_off * 2,
      src = Memory.unTag(_src) + src_off * 2;
    this.memory.i8View.copyWithin(dst, src, src + n * 2);
  }

  _hs_text_memcmp(_dst, dst_off, _src, src_off, n) {
    const dst = Memory.unTag(_dst) + dst_off * 2,
      src = Memory.unTag(_src) + src_off * 2;
    for (let i = 0; i < n * 2; ++i) {
      const sgn = Math.sign(
        this.memory.i8View[dst + i] - this.memory.i8View[src + i]
      );
      if (sgn) return sgn;
    }
    return 0;
  }

  _hs_text_decode_utf8(dest, destoffp, src, srcend) {
    const dec = new TextDecoder("utf-8", { fatal: true }),
      s = dec.decode(
        this.memory.i8View.subarray(Memory.unTag(src), Memory.unTag(srcend))
      );
    for (let i = 0; i < s.length; ++i)
      this.memory.i16Store(dest + i * 2, s.charCodeAt(i));
    this.memory.i64Store(destoffp, s.length);
    return srcend;
  }

  _hs_text_encode_utf8(destp, src, srcoff, srclen) {
    const dec = new TextDecoder("utf-16le", { fatal: true }),
      s = dec.decode(
        this.memory.i8View.subarray(
          Memory.unTag(src + srcoff * 2),
          Memory.unTag(src + srcoff * 2 + srclen * 2)
        )
      ),
      dest = Number(this.memory.i64Load(destp)),
      enc = new TextEncoder(),
      // `Data.Text.Encoding.encodeUtf8` allocates a `ByteArray#` of size
      // `srclen * 3` to ensure enough space for a single-pass encoding from
      // UTF-16 to UTF-8. See the comment near
      // `https://github.com/haskell/text/blob/2176eb38b5238e763e8076b0d0db8c2f2014ab8b/Data/Text/Encoding.hs#L432`
      // and the "Buffer Sizing" section of
      // `https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encodeInto`
      // for details.
      l = enc.encodeInto(
        s,
        this.memory.i8View.subarray(
          Memory.unTag(dest),
          Memory.unTag(dest + srclen * 3)
        )
      ).written;
    this.memory.i64Store(destp, dest + l);
  }
}

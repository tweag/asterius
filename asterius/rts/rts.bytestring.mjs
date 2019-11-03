import { Memory } from "./rts.memory.mjs";

export class ByteStringCBits {
  constructor(memory) {
    this.memory = memory;
    Object.seal(this);
  }

  fps_reverse(_q, _p, n) {
    const q = Memory.unTag(_q),
      p = Memory.unTag(_p);
    this.memory.i8View.copyWithin(q, p, p + n);
    this.memory.i8View.subarray(q, q + n).reverse();
  }

  fps_intersperse(_q, _p, n, c) {
    let q = Memory.unTag(_q),
      p = Memory.unTag(_p);
    while (n > 1) {
      this.memory.i8View[q++] = this.memory.i8View[p++];
      this.memory.i8View[q++] = c;
      --n;
    }
    if (n === 1) this.memory.i8View[q] = this.memory.i8View[p];
  }

  fps_maximum(_p, len) {
    const p = Memory.unTag(_p),
      buffer = this.memory.i8View.subarray(p, p + len);
    return buffer.reduce((x, y) => Math.max(x, y), buffer[0]);
  }

  fps_minimum(_p, len) {
    const p = Memory.unTag(_p),
      buffer = this.memory.i8View.subarray(p, p + len);
    return buffer.reduce((x, y) => Math.min(x, y), buffer[0]);
  }

  fps_count(_p, len, w) {
    const p = Memory.unTag(_p),
      buffer = this.memory.i8View.subarray(p, p + len);
    return buffer.reduce((tot, c) => (c === w ? tot + 1 : tot), 0);
  }

  fps_memcpy_offsets(_dst, dst_off, _src, src_off, n) {
    const dst = Memory.unTag(_dst),
      src = Memory.unTag(_src);
    this.memory.i8View.copyWithin(
      dst + dst_off,
      src + src_off,
      src + src_off + n
    );
    return _dst + dst_off;
  }

  _hs_bytestring_itoa(x, _buf, base, pad) {
    const buf = Memory.unTag(_buf),
      x_str = x.toString(base).padStart(pad, "0");
    for (let i = 0; i < x_str.length; ++i)
      this.memory.i8View[buf + i] = x_str.codePointAt(i);
    return _buf + x_str.length;
  }

  _hs_bytestring_int_dec(x, _buf) {
    return this._hs_bytestring_itoa(x, _buf, 10, 0);
  }

  _hs_bytestring_long_long_int_dec(x, _buf) {
    return this._hs_bytestring_itoa(x, _buf, 10, 0);
  }

  _hs_bytestring_uint_dec(x, _buf) {
    return this._hs_bytestring_itoa(x, _buf, 10, 0);
  }

  _hs_bytestring_long_long_uint_dec(x, _buf) {
    return this._hs_bytestring_itoa(x, _buf, 10, 0);
  }

  _hs_bytestring_int_dec_padded9(x, _buf) {
    this._hs_bytestring_itoa(x, _buf, 10, 9);
  }

  _hs_bytestring_long_long_int_dec_padded18(x, _buf) {
    this._hs_bytestring_itoa(x, _buf, 10, 18);
  }

  _hs_bytestring_uint_hex(x, _buf) {
    return this._hs_bytestring_itoa(x, _buf, 16, 0);
  }

  _hs_bytestring_long_long_uint_hex(x, _buf) {
    return this._hs_bytestring_itoa(x, _buf, 16, 0);
  }
}

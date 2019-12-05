import * as rtsConstants from "./rts.constants.mjs";

function mask(n) {
  return (BigInt(1) << BigInt(n)) - BigInt(1);
}

export class Memory {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.i8View = undefined;
    this.dataView = undefined;
    this.capacity = undefined;
    this.liveBitset = undefined;
    Object.seal(this);
  }

  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.initView();
    this.capacity = this.buffer.byteLength / rtsConstants.mblock_size;
    this.liveBitset = mask(this.capacity);
  }

  initView() {
    this.i8View = new Uint8Array(this.memory.buffer);
    this.dataView = new DataView(this.memory.buffer);
  }

  static unTag(p) {
    return Number(p) & 0xffffffff;
  }

  static getTag(p) {
    //return Number(BigInt(p) >> BigInt(32));
    return Math.floor(Number(p) / (2**32));
  }

  static tagData(p) {
    return rtsConstants.dataTag * (2**32) + Number(p);
  }

  static tagFunction(p) {
    return rtsConstants.functionTag * (2**32) + Number(p);
  }

  static unDynTag(p) {
    const np = Number(p);
    return np - (np & 7);
  }

  static getDynTag(p) {
    return Number(p) & 7;
  }

  static setDynTag(p, t) {
    const np = Number(p);
    return np - (np & 7) + t;
  }

  get buffer() {
    return this.memory.buffer;
  }

  grow(n) {
    const prev_pages = this.memory.grow(n);
    this.initView();
    return prev_pages;
  }

  i8Load(p) {
    return this.i8View[Memory.unTag(p)];
  }

  i8Store(p, v) {
    this.i8View[Memory.unTag(p)] = Number(v);
  }

  i16Load(p) {
    return this.dataView.getUint16(Memory.unTag(p), true);
  }

  i16Store(p, v) {
    this.dataView.setUint16(Memory.unTag(p), Number(v), true);
  }

  i32Load(p) {
    return this.dataView.getUint32(Memory.unTag(p), true);
  }

  i32Store(p, v) {
    this.dataView.setUint32(Memory.unTag(p), Number(v), true);
  }

  i64Load(p) {
    return this.dataView.getBigUint64(Memory.unTag(p), true);
  }

  i64Store(p, v) {
    this.dataView.setBigUint64(Memory.unTag(p), BigInt(v), true);
  }

  i128Load(p) {
    // little endian: number with hex digits <0A0B> at address p
    // get stored as mem[p] = 0B, mem[p+1] = 0A
    let low = this.dataView.getBigUint64(Memory.unTag(p), true);
    let high = this.dataView.getBigUint64(Memory.unTag(p) + 8, true);
    return low | (high << BigInt(64));
  }

  i128Store(p, v) {
    // create all 1s of 64 bits.
    const lowmask = (BigInt(1) << BigInt(64)) - BigInt(1);

    // little endian: number with digits <x y> at address p
    // get stored as mem[p] = y, mem[p+1] = x
    const low = v & lowmask;
    const high = v >> BigInt(64);

    // byte addressed, so +8 = 64-bit
    this.dataView.setBigUint64(Memory.unTag(p), low, true);
    this.dataView.setBigUint64(Memory.unTag(p) + 8, high, true);
  }

  f32Load(p) {
    return this.dataView.getFloat32(Memory.unTag(p), true);
  }

  f32Store(p, v) {
    this.dataView.setFloat32(Memory.unTag(p), Number(v), true);
  }

  f64Load(p) {
    return this.dataView.getFloat64(Memory.unTag(p), true);
  }

  f64Store(p, v) {
    this.dataView.setFloat64(Memory.unTag(p), Number(v), true);
  }

  i32LoadS8(p) {
    return this.dataView.getInt8(Memory.unTag(p));
  }

  i32LoadU8(p) {
    return this.dataView.getUint8(Memory.unTag(p));
  }

  i32LoadS16(p) {
    return this.dataView.getInt16(Memory.unTag(p), true);
  }

  i32LoadU16(p) {
    return this.dataView.getUint16(Memory.unTag(p), true);
  }

  i64LoadS8(p) {
    return BigInt(this.dataView.getInt8(Memory.unTag(p)));
  }

  i64LoadU8(p) {
    return BigInt(this.dataView.getUint8(Memory.unTag(p)));
  }

  i64LoadS16(p) {
    return BigInt(this.dataView.getInt16(Memory.unTag(p), true));
  }

  i64LoadU16(p) {
    return BigInt(this.dataView.getUint16(Memory.unTag(p), true));
  }

  heapAlloced(p) {
    return (
      Memory.unTag(p) >=
      this.staticMBlocks << rtsConstants.mblock_size_log2
    );
  }

  getMBlocks(n) {
    const m = mask(n);
    for (let i = BigInt(0); i <= BigInt(this.capacity - n); ++i) {
      const mi = m << i;
      if (!(this.liveBitset & mi)) {
        this.liveBitset |= mi;
        return Memory.tagData(Number(i) * rtsConstants.mblock_size);
      }
    }
    const d = Math.max(n, this.capacity),
      prev_capacity = this.capacity;
    this.grow(d * (rtsConstants.mblock_size / rtsConstants.pageSize));
    this.capacity += d;
    this.liveBitset |= m << BigInt(prev_capacity);
    return Memory.tagData(prev_capacity * rtsConstants.mblock_size);
  }

  freeMBlocks(p, n) {
    const mblock_no =
      BigInt(Memory.unTag(p)) >> BigInt(rtsConstants.mblock_size_log2);
    this.liveBitset &= ~(mask(n) << mblock_no);
  }

  strlen(_str) {
    return this.i8View.subarray(Memory.unTag(_str)).indexOf(0);
  }

  strLoad(_str) {
    let p = Memory.unTag(_str);
    let s = "";
    let i = 0;

    while (1) {
      let c = this.i8View[p + i];
      if (c == 0) {
        return s;
      }
      s += String.fromCharCode(c);
      i++;
    }
  }

  memchr(_ptr, val, num) {
    const ptr = Memory.unTag(_ptr),
      off = this.i8View.subarray(ptr, ptr + num).indexOf(val);
    return off === -1 ? 0 : _ptr + off;
  }

  memcpy(_dst, _src, n) {
    this.i8View.copyWithin(
      Memory.unTag(_dst),
      Memory.unTag(_src),
      Memory.unTag(_src) + n
    );
  }

  memmove(_dst, _src, n) {
    return this.memcpy(_dst, _src, n);
  }

  memset(_dst, c, n) {
    this.i8View.fill(c, Memory.unTag(_dst), Memory.unTag(_dst) + n);
  }

  memcmp(_ptr1, _ptr2, n) {
    for (let i = 0; i < n; ++i) {
      const sgn = Math.sign(
        this.i8View[Memory.unTag(_ptr1) + i] -
          this.i8View[Memory.unTag(_ptr2) + i]
      );
      if (sgn) return sgn;
    }
    return 0;
  }
}

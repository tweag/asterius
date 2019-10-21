function abs(bi) {
  return bi < BigInt(0) ? -bi : bi;
}

function gcd(a, b) {
  a = abs(a);
  b = abs(b);
  while (b) {
    const a_next = b,
      b_next = a % b;
    a = a_next;
    b = b_next;
  }
  return a;
}

function popCount(a) {
  return a ? a.toString(2).match(/1/g).length : 0;
}

export class IntegerManager {
  constructor(jsvalManager) {
    this.jsvalManager = jsvalManager;

    // buffer of 8 bytes to hold floats/doubles
    this.buffer = new ArrayBuffer(8);
    this.view = new DataView(this.buffer);

    Object.freeze(this);
  }

  newInteger(non_neg) {
    return this.jsvalManager.newTmpJSVal([
      Boolean(non_neg),
      BigInt(0),
      BigInt(0)
    ]);
  }

  prependInteger(i, x) {
    this.jsvalManager.mutTmpJSVal(i, ([non_neg, shift_bits, tot]) => [
      non_neg,
      shift_bits + BigInt(31),
      (BigInt(x) << shift_bits) | tot
    ]);
  }

  freezeInteger(i) {
    let [non_neg, , bi] = this.jsvalManager.freezeTmpJSVal(i);
    bi = non_neg ? bi : -bi;
    return this.encode(bi);
  }

  encode(bi) {
    return Number(
      abs(bi) >> BigInt(31)
        ? BigInt(this.jsvalManager.newJSVal(bi))
        : bi << BigInt(1)
    );
  }

  decode(i) {
    const x = BigInt(i);
    return x & BigInt(1)
      ? this.jsvalManager.getJSVal(Number(x))
      : x >> BigInt(1);
  }

  smallInteger(high, low) {
    this.view.setUint32(/*offset=*/ 0, low, /*little endian=*/ true);
    this.view.setUint32(/*offset=*/ 4, high, /*little endian=*/ true);
    return this.encode(
      this.view.getBigInt64(/*offset=*/ 0, /*little endian=*/ true)
    );
  }

  wordToInteger(high, low) {
    this.view.setUint32(/*offset=*/ 0, low, /*little endian=*/ true);
    this.view.setUint32(/*offset=*/ 4, high, /*little endian=*/ true);
    const bi = this.view.getBigUint64(/*offset=*/ 0, /*little endian=*/ true);
    const n = this.encode(bi);
    return n;
  }

  integerToWord(i, ipiece) {
    const n = BigInt.asUintN(64, this.decode(i));
    this.view.setBigUint64(/*offset=*/ 0, n, /*little endian=*/ true);
    const m = this.view.getUint32(
      /*offset=*/ 4 * ipiece,
      /*little endian=*/ true
    );
    return m;
  }

  integerToInt(i) {
    return Number(BigInt.asIntN(64, this.decode(i)));
  }

  plusInteger(i0, i1) {
    return this.encode(this.decode(i0) + this.decode(i1));
  }

  minusInteger(i0, i1) {
    return this.encode(this.decode(i0) - this.decode(i1));
  }

  timesInteger(i0, i1) {
    return this.encode(this.decode(i0) * this.decode(i1));
  }

  negateInteger(i) {
    return this.encode(-this.decode(i));
  }

  eqInteger(i0, i1) {
    return this.decode(i0) === this.decode(i1);
  }

  neqInteger(i0, i1) {
    return this.decode(i0) !== this.decode(i1);
  }

  absInteger(i) {
    return this.encode(abs(this.decode(i)));
  }

  signumInteger(i) {
    const bi = this.decode(i);
    return this.encode(BigInt(bi > BigInt(0) ? 1 : bi === BigInt(0) ? 0 : -1));
  }

  leInteger(i0, i1) {
    return this.decode(i0) <= this.decode(i1);
  }

  gtInteger(i0, i1) {
    return this.decode(i0) > this.decode(i1);
  }

  ltInteger(i0, i1) {
    return this.decode(i0) < this.decode(i1);
  }

  geInteger(i0, i1) {
    return this.decode(i0) >= this.decode(i1);
  }

  quotInteger(i0, i1) {
    return this.encode(this.decode(i0) / this.decode(i1));
  }

  remInteger(i0, i1) {
    return this.encode(this.decode(i0) % this.decode(i1));
  }

  gcdInteger(i0, i1) {
    return this.encode(gcd(this.decode(i0), this.decode(i1)));
  }

  lcmInteger(i0, i1) {
    const x = this.decode(i0),
      y = this.decode(i1);
    if (!x || !y) return this.encode(BigInt(0));
    return this.encode(abs((x / gcd(x, y)) * y));
  }

  andInteger(i0, i1) {
    return this.encode(this.decode(i0) & this.decode(i1));
  }

  orInteger(i0, i1) {
    return this.encode(this.decode(i0) | this.decode(i1));
  }

  xorInteger(i0, i1) {
    return this.encode(this.decode(i0) ^ this.decode(i1));
  }

  complementInteger(i) {
    return this.encode(~this.decode(i));
  }

  shiftLInteger(i0, i1) {
    return this.encode(this.decode(i0) << BigInt(i1));
  }

  shiftRInteger(i0, i1) {
    return this.encode(this.decode(i0) >> BigInt(i1));
  }

  popCountInteger(i) {
    const a = this.decode(i);
    return a < BigInt(0) ? -popCount(-a) : popCount(a);
  }

  bitInteger(i) {
    return this.encode(BigInt(1) << BigInt(i));
  }

  hashInteger(i) {
    return Number(BigInt.asIntN(64, this.decode(i)));
  }

  powInteger(i0, i1) {
    return this.encode(this.decode(i0) ** this.decode(i1));
  }

  testBitInteger(i0, i1) {
    return Boolean((this.decode(i0) >> BigInt(i1)) & BigInt(1));
  }

  integerLog2(i) {
    const deci = this.decode(i);
    return deci > BigInt(0) ? deci.toString(2).length - 1 : -1;
  }

  // return 0 if the number is power of 2, since it seems that
  // it is internally implement as IsPowerOf2 = x & (x - 1), which is 0
  // if x IS a power of 2.
  integerIsPowerOf2(i) {
    return Number(!/^10*$/.test(this.decode(i).toString(2)));
  }

  encodeDoubleInteger(i0, i1) {
    return Number(this.decode(i0)) * 2 ** i1;
  }

  doubleFromInteger(i) {
    return Number(this.decode(i));
  }

  mul2(hi_hi, hi_lo, lo_hi, lo_lo, ipiece) {
    this.view.setInt32(/*offset=*/ 0, hi_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, hi_hi, /*littleEndian=*/ true);
    const hi = this.view.getBigUint64(/*offset=*/ 0, /*littleEndian=*/ true);

    this.view.setInt32(/*offset=*/ 0, lo_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, lo_hi, /*littleEndian=*/ true);
    const lo = this.view.getBigUint64(/*offset=*/ 0, /*littleEndian=*/ true);

    const mul = hi * lo;
    // find the correct value that is masked
    const val = Number(
      (mul >> BigInt(32 * ipiece)) & ((BigInt(1) << BigInt(32)) - BigInt(1))
    );

    return Number(val);
  }

  quotrem2_quotient(
    lhs_hi_hi,
    lhs_hi_lo,
    lhs_lo_hi,
    lhs_lo_lo,
    rhs_hi,
    rhs_lo,
    ipiece
  ) {
    this.view.setInt32(/*offset=*/ 0, lhs_hi_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, lhs_hi_hi, /*littleEndian=*/ true);
    const lhs_hi = this.view.getBigUint64(
      /*offset=*/ 0,
      /*littleEndian=*/ true
    );

    this.view.setInt32(/*offset=*/ 0, lhs_lo_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, lhs_lo_hi, /*littleEndian=*/ true);
    const lhs_lo = this.view.getBigUint64(
      /*offset=*/ 0,
      /*littleEndian=*/ true
    );
    const lhs = (lhs_hi << BigInt(64)) | lhs_lo;

    this.view.setInt32(/*offset=*/ 0, rhs_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, rhs_hi, /*littleEndian=*/ true);
    const rhs = this.view.getBigUint64(/*offset=*/ 0, /*littleEndian=*/ true);

    const quot = lhs / rhs;
    // find the correct value that is masked
    const val = Number(
      (quot >> BigInt(32 * ipiece)) & ((BigInt(1) << BigInt(32)) - BigInt(1))
    );

    return Number(val);
  }

  quotrem2_remainder(
    lhs_hi_hi,
    lhs_hi_lo,
    lhs_lo_hi,
    lhs_lo_lo,
    rhs_hi,
    rhs_lo,
    ipiece
  ) {
    this.view.setInt32(/*offset=*/ 0, lhs_hi_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, lhs_hi_hi, /*littleEndian=*/ true);
    const lhs_hi = this.view.getBigUint64(
      /*offset=*/ 0,
      /*littleEndian=*/ true
    );

    this.view.setInt32(/*offset=*/ 0, lhs_lo_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, lhs_lo_hi, /*littleEndian=*/ true);
    const lhs_lo = this.view.getBigUint64(
      /*offset=*/ 0,
      /*littleEndian=*/ true
    );
    const lhs = (lhs_hi << BigInt(64)) | lhs_lo;

    this.view.setInt32(/*offset=*/ 0, rhs_lo, /*littleEndian=*/ true);
    this.view.setInt32(/*offset=*/ 4, rhs_hi, /*littleEndian=*/ true);
    const rhs = this.view.getBigUint64(/*offset=*/ 0, /*littleEndian=*/ true);

    const rem = lhs % rhs;
    // find the correct value that is masked
    const val = Number(
      (rem >> BigInt(32 * ipiece)) & ((BigInt(1) << BigInt(32)) - BigInt(1))
    );

    return Number(val);
  }
}

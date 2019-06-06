export class IntegerManager {
  constructor(jsvalManager, heap) {
    this.jsvalManager = jsvalManager;
    this.heap = heap;

    // buffer of 8 bytes to hold floats/doubles
    this.buffer = new ArrayBuffer(8);
    this.view = new DataView(this.buffer);

    Object.seal(this);
  }
  integerToString(i, l) {
    return this.heap.newHaskellString(this.decode(i).toString(), l);
  }
  newInteger(non_neg) {
    return this.jsvalManager.newTmpJSVal(
        [ Boolean(non_neg), BigInt(0), BigInt(0) ]);
  }
  prependInteger(i, x) {
    this.jsvalManager.mutTmpJSVal(i, ([ non_neg, shift_bits, tot ]) => [
      non_neg, shift_bits + BigInt(31), (BigInt(x) << shift_bits) | tot
    ]);
  }
  freezeInteger(i) {
    let[non_neg, , bi] = this.jsvalManager.freezeTmpJSVal(i);
    bi = non_neg ? bi : -bi;
    return this.encode(bi);
  }
  abs(bi) { return bi < BigInt(0) ? -bi : bi; }
  encode(bi) {
    return Number(this.abs(bi) >> BigInt(31)
                      ? BigInt(this.jsvalManager.newJSVal(bi))
                      : bi << BigInt(1));
  }
  decode(i) {
    const x = BigInt(i);
    return x & BigInt(1) ? this.jsvalManager.getJSVal(Number(x))
                         : x >> BigInt(1);
  }

  smallInteger(high, low) { 
      this.view.setUint32(/*offset=*/0, low,  /*little endian=*/true);
      this.view.setUint32(/*offset=*/4, high, /*little endian=*/true);
      return this.encode(this.view.getBigInt64(/*offset=*/0, /*little endian=*/true));
  }

  wordToInteger(high, low) { 
      this.view.setUint32(/*offset=*/0, low, /*little endian=*/true);
      this.view.setUint32( /*offset=*/4, high, /*little endian=*/true);
      return this.encode(this.view.getBigUint64(/*offset=*/0, /*little endian=*/true));
  }

  integerToWord(i) { return Number(BigInt.asUintN(64, this.decode(i))); }
  integerToInt(i) { return Number(BigInt.asIntN(64, this.decode(i))); }
  plusInteger(i0, i1) { return this.encode(this.decode(i0) + this.decode(i1)); }
  minusInteger(i0, i1) {
    return this.encode(this.decode(i0) - this.decode(i1));
  }
  timesInteger(i0, i1) {
    return this.encode(this.decode(i0) * this.decode(i1));
  }
  negateInteger(i) { return this.encode(-this.decode(i)); }
  eqInteger(i0, i1) { return this.decode(i0) === this.decode(i1); }
  neqInteger(i0, i1) { return this.decode(i0) !== this.decode(i1); }
  absInteger(i) { return this.encode(this.abs(this.decode(i))); }
  signumInteger(i) {
    const bi = this.decode(i);
    return this.encode(BigInt(bi > BigInt(0) ? 1 : bi === BigInt(0) ? 0 : -1));
  }
  leInteger(i0, i1) { return this.decode(i0) <= this.decode(i1); }
  gtInteger(i0, i1) { return this.decode(i0) > this.decode(i1); }
  ltInteger(i0, i1) { return this.decode(i0) < this.decode(i1); }
  geInteger(i0, i1) { return this.decode(i0) >= this.decode(i1); }
  quotInteger(i0, i1) { return this.encode(this.decode(i0) / this.decode(i1)); }
  remInteger(i0, i1) { return this.encode(this.decode(i0) % this.decode(i1)); }
  andInteger(i0, i1) { return this.encode(this.decode(i0) & this.decode(i1)); }
  orInteger(i0, i1) { return this.encode(this.decode(i0) | this.decode(i1)); }
  xorInteger(i0, i1) { return this.encode(this.decode(i0) ^ this.decode(i1)); }
  complementInteger(i) { return this.encode(~this.decode(i)); }
  shiftLInteger(i0, i1) { return this.encode(this.decode(i0) << BigInt(i1)); }
  shiftRInteger(i0, i1) { return this.encode(this.decode(i0) >> BigInt(i1)); }
  hashInteger(i) { return Number(BigInt.asIntN(64, this.decode(i))); }
  powInteger(i0, i1) { return this.encode(this.decode(i0) ** this.decode(i1)); }
  testBitInteger(i0, i1) {
    return Boolean((this.decode(i0) >> BigInt(i1)) & BigInt(1));
  }

  integerLog2(i) {
    const deci = this.decode(i);
    return deci > BigInt(0) ? deci.toString(2).length - 1 : -1;
  }

  integerIsPowerOf2(i) {
    return Number(/^10*$/.test(this.decode(i).toString(2)));
  }
  encodeDoubleInteger(i0, i1) {
    return Number(this.decode(i0)) * 2 ** i1;
  }

  doubleFromInteger(i) { return Number(this.decode(i)); }

  mul2_high(hi, lo) {
    const mul = this.decode(hi) * this.decode(lo);
    return mul >> BigInt(64);
  }
  
  mul2_low(hi, lo) {
    const mul = this.decode(hi) * this.decode(lo);
    return mul & ((BigInt(1) << BigInt(64)) - BigInt(1));
  }
}

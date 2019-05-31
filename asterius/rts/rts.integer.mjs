export class IntegerManager {
  constructor(jsvalManager, heap) {
    this.jsvalManager = jsvalManager;
    this.heap = heap;
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
    return x & BigInt(1) ? BigInt(this.jsvalManager.newJSVal(Number(x)))
                         : x >> BigInt(1);
  }
  smallInteger(x) { return this.encode(BigInt(x)); }
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
  integerLogBase(i, b) {
    const bi = this.decode(i);
    console.log("bi: " , bi);

    const dec = this.decode(bi);
    console.log("dec:", dec);
    return bi > BigInt(0)
               ? dec.toString(Number(this.decode(b))).length - 1
               : -1;
  }
  integerIsPowerOf2(i) {
    return Number(/^10*$/.test(this.decode(i).toString(2)));
  }
  encodeDoubleInteger(i0, i1) {
    return Number(this.decode(i0)) * 2 ** i1;
  }

  decodeDoubleInteger(d) {
      console.error("called integer: decodeDoubleInteger: ", d);
    const [, sgn, i, f] = /^(-?)([01]+)\.?([01]*)$/.exec(d.toString(2));
    let s = i + f, acc = BigInt(0), e = f ? -f.length : 0;
    while (s) {
      const c = s.slice(0, 53);
      s = s.slice(c.length);
      acc = (acc << BigInt(c.length)) | BigInt(Number.parseInt(c, 2));
    }
    if (acc !== BigInt(0))
      while ((acc & BigInt(1)) === BigInt(0)) {
        acc = acc >> BigInt(1);
        e += 1;
      }
    return [ sgn ? -acc : acc, e ];
  }
  doubleFromInteger(i) { return Number(this.decode(i)); }
}

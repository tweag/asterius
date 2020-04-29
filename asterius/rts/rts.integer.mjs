export class IntegerManager {
  constructor() {
    // buffer of 8 bytes to hold floats/doubles
    this.view = new DataView(new ArrayBuffer(8));

    Object.freeze(this);
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

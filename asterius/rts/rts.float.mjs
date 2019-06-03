// Implements primitives from primFloat.c
export class FloatCBits {
  constructor(memory) {
    this.memory = memory;
    /* Constants copy-pasted by running this C program:
        #include <float.h>
        #include <stdio.h>

        #define PRINT(name) printf("this." #name " = %d;\n", name);
        int main() {
            PRINT(FLT_MIN_EXP); PRINT(FLT_MANT_DIG);
            PRINT(DBL_MIN_EXP); PRINT(DBL_MANT_DIG);
            return 0;
        }
        Other code copy-pasted from C calculations.
        */

    this.FLT_MIN_EXP = -125;
    this.FLT_MANT_DIG = 24;
    this.DBL_MIN_EXP = -1021;
    this.DBL_MANT_DIG = 53;

    this.MY_DMINEXP = this.DBL_MIN_EXP - this.DBL_MANT_DIG - 1;
    /* DMINEXP is defined in values.h on Linux (for example) */
    this.DHIGHBIT = 0x00100000;
    this.DMSBIT = 0x80000000;

    this.MY_FMINEXP = this.FLT_MIN_EXP - this.FLT_MANT_DIG - 1;
    this.FHIGHBIT = 0x00800000;
    this.FMSBIT = 0x80000000;

    // buffer of 8 bytes to hold floats/doubles
    this.buffer = new ArrayBuffer(8);
    this.view = new DataView(this.buffer);

    Object.seal(this);
  }

  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/is
  isFloatNegativeZero(x) {
    return Object.is(-0, x);
  }

  isFloatNaN(x) {
    return x != x;
  }

  isDoubleNaN(x) {
    return x != x;
  }

  // Remember, floats have 3 states: {finite, infinite, NaN}.
  isFloatInfinite(x) {
    return !isFinite(x) && !this.isFloatNaN(x);
  }

  isDoubleInfinite(x) {
    return !isFinite(x) && !this.isDoubleNaN(x);
  }

  // extract the mantissa from the little endian representation of the bits
  // of the float.
  // little endian: <0A 0B> stored as mem[p] = 0A, mem[p + 1] = OB
  floatMantissaFromBits(bits) {
    const mask = (1 << 23) - 1;
    return bits & mask;
  }

  // extract the exponent from the little endian representation of the bits
  // of the float.
  floatExponentFromBits(bits) {
    const mask = (1 << 8) - 1;
    const sign = this.floatSignFromBits(bits);
    return ((bits >> 23) & mask) ^ (sign << 31);
  }

  floatSignFromBits(bits) {
    return bits >> 31;
  }

  doubleMantissaFromBits(bits) {
    const mask = (BigInt(1) << BigInt(52)) - BigInt(1);
    return bits & mask;
  }

  doubleExponentFromBits(bits) {
    const mask = BigInt((1 << 11) - 1);
    const sign = this.doubleSignFromBits(bits);

    const bitsNoSign = bits ^ (sign << BigInt(63));
    return (bitsNoSign >> BigInt(52)) & mask;
  }

  doubleSignFromBits(bits) {
    return bits >> BigInt(63);
  }

  // Check if a double is denormal.
  isDoubleDenormalized(x) {
    const bits = this.DoubleToIEEE(x);

    console.log("x: %f | bits: %s", x, bits.toString(16));
    const exponent = this.doubleExponentFromBits(bits);
    console.log("x: %f | exponent: %s", x, exponent.toString(16));
    const mantissa = this.doubleMantissaFromBits(bits);
    console.log("x: %f | mantissa: %s", x, mantissa.toString(16));
    return exponent === BigInt(0) && mantissa !== BigInt(0);
  }

  isFloatDenormalized(x) {
    const bits = this.FloatToIEEE(x);
    const exponent = this.floatExponentFromBits(bits);
    const mantissa = this.floatMantissaFromBits(bits);
    return exponent === 0 && mantissa !== 0;
  }

  // Does it really make sense to have two functions?  probably not...
  isDoubleNegativeZero(x) {
    return Object.is(-0, x);
  }

  FloatToIEEE(f) {
    this.view.setFloat32(0, f);
    return this.view.getUint32(0);
  }

  DoubleToIEEE(d) {
    this.view.setFloat64(0, d);
    return this.view.getBigUint64(0);
  }

  // return two 32-bit integers, [low, high] from a 64 bit double;
  DoubleTo2Int(d) {
    this.view.setFloat64(0, d);
    const low = this.view.getUint32(0);
    const high = this.view.getUint32(/*offset=*/ 4);
    return [low, high];
  }

  IEEEToFloat(ieee) {
    this.view.setUint32(0, ieee);
    return this.view.getFloat32(0);
  }

  IEEEToDouble(ieee) {
    this.view.setUint64(0, ieee);
    return this.view.getFloat64(0);
  }

  __decodeFloat_Int(manp, expp, f) {
    console.error("inside decodeFloat\n");
    // https://github.com/ghc/ghc/blob/610ec224a49e092c802a336570fd9613ea15ef3c/rts/StgPrimFloat.c#L215
    let man, exp, sign;
    let high = this.FloatToIEEE(f);

    if ((high & ~this.FMSBIT) == 0) {
      man = 0;
      exp = 0;
    } else {
      exp = ((high >> 23) & 0xff) + this.MY_FMINEXP;
      high &= this.FHIGHBIT - 1;
      if (exp != this.MY_FMINEXP)
        /* don't add hidden bit to denorms */
        high |= this.FHIGHBIT;
      else {
        exp += 1;
        /* A denorm, normalize the mantissa */
        while (!(high & this.FHIGHBIT)) {
          high <<= 1;
          exp -= 1;
        }
      }

      man = high;
      if (sign < 0) {
        man = -man;
      }
    }

    // TODO: double check! Is this i32 or i64? I suspect it is i32.
    this.memory.i64Store(manp, man);
    this.memory.i64Store(expp, exp);
  }

  // https://github.com/ghc/ghc/blob/610ec224a49e092c802a336570fd9613ea15ef3c/rts/StgPrimFloat.c
  // From StgPrimFloat.c
  // returns [man_sign, man_high,  man_low, exp]
  __decodeDouble_2IntJS(dbl) {
    let sign, iexp, man_low, man_high, man_sign;
    const ints = this.DoubleTo2Int(dbl);
    let low = ints[1];
    let high = ints[0];
    let exp = 0;

    if (low == 0 && (high & ~this.DMSBIT) == 0) {
      man_low = 0;
      man_high = 0;
      man_sign = 0;
      iexp = 0;
    } else {
      iexp = ((high >> 20) & 0x7ff) + this.MY_DMINEXP;

      // unsigned to signed conversion
      this.view.setUint32(0, high);
      sign = this.view.getInt32(0);

      high &= this.DHIGHBIT - 1;
      if (iexp != this.MY_DMINEXP)
        /* don't add hidden bit to denorms */
        high |= this.DHIGHBIT;
      else {
        iexp++;
        /* A denorm, normalize the mantissa */
        while (!(high & this.DHIGHBIT)) {
          high <<= 1;
          if (low & this.DMSBIT) high++;
          low <<= 1;
          iexp--;
        }
      }
      exp = iexp;
      man_low = low;
      man_high = high;
      man_sign = sign < 0 ? -1 : 1;
    }

    return [man_sign, man_high, man_low, exp];
  }

  // From GHC/Integer/Type.hs
  decodeDoubleInteger(d) {
    const out = this.__decodeDouble_2IntJS(d);
    const man_sign = out[0];
    const man_high = out[1];
    const man_low = out[2];
    const exp = out[3];

    const acc =
      BigInt(man_sign) *
      (BigInt(man_high) * (BigInt(1) << BigInt(32)) + BigInt(man_low));
    return [acc, exp];
  }
}

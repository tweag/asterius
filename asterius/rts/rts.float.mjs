import { Memory } from "./rts.memory.mjs";

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

    this.FLT_HIDDEN = 0x800000;
    this.FLT_POWER2 = 0x1000000;

    this.DBL_HIDDEN = 0x100000;
    this.DBL_POWER2 = 0x200000;

    this.LTOP_BIT = 0x80000000;

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

  isFloatFinite(x) {
    return isFinite(x);
  }

  isDoubleFinite(x) {
    return isFinite(x);
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
    return ((bits ^ (sign << 31)) >>> 23) & mask;
  }

  floatSignFromBits(bits) {
    return bits >>> 31;
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

    const exponent = this.doubleExponentFromBits(bits);
    const mantissa = this.doubleMantissaFromBits(bits);
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
    this.view.setInt32(0, ieee);
    return this.view.getFloat32(0);
  }

  IEEEToDouble(ieee) {
    this.view.setBigInt64(0, ieee);
    return this.view.getFloat64(0);
  }

  __decodeFloat_Int(manp, expp, f) {
    // https://github.com/ghc/ghc/blob/610ec224a49e092c802a336570fd9613ea15ef3c/rts/StgPrimFloat.c#L215
    let man, exp, sign;
    let high = this.FloatToIEEE(f);

    if ((high & ~this.FMSBIT) == 0) {
      man = 0;
      exp = 0;
    } else {
      exp = ((high >>> 23) & 0xff) + this.MY_FMINEXP;

      // [sign = high] with a [uint -> int] conversion.
      this.view.setUint32(0, high);
      sign = this.view.getInt32(0);

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
      iexp = ((high >>> 20) & 0x7ff) + this.MY_DMINEXP;

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

  __decodeDouble_2Int(p_man_sign, p_man_high, p_man_low, p_exp, dbl) {
    const [man_sign, man_high, man_low, exp] = this.__decodeDouble_2IntJS(dbl);
    this.memory.dataView.setBigInt64(Memory.unTag(p_man_sign), BigInt(man_sign), true);
    this.memory.i64Store(p_man_high, man_high);
    this.memory.i64Store(p_man_low, man_low);
    this.memory.i64Store(p_exp, exp);
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

  // from cbits/primFloat
  rintFloat(f) {
    const bits = this.FloatToIEEE(f);
    let fexp = BigInt(this.floatExponentFromBits(bits));
    let fman = BigInt(this.floatMantissaFromBits(bits));
    let fsign = BigInt(this.floatSignFromBits(bits));

    // put back the float together
    const reconstructFloat = () => {
      return this.IEEEToFloat(
        Number((fsign << BigInt(31)) | (fexp << BigInt(23)) | fman)
      );
    };

    /* if real exponent > 22, it's already integral, infinite or nan */
    if (fexp > 149) {
      /* 22 + 127 */
      return f;
    }
    if (fexp < 126) {
      /* (-1) + 127, abs(f) < 0.5 */
      /* only used for rounding to Integral a, so don't care about -0.0 */
      return 0.0;
    }
    /* 0.5 <= abs(f) < 2^23 */
    /// let half, mask, mant, frac;
    const half = BigInt(1) << (BigInt(149) - fexp); /* bit for 0.5 */
    const mask = BigInt(2) * half - BigInt(1); /* fraction bits */
    let mant = fman | BigInt(this.FLT_HIDDEN); /* add hidden bit */
    let frac = mant & mask; /* get fraction */
    mant ^= frac; /* truncate mantissa */

    if (frac < half || (frac == half && (mant & (BigInt(2) * half)) == 0)) {
      /* this means we have to truncate */
      if (mant == 0) {
        /* f == ±0.5, return 0.0 */
        return 0.0;
      } else {
        /* remove hidden bit and set mantissa */
        // u.ieee.mantissa = mant ^ FLT_HIDDEN;
        fman = mant ^ BigInt(this.FLT_HIDDEN);
        return reconstructFloat();
      }
    } else {
      /* round away from zero, increment mantissa */
      mant += BigInt(2) * half;
      if (mant == this.FLT_POWER2) {
        /* next power of 2, increase exponent and set mantissa to 0 */
        fman = BigInt(0);
        fexp += BigInt(1);
        return reconstructFloat();
      } else {
        /* remove hidden bit and set mantissa */
        fman = mant ^ BigInt(this.FLT_HIDDEN);
        return reconstructFloat();
      }
    }
  }

  rintDouble(d) {
    // Code stolen from cbits/primFloat.
    const bits = this.DoubleToIEEE(d);
    let exp = this.doubleExponentFromBits(bits);
    let manFull = this.doubleMantissaFromBits(bits);
    this.view.setBigUint64(0, manFull, /*little endian=*/ true);
    let mant1 = BigInt(this.view.getUint32(0, /*little endian=*/ true));
    let mant0 = BigInt(this.view.getUint32(4, /*little endian=*/ true));
    let sign = this.doubleSignFromBits(bits);

    // put back the double together
    const reconstructDouble = () => {
      this.view.setInt32(0, Number(mant1), true);
      this.view.setInt32(4, Number(mant0), true);
      const mantFull = this.view.getBigUint64(0, true);

      const bits = (sign << BigInt(63)) | (exp << BigInt(52)) | mantFull;
      const n = Number(this.IEEEToDouble(bits));

      return n;
    };

    // union stg_ieee754_dbl u;
    // u.d = d;
    /* if real exponent > 51, it's already integral, infinite or nan */
    // if (u.ieee.exponent > 1074) /* 51 + 1023 */
    if (exp > 1074) {
      /* 51 + 1023 */
      return d;
    }
    // if (u.ieee.exponent < 1022)  /* (-1) + 1023, abs(d) < 0.5 */
    if (exp < 1022) {
      /* (-1) + 1023, abs(d) < 0.5 */
      /* only used for rounding to Integral a, so don't care about -0.0 */
      return 0.0;
    }
    // unsigned int half, mask, mant, frac;
    if (exp < 1043) {
      /* 20 + 1023, real exponent < 20 */
      /* the fractional part meets the higher part of the mantissa */
      const half = BigInt(1) << (BigInt(1042) - exp); /* bit for 0.5 */
      const mask = BigInt(2) * half - BigInt(1); /* fraction bits */
      let mant = mant0 | BigInt(this.DBL_HIDDEN); /* add hidden bit */
      const frac = mant & mask; /* get fraction */
      mant ^= frac; /* truncate mantissa */

      if (
        frac < half ||
        (frac == half &&
        mant1 == 0 /* a tie */ &&
          (mant & (BigInt(2) * half)) == 0)
      ) {
        /* truncate */
        if (mant == 0) {
          /* d = ±0.5, return 0.0 */
          return 0.0;
        }
        /* remove hidden bit and set mantissa */
        mant0 = mant ^ BigInt(this.DBL_HIDDEN);
        mant1 = BigInt(0);

        // reassemble double here
        // return u.d;
        return reconstructDouble();
      } /* round away from zero */ else {
        /* zero low mantissa bits */
        mant1 = BigInt(0);
        /* increment integer part of mantissa */
        mant += BigInt(2) * half;
        if (mant == this.DBL_POWER2) {
          /* power of 2, increment exponent and zero mantissa */
          mant0 = BigInt(0);
          exp += BigInt(1);
          // reassamble
          return reconstructDouble();
        }
        /* remove hidden bit */
        mant0 = mant ^ BigInt(this.DBL_HIDDEN);
        // reassemble
        return reconstructDouble();
      }
    } else {
      /* 20 <= real exponent < 52, fractional part entirely in mantissa1 */
      const half = BigInt(1) << (BigInt(1074) - exp); /* bit for 0.5 */
      const mask = BigInt(2) * half - BigInt(1); /* fraction bits */
      let mant = mant1; /* no hidden bit here */
      let frac = mant & mask; /* get fraction */
      mant ^= frac; /* truncate mantissa */

      if (
        frac < half ||
        (frac == half /* tie */ &&
          (half == this.LTOP_BIT
            ? mant0 & 1 /* yuck */
            : mant & (2 * half)) == 0)
      ) {
        /* truncate */
        mant1 = mant;
        return reconstructDouble();
      } else {
        /* round away from zero */
        /* increment mantissa */
        mant += BigInt(2) * half;
        mant1 = mant;

        // ORIGINAL CODE: if (mant == 0) { where they exploit 32-bit unsigned
        // representation.
        if (mant % (BigInt(1) << BigInt(32)) == 0) {
          /* low part of mantissa overflowed */
          /* increment high part of mantissa */
          mant = mant0 + BigInt(1);
          if (mant == this.DBL_HIDDEN) {
            /* hit power of 2 */
            /* zero mantissa */
            mant0 = BigInt(0);
            /* and increment exponent */
            exp += BigInt(1);

            return reconstructDouble();
          } else {
            u.ieee.mantissa0 = mant;
            return reconstructDouble();
          }
        } else {
          return reconstructDouble();
        }
      }
    }
  }
}

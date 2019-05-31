// Implements primitives from primFloat.c
export class FloatCBits {
    constructor(memory) {
        this.memory  = memory;
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

        this.MY_DMINEXP = ((this.DBL_MIN_EXP) - (this.DBL_MANT_DIG) - 1);
        /* DMINEXP is defined in values.h on Linux (for example) */
        this.DHIGHBIT = 0x00100000;
        this.DMSBIT = 0x80000000;

        this.MY_FMINEXP = ((this.FLT_MIN_EXP) - (this.FLT_MANT_DIG) - 1);
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
        return ((bits >> 23) &  mask) ^ (sign << 31);
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
        return bitsNoSign >> BigInt(52) & mask;
    }

    doubleSignFromBits(bits) {
        return bits >> BigInt(63);
    }



    signManExpToDouble(sign, man, exp) {
	return sign << 63 | exp << 52 | man;
    }
    
    // Check if a double is denormal.
    isDoubleDenormalized(x) { 
        const bits = this.DoubleToIEEE(x);
        
        console.log("x: %f | bits: %s", x, bits.toString(16));
        const exponent = this.doubleExponentFromBits(bits);
        console.log("x: %f | exponent: %s",x,  exponent.toString(16));
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
        const high = this.view.getUint32(/*offset=*/4);
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
            high &= this.FHIGHBIT-1;
            if (exp != this.MY_FMINEXP) /* don't add hidden bit to denorms */
                high |= this.FHIGHBIT;
            else {
                exp += 1;
                /* A denorm, normalize the mantissa */
                while (! (high & this.FHIGHBIT)) {
                    high <<= 1;
                    exp -= 1;
                }
            }

            man = high;
            if (sign < 0) {
                man = - man;
            }
        }

        // TODO: double check! Is this i32 or i64? I suspect it is i32.
        this.memory.i64Store(manp, man);
        this.memory.i64Store(expp, exp);
    }

    // From StgPrimFloat.c
    __decodeDouble_2Int(man_signp, man_highp, man_lowp, expp, dbl) {

        const dbl2i = this.DoubleTo2Int(dbl);
        let low = dbl2i[0];
        let high = dbl2i[1];
        let iexp = 0, sign = 0;

        if (low == 0 && (high & ~this.DMSBIT) == 0) {
            this.memory.i32Store(man_lowp, 0);
            this.memory.i32Store(man_highp, 0);
            this.memory.i32Store(expp, 0);
            // *man_low = 0;
            // *man_high = 0;
            // *exp = 0L;
        } else {
            iexp = ((high >> 20) & 0x7ff) + this.MY_DMINEXP;
            sign = high;

            high &= this.DHIGHBIT-1;
            if (iexp != this.MY_DMINEXP) {
                /* don't add hidden bit to denorms */
                high |= this.DHIGHBIT;
            } else {
                iexp++;
                /* A denorm, normalize the mantissa */
                while (! (high & this.DHIGHBIT)) {
                    high <<= 1;
                    if (low & this.DMSBIT) {
                        high++;
                    }
                    low <<= 1;
                    iexp--;
                }
            }
 
            this.memory.i32Store(expp, iexp);
            this.memory.i32Store(man_lowp, low);
            this.memory.i32Store(man_highp, high);
            this.memory.i32Store(man_signp, (sign < 0) ? (-1) : 1);
            // *exp = (I_) iexp;
            // *man_low = low;
            // *man_high = high;
            // *man_sign = (sign < 0) ? -1 : 1;
        }

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
            // 0x7ff is a 64 bit value
            iexp = ((high >> 20) & 0x7ff) + this.MY_DMINEXP;
            sign = high;

            high &= this.DHIGHBIT-1;
            if (iexp != this.MY_DMINEXP) /* don't add hidden bit to denorms */
                high |= this.DHIGHBIT;
            else {
                iexp++;
                /* A denorm, normalize the mantissa */
                while (! (high & this.DHIGHBIT)) {
                    high <<= 1;
                    if (low & this.DMSBIT)
                        high++;
                    low <<= 1;
                    iexp--;
                }
            }
            exp = iexp;
            man_low = low;
            man_high = high;
            man_sign = (sign < 0) ? -1 : 1;
        }
        // TODO: just use call by reference
        return [man_sign, man_high, man_low, exp];
    }

    // From GHC/Integer/Type.hs
    decodeDoubleInteger(d) {
        const out = this.__decodeDouble_2IntJS(d);
        const man_sign = out[0];
        const man_high = out[1];
        const man_low = out[2];
        const exp =  out[3];


        const acc = 
            BigInt(man_sign) * (BigInt(man_high) * (BigInt(1) << BigInt(32)) + BigInt(man_low));
        console.log("FROM JS **d: ", d, " **acc: ", acc,  "  **exp: ", exp);
        return [acc, exp]

    }

    rintDouble(d) {
        // Code stolen from cbits/float.
        const bits = this.DoubleToIEEE(d);
        let exp = this.doubleExponentFromBits(bits);
        let manFull = this.doubleMantissaFromBits(bits);
        let mant1 = manFull & (BigInt(1) << BigInt(32) - BigInt(1));
        let mant0 = (manFull >> BigInt(32));
        let sign = this.doubleSignFromBits(bits);


        // put back the double together
        function reconstructDouble() {
            let mantFull = (mant0 << BigInt(32)) | mant1;
            return Number((sign << BigInt(63)) | (exp << BigInt(52)) | mantFull);
            

        }
        // union stg_ieee754_dbl u;
        // u.d = d;
        /* if real exponent > 51, it's already integral, infinite or nan */
        // if (u.ieee.exponent > 1074) /* 51 + 1023 */
        if (exp > 1074) /* 51 + 1023 */
        {
            return d;
        }
        // if (u.ieee.exponent < 1022)  /* (-1) + 1023, abs(d) < 0.5 */
        if (exp < 1022)  /* (-1) + 1023, abs(d) < 0.5 */
        {
            /* only used for rounding to Integral a, so don't care about -0.0 */
            return 0.0;
        }
        // unsigned int half, mask, mant, frac;
        if (exp < 1043) /* 20 + 1023, real exponent < 20 */
        {
            /* the fractional part meets the higher part of the mantissa */
            const half = BigInt(1) << (BigInt(1042) - exp);   /* bit for 0.5 */
            const mask = BigInt(2)*half - BigInt(1);                      /* fraction bits */
            let mant = man0 | DBL_HIDDEN;   /* add hidden bit */
            const frac = mant & mask;                     /* get fraction */
            mant ^= frac;                           /* truncate mantissa */
            if ((frac < half) ||
                ((frac == half) && (man1 == 0)  /* a tie */
                    && ((mant & (2*half)) == 0)))
            {
                /* truncate */
                if (mant == 0)
                {
                    /* d = ±0.5, return 0.0 */
                    return 0.0;
                }
                /* remove hidden bit and set mantissa */
                mant0 = mant ^ DBL_HIDDEN;
                mant1 = 0;

                // reassemble double here
                // return u.d;
                return reconstructDouble();
           }
            else    /* round away from zero */
            {
                /* zero low mantissa bits */
                mant1 = 0;
                /* increment integer part of mantissa */
                mant += 2*half;
                if (mant == DBL_POWER2)
                {
                    /* power of 2, increment exponent and zero mantissa */
                    mant0 = 0;
                    exp += 1;
                    // reassamble
                }
                /* remove hidden bit */
                mant0 = mant ^ DBL_HIDDEN;
                // reassemble
                return -42;
            }
        }
        else
        {
            /* 20 <= real exponent < 52, fractional part entirely in mantissa1 */
            const half = BigInt(1) << (BigInt(1074) - exp);   /* bit for 0.5 */
            const mask = BigInt(2)*half - BigInt(1);                      /* fraction bits */
            let mant = mant1;                /* no hidden bit here */
            let frac = mant & mask;                     /* get fraction */
            mant ^= frac;                           /* truncate mantissa */
            if ((frac < half) ||
                ((frac == half) &&                  /* tie */
                (((half == LTOP_BIT) ? (u.ieee.mantissa0 & 1)  /* yuck */
                                    : (mant & (2*half)))
                                            == 0)))
            {
                /* truncate */
                mant1 = mant;
                return reconstructDouble();
            }
            else
            {
                /* round away from zero */
                /* increment mantissa */
                mant += BigInt(2)*half;
                mant1 = mant;
                if (mant == 0)
                {
                    /* low part of mantissa overflowed */
                    /* increment high part of mantissa */
                    mant = u.ieee.mantissa0 + 1;
                    if (mant == DBL_HIDDEN)
                    {
                        /* hit power of 2 */
                        /* zero mantissa */
                        mant0 = 0;
                        /* and increment exponent */
                        exp += BigInt(1);
                        return reconstructDouble();
                    }
                    else
                    {
                        u.ieee.mantissa0 = mant;
                        return reconstructDouble();
                    }
                }
                else
                {
                    return reconstructDouble();
                }
            }
        }
    }
}

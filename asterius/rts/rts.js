"use strict";

(() => {
  const __asterius_root =
    (typeof self === "object" && self.self === self && self) ||
    (typeof global === "object" && global.global === global && global) ||
    this;

  __asterius_root.newAsteriusInstance = async req => {
    const __asterius_bigints = [,],
      __asterius_staging_bigints = [,];
    function __asterius_bigint_abs(bi) {
      return bi < BigInt(0) ? -bi : bi;
    }
    function __asterius_bigint_decode(i) {
      const x = BigInt(i);
      return x & BigInt(1)
        ? __asterius_bigints[x >> BigInt(1)]
        : x >> BigInt(1);
    }
    function __asterius_bigint_encode(bi) {
      return Number(
        __asterius_bigint_abs(bi) >> BigInt(52)
          ? (BigInt(__asterius_bigints.push(bi) - 1) << BigInt(1)) | BigInt(1)
          : bi << BigInt(1)
      );
    }
    function __asterius_number_decomp(d) {
      const [, sgn, i, f] = /^(-?)([01]+)\.?([01]*)$/.exec(d.toString(2));
      let s = i + f,
        acc = BigInt(0),
        e = f ? -f.length : 0;
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
      return [sgn ? -acc : acc, e];
    }
    const __asterius_stdio_bufs = [, "", ""];
    let __asterius_debug_log_enabled = true;
    function __asterius_debug_log_info(msg) {
      if (__asterius_debug_log_enabled) console.log("[INFO] " + msg);
    }
    let __asterius_wasm_instance = null,
      __asterius_mem_cap = null,
      __asterius_mem_size = null,
      __asterius_last_mblock = null,
      __asterius_last_block = null;
    function __asterius_show_I(x) {
      return x.toString(16).padStart(8, "0");
    }
    function __asterius_show_I64(lo, hi) {
      return (
        "0x" +
        (__asterius_show_I(hi) + __asterius_show_I(lo))
          .replace(/^0+/, "")
          .padStart(8, "0")
      );
    }
    function __asterius_make_symbol_lookup_table(sym_map) {
      let tbl = {};
      for (const [k, v] of Object.entries(sym_map)) tbl[v & 0xffffffff] = k;
      return tbl;
    }
    const __asterius_statics_lookup_table = __asterius_make_symbol_lookup_table(
        req.staticsSymbolMap
      ),
      __asterius_function_lookup_table = __asterius_make_symbol_lookup_table(
        req.functionSymbolMap
      );
    function __asterius_show_func_sym(x) {
      return __asterius_function_lookup_table[x & 0xffffffff];
    }
    function __asterius_show_I64_with_sym(lo, hi) {
      switch (hi) {
        case 2097143:
          return (
            __asterius_show_I64(lo, hi) +
            (__asterius_statics_lookup_table[lo]
              ? "(" + __asterius_statics_lookup_table[lo] + ")"
              : "")
          );
        case 2097133:
          return (
            __asterius_show_I64(lo, hi) +
            (__asterius_function_lookup_table[lo]
              ? "(" + __asterius_function_lookup_table[lo] + ")"
              : "")
          );
        default:
          return __asterius_show_I64(lo, hi);
      }
    }
    const __asterius_jsffi_JSRefs = [,],
      __asterius_jsffi_JSRef_revs = new Map();
    function __asterius_jsffi_newJSRef(e) {
      let i = __asterius_jsffi_JSRef_revs.get(e);
      if (!i) {
        i = __asterius_jsffi_JSRefs.push(e) - 1;
        __asterius_jsffi_JSRef_revs.set(e, i);
      }
      return i;
    }
    function __asterius_jsffi_newTempJSRef(e) {
      return __asterius_jsffi_JSRefs.push(e) - 1;
    }
    function __asterius_jsffi_mutTempJSRef(i, f) {
      __asterius_jsffi_JSRefs[i] = f(__asterius_jsffi_JSRefs[i]);
    }
    function __asterius_jsffi_freezeTempJSRef(i) {
      const e = __asterius_jsffi_JSRefs[i];
      delete __asterius_jsffi_JSRefs[i];
      return e;
    }
    const __asterius_SPT = [,];
    function __asterius_newStablePtr(obj) {
      return __asterius_SPT.push(obj) - 1;
    }
    function __asterius_deRefStablePtr(sp) {
      return __asterius_SPT[sp];
    }
    function __asterius_freeStablePtr(sp) {
      delete __asterius_SPT[sp];
    }
    function __asterius_memory_trap(p_lo, p_hi) {
      if (p_hi !== 2097143) {
        throw new WebAssembly.RuntimeError(
          "[ERROR] Memory trap caught invalid memory access at " +
            __asterius_show_I64(p_lo, p_hi)
        );
      }
    }
    const __asterius_jsffi_instance = {
      JSRefs: __asterius_jsffi_JSRefs,
      newJSRef: __asterius_jsffi_newJSRef,
      newTempJSRef: __asterius_jsffi_newTempJSRef,
      mutTempJSRef: __asterius_jsffi_mutTempJSRef,
      freezeTempJSRef: __asterius_jsffi_freezeTempJSRef,
      makeHaskellCallback: s => () => {
        const cap = req.staticsSymbolMap.MainCapability,
          export_funcs = __asterius_wasm_instance.exports;
        export_funcs.rts_evalIO(cap, __asterius_deRefStablePtr(s), 0);
      },
      makeHaskellCallback1: s => ev => {
        const cap = req.staticsSymbolMap.MainCapability,
          export_funcs = __asterius_wasm_instance.exports;
        export_funcs.rts_evalIO(
          cap,
          export_funcs.rts_apply(
            cap,
            __asterius_deRefStablePtr(s),
            export_funcs.rts_mkInt(cap, __asterius_jsffi_newJSRef(ev))
          ),
          0
        );
      },
      Integer: {
        mkInteger_init: non_neg =>
          __asterius_staging_bigints.push([
            Boolean(non_neg),
            BigInt(0),
            BigInt(0)
          ]) - 1,
        mkInteger_prepend: (i, x) => {
          __asterius_staging_bigints[i][2] |=
            BigInt(x) << __asterius_staging_bigints[i][1];
          __asterius_staging_bigints[i][1] += BigInt(31);
          return i;
        },
        mkInteger_finalize: i => {
          const bi = __asterius_staging_bigints[i][0]
            ? __asterius_staging_bigints[i][2]
            : -__asterius_staging_bigints[i][2];
          delete __asterius_staging_bigints[i];
          return __asterius_bigint_encode(bi);
        },
        smallInteger: x => __asterius_bigint_encode(BigInt(x)),
        wordToInteger: x => __asterius_bigint_encode(BigInt(x)),
        integerToWord: i =>
          Number(BigInt.asUintN(64, __asterius_bigint_decode(i))),
        integerToInt: i =>
          Number(BigInt.asIntN(64, __asterius_bigint_decode(i))),
        plusInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) + __asterius_bigint_decode(i1)
          ),
        minusInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) - __asterius_bigint_decode(i1)
          ),
        timesInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) * __asterius_bigint_decode(i1)
          ),
        negateInteger: i =>
          __asterius_bigint_encode(-__asterius_bigint_decode(i)),
        eqInteger: (i0, i1) =>
          __asterius_bigint_decode(i0) === __asterius_bigint_decode(i1),
        neqInteger: (i0, i1) =>
          __asterius_bigint_decode(i0) !== __asterius_bigint_decode(i1),
        absInteger: i =>
          __asterius_bigint_encode(
            __asterius_bigint_abs(__asterius_bigint_decode(i))
          ),
        signumInteger: i => {
          const bi = __asterius_bigint_decode(i);
          return __asterius_bigint_encode(
            BigInt(bi > BigInt(0) ? 1 : bi === BigInt(0) ? 0 : -1)
          );
        },
        leInteger: (i0, i1) =>
          __asterius_bigint_decode(i0) <= __asterius_bigint_decode(i1),
        gtInteger: (i0, i1) =>
          __asterius_bigint_decode(i0) > __asterius_bigint_decode(i1),
        ltInteger: (i0, i1) =>
          __asterius_bigint_decode(i0) < __asterius_bigint_decode(i1),
        geInteger: (i0, i1) =>
          __asterius_bigint_decode(i0) >= __asterius_bigint_decode(i1),
        divInteger: (i0, i1) => {
          const bi0 = __asterius_bigint_decode(i0),
            bi1 = __asterius_bigint_decode(i1);
          return __asterius_bigint_encode(
            bi0 > BigInt(0) && bi1 < BigInt(0)
              ? (bi0 - BigInt(1)) / bi1 - BigInt(1)
              : bi0 < BigInt(0) && bi1 > BigInt(0)
              ? (bi0 + BigInt(1)) / bi1 - BigInt(1)
              : bi0 / bi1
          );
        },
        modInteger: (i0, i1) => {
          const bi0 = __asterius_bigint_decode(i0),
            bi1 = __asterius_bigint_decode(i1);
          return __asterius_bigint_encode(
            bi0 > BigInt(0) && bi1 < BigInt(0)
              ? ((bi0 - BigInt(1)) % bi1) + bi1 + BigInt(1)
              : bi0 < BigInt(0) && bi1 > BigInt(0)
              ? ((bi0 + BigInt(1)) % bi1) + bi1 - BigInt(1)
              : bi0 % bi1
          );
        },
        quotInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) / __asterius_bigint_decode(i1)
          ),
        remInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) % __asterius_bigint_decode(i1)
          ),
        encodeFloatInteger: (i0, i1) =>
          Number(__asterius_bigint_decode(i0)) * 2 ** i1,
        decodeFloatInteger_m: d =>
          __asterius_bigint_encode(__asterius_number_decomp(d)[0]),
        decodeFloatInteger_n: d => __asterius_number_decomp(d)[1],
        floatFromInteger: i => Number(__asterius_bigint_decode(i)),
        encodeDoubleInteger: (i0, i1) =>
          Number(__asterius_bigint_decode(i0)) * 2 ** i1,
        decodeDoubleInteger_m: d =>
          __asterius_bigint_encode(__asterius_number_decomp(d)[0]),
        decodeDoubleInteger_n: d => __asterius_number_decomp(d)[1],
        doubleFromInteger: i => Number(__asterius_bigint_decode(i)),
        andInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) & __asterius_bigint_decode(i1)
          ),
        orInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) | __asterius_bigint_decode(i1)
          ),
        complementInteger: i =>
          __asterius_bigint_encode(~__asterius_bigint_decode(i)),
        shiftLInteger: (i0, i1) =>
          __asterius_bigint_encode(__asterius_bigint_decode(i0) << BigInt(i1)),
        shiftRInteger: (i0, i1) =>
          __asterius_bigint_encode(__asterius_bigint_decode(i0) >> BigInt(i1)),
        testBitInteger: (i0, i1) =>
          Boolean((__asterius_bigint_decode(i0) >> BigInt(i1)) & BigInt(1)),
        hashInteger: i =>
          Number(BigInt.asIntN(64, __asterius_bigint_decode(i))),
        integerLogBase: (b, i) => {
          const bi = __asterius_bigint_decode(i);
          return bi > BigInt(0)
            ? __asterius_bigint_decode(bi).toString(
                Number(__asterius_bigint_decode(b))
              ).length - 1
            : -1;
        },
        integerIsPowerOf2: i =>
          Number(/^10*$/.test(__asterius_bigint_decode(i).toString(2))),
        powInteger: (i0, i1) =>
          __asterius_bigint_encode(
            __asterius_bigint_decode(i0) ** __asterius_bigint_decode(i1)
          ),
        encode: __asterius_bigint_encode,
        decode: __asterius_bigint_decode
      },
      stdio: {
        putChar: (h, c) => {
          __asterius_stdio_bufs[h] += String.fromCodePoint(c);
        },
        stdout: () => __asterius_stdio_bufs[1],
        stderr: () => __asterius_stdio_bufs[2]
      }
    };
    const importObject = Object.assign(
      req.jsffiFactory(__asterius_jsffi_instance),
      {
        Math: {
          sin: x => Math.sin(x),
          cos: x => Math.cos(x),
          tan: x => Math.tan(x),
          sinh: x => Math.sinh(x),
          cosh: x => Math.cosh(x),
          tanh: x => Math.tanh(x),
          asin: x => Math.asin(x),
          acos: x => Math.acos(x),
          atan: x => Math.atan(x),
          log: x => Math.log(x),
          exp: x => Math.exp(x),
          pow: (x, y) => Math.pow(x, y)
        },
        rts: {
          newStablePtr: __asterius_newStablePtr,
          deRefStablePtr: __asterius_deRefStablePtr,
          freeStablePtr: __asterius_freeStablePtr,
          printI64: (lo, hi) => console.log(__asterius_show_I64(lo, hi)),
          printI64_with_sym: (lo, hi) =>
            console.log("[INFO] " + __asterius_show_I64_with_sym(lo, hi)),
          print: x => console.log(x),
          emitEvent: e => console.log("[EVENT] " + req.errorMessages[e]),
          __asterius_allocGroupOnNode: (node, n) => {
            let ret_mblock = null,
              ret_block = null;
            if (__asterius_last_block + n <= 252) {
              ret_mblock = __asterius_last_mblock;
              ret_block = __asterius_last_block;
              __asterius_last_block += n;
            } else {
              const mblocks = 1 + (n <= 252 ? 0 : Math.ceil((n - 252) / 256));
              const d = mblocks << 4;
              if (__asterius_mem_cap < __asterius_mem_size + d) {
                const pd = Math.max(d, __asterius_mem_cap);
                if (
                  __asterius_wasm_instance.exports.memory.grow(pd) !==
                  __asterius_mem_cap
                )
                  throw new WebAssembly.RuntimeError("allocGroupOnNode failed");
                __asterius_mem_cap += pd;
              }
              __asterius_mem_size += d;
              ret_mblock = __asterius_last_mblock + 1;
              ret_block = 0;
              __asterius_last_mblock += mblocks;
              __asterius_last_block = Math.min(n, 252);
            }
            return Number(
              (BigInt(2097143) << BigInt(32)) |
                (BigInt(ret_mblock) << BigInt(20)) |
                (BigInt(256) + (BigInt(ret_block) << BigInt(6)))
            );
          },
          __asterius_strlen: _str => {
            const str = _str & 0xffffffff;
            const buf = new Uint8Array(
              __asterius_wasm_instance.exports.memory.buffer,
              str
            );
            return buf.indexOf(0);
          },
          __asterius_memchr: (_ptr, val, num) => {
            const ptr = _ptr & 0xffffffff;
            const buf = new Uint8Array(
              __asterius_wasm_instance.exports.memory.buffer,
              ptr,
              num
            );
            const off = buf.indexOf(val);
            return off === -1 ? 0 : _ptr + off;
          },
          __asterius_memcpy: (_dst, _src, n) => {
            const dst = _dst & 0xffffffff,
              src = _src & 0xffffffff,
              buf = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              );
            buf.copyWithin(dst, src, src + n);
          },
          __asterius_memmove: (_dst, _src, n) => {
            const dst = _dst & 0xffffffff,
              src = _src & 0xffffffff,
              buf = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              );
            buf.copyWithin(dst, src, src + n);
          },
          __asterius_memset: (_dst, c, n) => {
            const dst = _dst & 0xffffffff,
              buf = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              );
            buf.fill(c, dst, dst + n);
          },
          __asterius_memcmp: (_ptr1, _ptr2, n) => {
            const ptr1 = _ptr1 & 0xffffffff,
              ptr2 = _ptr2 & 0xffffffff,
              buf = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              );
            for (let i = 0; i < n; ++i) {
              const sgn = Math.sign(buf[ptr1 + i] - buf[ptr2 + i]);
              if (sgn) return sgn;
            }
            return 0;
          },
          __asterius_fromJSArrayBuffer: _i => {
            const buf = __asterius_jsffi_JSRefs[_i],
              cap = req.staticsSymbolMap.MainCapability;
            let p = __asterius_wasm_instance.exports.allocate(
              cap,
              Math.ceil((buf.byteLength + 31) / 8)
            );
            p = Math.ceil(p / 16) * 16;
            new Uint8Array(
              __asterius_wasm_instance.exports.memory.buffer,
              (p + 16) & 0xffffffff,
              buf.byteLength
            ).set(new Uint8Array(buf));
            const buf_header = new BigUint64Array(
              __asterius_wasm_instance.exports.memory.buffer,
              p & 0xffffffff,
              2
            );
            buf_header[0] = BigInt(req.staticsSymbolMap.stg_ARR_WORDS_info);
            buf_header[1] = BigInt(buf.byteLength);
            return p;
          },
          __asterius_toJSArrayBuffer: (_addr, len) => {
            const addr = _addr & 0xffffffff;
            return __asterius_jsffi_newJSRef(
              __asterius_wasm_instance.exports.memory.buffer.slice(
                addr,
                addr + len
              )
            );
          },
          __asterius_fromJSString: _i => {
            const s = __asterius_jsffi_JSRefs[_i];
            if (s) {
              const cap = req.staticsSymbolMap.MainCapability;
              const rp = __asterius_wasm_instance.exports.allocate(
                cap,
                s.length * 5
              );
              const buf = new BigUint64Array(
                __asterius_wasm_instance.exports.memory.buffer,
                rp & 0xffffffff,
                s.length * 5
              );
              for (let i = 0; i < s.length; ++i) {
                buf[i * 5] = BigInt(
                  req.staticsSymbolMap.ghczmprim_GHCziTypes_ZC_con_info
                );
                buf[i * 5 + 1] = BigInt(rp + i * 40 + 25);
                buf[i * 5 + 2] = BigInt(rp + (i + 1) * 40 + 2);
                buf[i * 5 + 3] = BigInt(
                  req.staticsSymbolMap.ghczmprim_GHCziTypes_Czh_con_info
                );
                buf[i * 5 + 4] = BigInt(s.codePointAt(i));
              }
              buf[(s.length - 1) * 5 + 2] = BigInt(
                req.staticsSymbolMap.ghczmprim_GHCziTypes_ZMZN_closure + 1
              );
              return rp + 2;
            } else
              return req.staticsSymbolMap.ghczmprim_GHCziTypes_ZMZN_closure + 1;
          },
          __asterius_fromJSArray: _i => {
            const arr = __asterius_jsffi_JSRefs[_i];
            if (arr.length) {
              const cap = req.staticsSymbolMap.MainCapability;
              const rp = __asterius_wasm_instance.exports.allocate(
                cap,
                arr.length * 5
              );
              const buf = new BigUint64Array(
                __asterius_wasm_instance.exports.memory.buffer,
                rp & 0xffffffff,
                arr.length * 5
              );
              for (let i = 0; i < arr.length; ++i) {
                buf[i * 5] = BigInt(
                  req.staticsSymbolMap.ghczmprim_GHCziTypes_ZC_con_info
                );
                buf[i * 5 + 1] = BigInt(rp + i * 40 + 25);
                buf[i * 5 + 2] = BigInt(rp + (i + 1) * 40 + 2);
                buf[i * 5 + 3] = BigInt(
                  req.staticsSymbolMap.ghczmprim_GHCziTypes_Izh_con_info
                );
                buf[i * 5 + 4] = BigInt(__asterius_jsffi_newJSRef(arr[i]));
              }
              buf[(arr.length - 1) * 5 + 2] = BigInt(
                req.staticsSymbolMap.ghczmprim_GHCziTypes_ZMZN_closure + 1
              );
              return rp + 2;
            } else
              return req.staticsSymbolMap.ghczmprim_GHCziTypes_ZMZN_closure + 1;
          },
          __asterius_current_memory: p => {
            __asterius_debug_log_info("Current Memory Pages: " + p);
            return p;
          },
          __asterius_debug_log_is_enabled: () => __asterius_debug_log_enabled,
          __asterius_debug_log_set_enabled: f => {
            __asterius_debug_log_enabled = Boolean(f);
          },
          __asterius_grow_memory: (p0, dp) => {
            __asterius_debug_log_info(
              "Previous Memory Pages: " + p0 + ", Allocated Memory Pages: " + dp
            );
            return p0;
          },
          __asterius_load_i64: (p_lo, p_hi, o, v_lo, v_hi) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Loading i64 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                __asterius_show_I64_with_sym(v_lo, v_hi)
            );
          },
          __asterius_store_i64: (p_lo, p_hi, o, v_lo, v_hi) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Storing i64 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                __asterius_show_I64_with_sym(v_lo, v_hi)
            );
          },
          __asterius_load_i8: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Loading i8 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_store_i8: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Storing i8 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_load_i16: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Loading i16 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_store_i16: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Storing i16 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_load_i32: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Loading i32 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_store_i32: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Storing i32 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_load_f32: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Loading f32 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_store_f32: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Storing f32 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_load_f64: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Loading f64 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_store_f64: (p_lo, p_hi, o, v) => {
            __asterius_memory_trap(p_lo, p_hi);
            __asterius_debug_log_info(
              "Storing f64 at " +
                __asterius_show_I64_with_sym(p_lo, p_hi) +
                "+" +
                o +
                ", value: " +
                v
            );
          },
          __asterius_traceCmm: f =>
            __asterius_debug_log_info(
              "Entering " +
                __asterius_show_func_sym(f) +
                ", Sp: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_Sp()
                ) +
                ", SpLim: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_SpLim()
                ) +
                ", Hp: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_Hp()
                ) +
                ", HpLim: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_HpLim()
                )
            ),
          __asterius_traceCmmBlock: (f, lbl) =>
            __asterius_debug_log_info(
              "Branching to " +
                __asterius_show_func_sym(f) +
                " basic block " +
                lbl +
                ", Sp: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_Sp()
                ) +
                ", SpLim: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_SpLim()
                ) +
                ", Hp: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_Hp()
                ) +
                ", HpLim: 0x" +
                __asterius_show_I(
                  __asterius_wasm_instance.exports.__asterius_Load_HpLim()
                )
            ),
          __asterius_traceCmmSetLocal: (f, i, lo, hi) =>
            __asterius_debug_log_info(
              "In " +
                __asterius_show_func_sym(f) +
                ", Setting local register " +
                i +
                " to " +
                __asterius_show_I64_with_sym(lo, hi)
            )
        },
        bytestring: {
          fps_reverse: (_q, _p, n) => {
            const q = _q & 0xffffffff,
              p = _p & 0xffffffff;
            const buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              subbuffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer,
                q,
                n
              );
            buffer.copyWithin(q, p, p + n);
            subbuffer.reverse();
          },
          fps_intersperse: (_q, _p, n, c) => {
            let q = _q & 0xffffffff,
              p = _p & 0xffffffff;
            const buffer = new Uint8Array(
              __asterius_wasm_instance.exports.memory.buffer
            );
            while (n > 1) {
              buffer[q++] = buffer[p++];
              buffer[q++] = c;
              --n;
            }
            if (n === 1) buffer[q] = buffer[p];
          },
          fps_maximum: (_p, len) => {
            const p = _p & 0xffffffff;
            const buffer = new Uint8Array(
              __asterius_wasm_instance.exports.memory.buffer,
              p,
              len
            );
            return buffer.reduce((x, y) => Math.max(x, y), buffer[0]);
          },
          fps_minimum: (_p, len) => {
            const p = _p & 0xffffffff;
            const buffer = new Uint8Array(
              __asterius_wasm_instance.exports.memory.buffer,
              p,
              len
            );
            return buffer.reduce((x, y) => Math.min(x, y), buffer[0]);
          },
          fps_count: (_p, len, w) => {
            const p = _p & 0xffffffff;
            const buffer = new Uint8Array(
              __asterius_wasm_instance.exports.memory.buffer,
              p,
              len
            );
            return buffer.reduce((tot, c) => (c === w ? tot + 1 : tot), 0);
          },
          fps_memcpy_offsets: (_dst, dst_off, _src, src_off, n) => {
            const dst = _dst & 0xffffffff,
              src = _src & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              );
            buffer.copyWithin(dst + dst_off, src + src_off, src + src_off + n);
            return _dst + dst_off;
          },
          _hs_bytestring_int_dec: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(10);
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
            return _buf + x_str.length;
          },
          _hs_bytestring_long_long_int_dec: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(10);
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
            return _buf + x_str.length;
          },
          _hs_bytestring_uint_dec: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(10);
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
            return _buf + x_str.length;
          },
          _hs_bytestring_long_long_uint_dec: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(10);
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
            return _buf + x_str.length;
          },
          _hs_bytestring_int_dec_padded9: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(10).padStart(9, "0");
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
          },
          _hs_bytestring_long_long_int_dec_padded18: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(10).padStart(18, "0");
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
          },
          _hs_bytestring_uint_hex: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(16);
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
            return _buf + x_str.length;
          },
          _hs_bytestring_long_long_uint_hex: (x, _buf) => {
            const buf = _buf & 0xffffffff,
              buffer = new Uint8Array(
                __asterius_wasm_instance.exports.memory.buffer
              ),
              x_str = x.toString(16);
            for (let i = 0; i < x_str.length; ++i)
              buffer[buf + i] = x_str.codePointAt(i);
            return _buf + x_str.length;
          }
        }
      }
    );
    const resultObject = await (WebAssembly.instantiateStreaming
      ? WebAssembly.instantiateStreaming(req.bufferSource, importObject)
      : WebAssembly.instantiate(req.bufferSource, importObject));
    __asterius_wasm_instance = resultObject.instance;
    __asterius_mem_cap =
      __asterius_wasm_instance.exports.memory.buffer.byteLength >> 16;
    __asterius_mem_size = __asterius_mem_cap;
    __asterius_last_mblock = (__asterius_mem_cap >> 4) - 1;
    __asterius_last_block = 252;
    return Object.assign(__asterius_jsffi_instance, {
      wasmModule: resultObject.module,
      wasmInstance: resultObject.instance,
      staticsSymbolMap: req.staticsSymbolMap,
      functionSymbolMap: req.functionSymbolMap,
      __asterius_jsffi_JSRefs: __asterius_jsffi_JSRefs,
      __asterius_SPT: __asterius_SPT
    });
  };
})();

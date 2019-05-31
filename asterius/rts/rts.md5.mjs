import { Memory } from "./rts.memory.mjs";
export class MD5 {
  // We implement the 128-bit FNV-1a hash function:
  // https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
  // This is easier, and it is unclear whether GHC
  // needs cryptographic hash functions. We assume that GHC does not ever 
  // depend on the fact that the hash function _must be MD5_.

  // used by GHC/Fingerprint.hs
  // C implementation header: libraries/base/include/md5.h
  // C implementation source: libraries/base/cbits/md5.c
  constructor(memory) {
    this.memory = memory;
    Object.seal(this);
  }


  // void MD5Init(struct MD5Context *context);
  __hsbase_MD5Init(ctxp) {
      const offset = BigInt("144066263297769815596495629667062367629");
      this.memory.i128Store(ctxp, offset);
  }

  // void MD5Update(struct MD5Context *context, byte const *buf, int len);
  __hsbase_MD5Update(ctxp, bufp, len) {
      const prime = BigInt("309485009821345068724781371");

	  let i = 0;
      let hash = this.memory.i128Load(ctxp);
	  while(i < len) {
          let c = BigInt(this.memory.i8View[Memory.unTag(bufp) + i]);
          hash = hash * prime;
          hash = hash ^ c;
          // only keep 128 bits, overflow is overflown.
          hash = hash & ((BigInt(1) << BigInt(128)) - BigInt(1));
		  i++;
	  }
      this.memory.i128Store(ctxp, hash);

  }

  
  // 16 byte = 128 bit.
  // void MD5Final(byte digest[16], struct MD5Context *context);
  __hsbase_MD5Final(digestp, ctxp) {
      this.memory.i128Store(digestp, this.memory.i128Load(ctxp));
  }
}

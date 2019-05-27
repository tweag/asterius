import { Memory } from "./rts.memory.mjs";
export class MD5 {
  // We implement the djb2 hash function (http://www.cse.yorku.ca/~oz/hash.html)
  // rather than MD5, since it is easier, and it is unclear whether GHC
  // needs cryptographic hash functions. We assume that GHC does not ever 
  // depend on the fact that the hash function _must be MD5_.

  // used by GHC/Fingerprint.hs
  // C implementation header: libraries/base/include/md5.h
  // C implementation source: libraries/base/cbits/md5.c
  constructor(memory) {
    this.memory = memory;
    Object.seal(this);
  }

  // struct MD5Context {
  //  word32 buf[4]; // 32 * 8 = 256 bits
  //  word32 bytes[2];
  //  word32 in[16];
  // };

  // void MD5Init(struct MD5Context *context);
  __hsbase_MD5Init(ctxp) {
      this.memory.i64Store(ctxp, 5381);
  }

  // void MD5Update(struct MD5Context *context, byte const *buf, int len);
  __hsbase_MD5Update(ctxp, bufp, len) {
	  let i = 0;
      let hash = this.memory.i64Load(ctxp);
	  while(i < len) {
          let c = BigInt(this.memory.i8View[Memory.unTag(bufp) + i]);
          console.error("hash: ", hash, "|c: ", c);
          hash = ((hash << BigInt(5)) + hash) + c; // hash * 33 + c
		  i++;
	  }
      this.memory.i64Store(ctxp, hash);

  }

  
  // 16 byte = 128 bit.
  // void MD5Final(byte digest[16], struct MD5Context *context);
  __hsbase_MD5Final(digestp, ctxp) {
      this.memory.i64Store(digestp, this.memory.i64Load(ctxp));
  }
}

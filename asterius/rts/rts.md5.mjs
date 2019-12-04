import { FNV } from "./rts.fnv.mjs";

export class MD5 {
  constructor(memory) {
    this.fnv = new FNV(memory);
    this.memory = memory;
    Object.freeze(this);
  }

  // void MD5Init(struct MD5Context *context);
  __hsbase_MD5Init(ctxp) {
    this.memory.i128Store(ctxp, this.fnv.init());
  }

  // void MD5Update(struct MD5Context *context, byte const *buf, int len);
  __hsbase_MD5Update(ctxp, bufp, len) {
    this.memory.i128Store(
      ctxp,
      this.fnv.update(this.memory.i128Load(ctxp), bufp, len)
    );
  }

  // 16 byte = 128 bit.
  // void MD5Final(byte digest[16], struct MD5Context *context);
  __hsbase_MD5Final(digestp, ctxp) {
    this.memory.i128Store(digestp, this.memory.i128Load(ctxp));
  }
}

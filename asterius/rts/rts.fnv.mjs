// We implement the 128-bit FNV-1a hash function:
// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
// Original work by @bollu. This module is used in two cases:
// 1. Pseudo-MD5 hashing for Fingerprints in base
// 2. FNV hashing for hashable. The 128-bits hash result is truncated to 64-bits

const fnv_offset = BigInt("144066263297769815596495629667062367629"),
  fnv_prime = BigInt("309485009821345068724781371"),
  fnv_mask = (BigInt(1) << BigInt(128)) - BigInt(1);

export class FNV {
  constructor(memory) {
    this.memory = memory;
    Object.freeze(this);
  }

  init() {
    return fnv_offset;
  }

  update(hash, bufp, len) {
    for (let i = 0; i < len; ++i) {
      hash ^= BigInt(this.memory.i8Load(bufp + i));
      hash *= fnv_prime;
      // only keep 128 bits, overflow is overflown.
      hash &= fnv_mask;
    }
    return hash;
  }
}

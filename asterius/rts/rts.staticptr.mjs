const w0_mask = (BigInt(1) << BigInt(64)) - BigInt(1);

export class StaticPtrManager {
  constructor(memory, stableptr_manager, spt_entries) {
    this.memory = memory;
    this.stablePtrManager = stableptr_manager;
    this.sptEntries = spt_entries;
    Object.freeze(this);
    for (const [, c] of this.sptEntries) {
      this.stablePtrManager.newStablePtr(c);
    }
  }

  hs_spt_lookup(w0_lo, w0_hi, w1_lo, w1_hi) {
    const r = this.sptEntries.get(
      (BigInt(w1_hi) << BigInt(96)) |
        (BigInt(w1_lo) << BigInt(64)) |
        (BigInt(w0_hi) << BigInt(32)) |
        BigInt(w0_lo)
    );
    return r ? r : 0;
  }

  hs_spt_key_count() {
    return this.sptEntries.size;
  }

  hs_spt_keys(p, n) {
    if (n !== this.hs_spt_key_count()) {
      throw new WebAssembly.RuntimeError(
        `hs_spt_keys required ${n} keys, but there are ${this.hs_spt_key_count()}`
      );
    }
    for (const [k] of this.sptEntries) {
      this.memory.i64Store(p, k & w0_mask);
      this.memory.i64Store(p + 8, k >> BigInt(64));
      p += 16;
    }
    return n;
  }
}

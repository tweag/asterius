export class Tracer {
  constructor(logger, symbol_table) {
    this.logger = logger;
    this.symbolLookupTable = {};
    for (const [k, v] of symbol_table.allEntries()) {
      this.symbolLookupTable[v] = k;
    }
    Object.freeze(this);
  }

  traceCmm(f) {
    this.logger.logInfo(["call", f, this.symbolLookupTable[f]]);
  }

  traceCmmBlock(f, lbl) {
    this.logger.logInfo(["br", f, this.symbolLookupTable[f], lbl]);
  }

  traceCmmSetLocal(f, i, v) {
    this.logger.logInfo([
      "set_local",
      f,
      this.symbolLookupTable[f],
      i,
      v,
      this.symbolLookupTable[v]
    ]);
  }
}

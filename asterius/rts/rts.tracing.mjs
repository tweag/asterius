export class Tracer {
  constructor(logger, syms) {
    this.logger = logger;
    this.symbolLookupTable = {};
    for (const[k, v] of Object.entries(syms)) this.symbolLookupTable[v] = k;
    Object.freeze(this);
  }

  traceCmm(f) { this.logger.logInfo([ "call", f, this.symbolLookupTable[f] ]); }

  traceCmmBlock(f, lbl) {
    this.logger.logInfo([ "br", f, this.symbolLookupTable[f], lbl ]);
  }

  traceCmmSetLocal(f, i, v) {
    this.logger.logInfo([
      "set_local", f, this.symbolLookupTable[f], i, v, this.symbolLookupTable[v]
    ]);
  }
}

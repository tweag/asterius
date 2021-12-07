export class SymbolTable {
  constructor(
    sym_map
  ) {
    this.symbolTable = new Map();
    for (const [k, p] of Object.entries(sym_map)) {
      this.symbolTable.set(k, p);
    }
    Object.freeze(this);
  }

  addressOf(sym) {
    if (!this.symbolTable.has(sym)) {
      throw new WebAssembly.RuntimeError(`${sym} not in symbol table`);
    }
    return this.symbolTable.get(sym);
  }

  allEntries() {
    return this.symbolTable;
  }
}

export class SymbolTable {
  constructor(
    func_symbol_table,
    statics_symbol_table
  ) {
    this.symbolTable = new Map();
    for (const [k, v] of Object.entries(func_symbol_table)) {
      this.symbolTable.set(k, v);
    }
    for (const [k, v] of Object.entries(statics_symbol_table)) {
      this.symbolTable.set(k, v);
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

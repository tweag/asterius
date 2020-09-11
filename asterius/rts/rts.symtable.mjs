export class SymbolTable {
  constructor(
    func_offset_table,
    statics_offset_table,
    table_base,
    memory_base
  ) {
    this.offsetTable = {
      ...func_offset_table,
      ...statics_offset_table,
    };
    this.symbolTable = new Map();
    for (const [k, v] of Object.entries(func_offset_table)) {
      this.symbolTable.set(k, table_base + v);
    }
    for (const [k, v] of Object.entries(statics_offset_table)) {
      this.symbolTable.set(k, memory_base + v);
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

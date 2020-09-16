export class SymbolTable {
  constructor(
    fn_offset_table,
    ss_offset_table,
    table_base,
    memory_base
  ) {
    this.symbolTable = new Map();
    for (const [k, v] of Object.entries(fn_offset_table)) {
      this.symbolTable.set(k, table_base + v); // TODO: TAGGING IS REQUIRED.
    }
    for (const [k, v] of Object.entries(ss_offset_table)) {
      this.symbolTable.set(k, memory_base + v); // TODO: TAGGING IS REQUIRED.
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

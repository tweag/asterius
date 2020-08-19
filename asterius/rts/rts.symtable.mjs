export class SymbolTable {
  constructor(func_offset_table, statics_offset_table, table_base, memory_base) {
    this.offsetTable = {
       ...func_offset_table,
       ...statics_offset_table
    };
    this.tableBase = table_base;
    this.memoryBase = memory_base;
    this.symbolTable = new Map();
    for (const [k, v] of Object.entries(func_offset_table)) {
      this.symbolTable.set(k, this.tableBase + v);
    }
    for (const [k, v] of Object.entries(statics_offset_table)) {
      this.symbolTable.set(k, this.memoryBase + v);
    }
    Object.freeze(this);
  }

  addressOf(sym) {
    return this.symbolTable.get(sym);
  }

  getTableBase() {
    return this.tableBase;
  }

  getMemoryBase() {
    return this.memoryBase;
  }

  allEntries() {
    return this.symbolTable;
  }
}

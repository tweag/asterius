export class SymbolTable {
  constructor(func_offset_table, statics_offset_table, base_address) {
    this.offsetTable = {
       ...func_offset_table,
       ...statics_offset_table
    };
    this.baseAddress = base_address;
    this.symbolTable = new Map();
    for (const [k, v] of Object.entries(this.offsetTable)) {
      this.symbolTable.set(k, v + this.baseAddress);
    }
    Object.freeze(this);
  }

  addressOf(sym) {
    return this.symbolTable.get(sym);
  }

  getBaseAddress() {
    return this.baseAddress;
  }

  // TODO: not sure we want this.
  allEntries() {
    return this.symbolTable;
  }
}

export class SymbolTable {
  constructor(offset_table, base_address) {
    this.offsetTable = offset_table;
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

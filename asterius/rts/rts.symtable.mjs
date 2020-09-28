import { Memory } from "./rts.memory.mjs";

export class SymbolTable {
  constructor(
    fn_offset_table,
    ss_offset_table,
    table_base,
    memory_base
  ) {
    this.symbolTable = new Map();
    for (const [k, off] of Object.entries(fn_offset_table)) {
      this.symbolTable.set(k, Memory.tagFunction(table_base + off));
    }
    for (const [k, off] of Object.entries(ss_offset_table)) {
      this.symbolTable.set(k, Memory.tagData(memory_base + off));
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

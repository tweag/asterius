export class Exports {
  constructor(reentrancy_guard, symbol_table, tso_manager, exports) {
    this.context = Object.freeze({
      reentrancyGuard: reentrancy_guard,
      symbolTable: symbol_table,
      tsoManager: tso_manager
    });
    Object.assign(this, exports);
  }

  async rts_eval(p) {
    this.context.reentrancyGuard.enter(0);
    const tso = this.createGenThread(p);
    this.scheduleWaitThread(tso, false);
    this.context.reentrancyGuard.exit(0);
    return this.context.tsoManager.getTSOid(tso);
  }

  async rts_evalIO(p) {
    this.context.reentrancyGuard.enter(0);
    const tso = this.createStrictIOThread(p);
    this.scheduleWaitThread(tso, false);
    this.context.reentrancyGuard.exit(0);
    return this.context.tsoManager.getTSOid(tso);
  }

  async rts_evalLazyIO(p) {
    this.context.reentrancyGuard.enter(0);
    const tso = this.createIOThread(p);
    this.scheduleWaitThread(tso, false);
    this.context.reentrancyGuard.exit(0);
    return this.context.tsoManager.getTSOid(tso);
  }

  main() {
    return this.rts_evalLazyIO(this.context.symbolTable.Main_main_closure);
  }
}

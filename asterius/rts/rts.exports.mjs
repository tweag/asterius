export class Exports {
  constructor(
    memory,
    reentrancy_guard,
    symbol_table,
    scheduler,
    exports,
    stableptr_manager
  ) {
    this.context = Object.freeze({
      memory: memory,
      reentrancyGuard: reentrancy_guard,
      symbolTable: symbol_table,
      scheduler: scheduler,
      stablePtrManager: stableptr_manager
    });
    Object.assign(this, exports);
  }

  rts_evalIO(p) {
    return this.context.scheduler.submitCmdCreateThread(
      "createStrictIOThread",
      p
    );
  }

  rts_evalLazyIO(p) {
    return this.context.scheduler.submitCmdCreateThread("createIOThread", p);
  }
}

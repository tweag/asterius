import { Channel } from "./rts.channel.mjs";
import * as rtsConstants from "./rts.constants.mjs";

async function scheduler_loop(e) {
  e.context.reentrancyGuard.enter(0);
  while (true) {
    const [f, p, resolve, reject] = await e.context.channel.take(),
      tso = e[f](p);
    e.context.memory.i64Store(
      e.context.symbolTable.__asterius_func,
      e.context.symbolTable.stg_returnToStackTop
    );
    let running = true;
    while (running) {
      e.scheduleWaitThread(tso, false);
      const ret = Number(
        e.context.memory.i64Load(
          e.context.symbolTable.MainCapability +
            rtsConstants.offset_Capability_r +
            rtsConstants.offset_StgRegTable_rRet
        )
      );
      switch (ret) {
        case 4: {
          await e.context.tsoManager.promise;
          break;
        }
        case 5: {
          resolve(e.context.tsoManager.getTSOid(tso));
          running = false;
          break;
        }
        default: {
          reject(new WebAssembly.RuntimeError(`Invalid rRet ${ret}`));
          running = false;
          break;
        }
      }
    }
  }
}

export class Exports {
  constructor(
    memory,
    reentrancy_guard,
    symbol_table,
    tso_manager,
    exports,
    stableptr_manager
  ) {
    this.context = Object.freeze({
      channel: new Channel(),
      memory: memory,
      reentrancyGuard: reentrancy_guard,
      symbolTable: symbol_table,
      tsoManager: tso_manager,
      stablePtrManager: stableptr_manager
    });
    Object.assign(this, exports);
    scheduler_loop(this);
  }

  rts_eval(p) {
    return new Promise((resolve, reject) =>
      this.context.channel.resolve(["createGenThread", p, resolve, reject])
    );
  }

  rts_evalIO(p) {
    return new Promise((resolve, reject) =>
      this.context.channel.resolve(["createStrictIOThread", p, resolve, reject])
    );
  }

  rts_evalLazyIO(p) {
    return new Promise((resolve, reject) =>
      this.context.channel.resolve(["createIOThread", p, resolve, reject])
    );
  }

  main() {
    return this.rts_evalLazyIO(
      this.rts_apply(
        this.context.symbolTable.base_AsteriusziTopHandler_runMainIO_closure,
        this.context.symbolTable.Main_main_closure
      )
    );
  }
}

import * as rtsConstants from "./rts.constants.mjs";

async function rts_eval_common(e, f, p) {
  e.context.reentrancyGuard.enter(0);
  const tso = e[f](p);
  e.context.memory.i64Store(
    e.context.symbolTable.__asterius_func,
    e.context.symbolTable.stg_returnToStackTop
  );
  while (true) {
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
        e.context.reentrancyGuard.exit(0);
        return e.context.tsoManager.getTSOid(tso);
      }
      default:
        throw new WebAssembly.RuntimeError(`Invalid rRet ${ret}`);
    }
  }
}

export class Exports {
  constructor(memory, reentrancy_guard, symbol_table, tso_manager, exports) {
    this.context = Object.freeze({
      memory: memory,
      reentrancyGuard: reentrancy_guard,
      symbolTable: symbol_table,
      tsoManager: tso_manager
    });
    Object.assign(this, exports);
  }

  rts_eval(p) {
    return rts_eval_common(this, "createGenThread", p);
  }

  rts_evalIO(p) {
    return rts_eval_common(this, "createStrictIOThread", p);
  }

  rts_evalLazyIO(p) {
    return rts_eval_common(this, "createIOThread", p);
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

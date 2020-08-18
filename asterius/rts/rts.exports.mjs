import * as rtsConstants from "./rts.constants.mjs";

function decodeTys(arr, tag) {
  const tys = [];
  while (tag) {
    const i = (tag & 0x1f) - 1;
    if (!arr[i]) {
      throw new WebAssembly.RuntimeError(`decodeTys: unsupported tag ${tag}`);
    }
    tys.push(arr[i]);
    tag >>>= 5;
  }
  return tys;
}

function decodeRtsMk(e, ty) {
  switch (ty) {
    case "JSVal": {
      return v => e.rts_mkJSVal(e.context.stablePtrManager.newJSVal(v));
    }
    default: {
      const f = `rts_mk${ty}`;
      return v => e[f](v);
    }
  }
}

function decodeRtsGet(e, ty) {
  switch (ty) {
    case "JSVal": {
      return p => e.context.stablePtrManager.getJSVal(e.rts_getJSVal(p));
    }
    default: {
      const f = `rts_get${ty}`;
      return p => e[f](p);
    }
  }
}

export class Exports {
  constructor(
    memory,
    reentrancy_guard,
    symbol_table,
    scheduler,
    stableptr_manager
  ) {
    this.context = Object.freeze({
      memory: memory,
      reentrancyGuard: reentrancy_guard,
      symbolTable: symbol_table,
      scheduler: scheduler,
      stablePtrManager: stableptr_manager,
      callbackStablePtrs: new Map(),
      rtsMkFuncs: rtsConstants.hsTyCons.map(ty => decodeRtsMk(this, ty)),
      rtsGetFuncs: rtsConstants.hsTyCons.map(ty => decodeRtsGet(this, ty))
    });
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

  newHaskellCallback(sp, arg_tag, ret_tag, io, finalizer) {
    const arg_mk_funcs = decodeTys(this.context.rtsMkFuncs, arg_tag),
      ret_get_funcs = decodeTys(this.context.rtsGetFuncs, ret_tag),
      run_func = this.context.symbolTable.addressOf(
        io
          ? "base_AsteriusziTopHandler_runIO_closure"
          : "base_AsteriusziTopHandler_runNonIO_closure"
      ),
      eval_func = ret_get_funcs.length
        ? p => this.rts_evalIO(p)
        : p => this.rts_evalLazyIO(p);
    if (ret_get_funcs.length > 1) {
      throw new WebAssembly.RuntimeError(`Multiple returns not supported`);
    }
    const cb = async (...args) => {
      try {
        if (args.length < arg_mk_funcs.length) {
          throw new WebAssembly.RuntimeError(
            `Expected ${arg_mk_funcs.length} arguments, got ${args.length}`
          );
        }
        let p = this.context.stablePtrManager.deRefStablePtr(sp);
        for (let i = 0; i < arg_mk_funcs.length; ++i) {
          p = this.rts_apply(p, arg_mk_funcs[i](args[i]));
        }
        p = this.rts_apply(run_func, p);
        const tid = await eval_func(p);
        if (ret_get_funcs.length) {
          return ret_get_funcs[0](this.context.scheduler.getTSOret(tid));
        }
      } finally {
        finalizer();
      }
    };
    this.context.callbackStablePtrs.set(cb, sp);
    return cb;
  }

  freeHaskellCallback(sn) {
    const cb = this.context.stablePtrManager.getJSVal(sn);
    this.context.stablePtrManager.freeStablePtr(
      this.context.callbackStablePtrs.get(cb)
    );
    this.context.callbackStablePtrs.delete(cb);
    this.context.stablePtrManager.freeJSVal(sn);
  }
}

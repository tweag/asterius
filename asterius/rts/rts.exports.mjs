const hsTyCons = [
  "JSVal",
  "Bool",
  null,
  null,
  "Int",
  "Int8",
  "Int16",
  "Int32",
  "Int64",
  "Word",
  "Word8",
  "Word16",
  "Word32",
  "Word64",
  "Ptr",
  "Float",
  "Double",
];

function decodeTys(tag) {
  const tys = [];
  while (tag) {
    const i = (tag & 0x1f) - 1;
    tys.push(i);
    tag >>>= 5;
  }
  return tys;
}

function decodeRtsMk(e, ty) {
  switch (ty) {
    case 0: {
      return (v) =>
        e.rts_mkJSVal(BigInt(e.context.components.jsvalManager.newJSValzh(v)));
    }
    case 2: {
      return (v) => v;
    }
    case 3: {
      return (v) => v;
    }
    default: {
      return e[`rts_mk${hsTyCons[ty]}`].bind(e);
    }
  }
}

function decodeRtsGet(e, ty) {
  switch (ty) {
    case 0: {
      return (p) =>
        e.context.components.jsvalManager.getJSValzh(Number(e.rts_getJSVal(p)));
    }
    case 2: {
      return (p) => p;
    }
    case 3: {
      return (p) => p;
    }
    default: {
      return e[`rts_get${hsTyCons[ty]}`].bind(e);
    }
  }
}

export class Exports {
  constructor(
    components,
    memory,
    reentrancy_guard,
    symbol_table,
    scheduler,
    stableptr_manager
  ) {
    this.context = Object.freeze({
      components: components,
      memory: memory,
      reentrancyGuard: reentrancy_guard,
      symbolTable: symbol_table,
      scheduler: scheduler,
      stablePtrManager: stableptr_manager,
      callbackStablePtrs: new Map(),
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
    const arg_mk_funcs = decodeTys(arg_tag).map((ty) => decodeRtsMk(this, ty)),
      ret_get_funcs = decodeTys(ret_tag).map((ty) => decodeRtsGet(this, ty)),
      run_func = this.context.symbolTable.addressOf(
        io
          ? "base_AsteriusziTopHandler_runIO_closure"
          : "base_AsteriusziTopHandler_runNonIO_closure"
      ),
      eval_func = ret_get_funcs.length
        ? (p) => this.rts_evalIO(p)
        : (p) => this.rts_evalLazyIO(p);
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
        let p = BigInt(this.context.stablePtrManager.deRefStablePtr(sp));
        for (let i = 0; i < arg_mk_funcs.length; ++i) {
          p = this.rts_apply(p, arg_mk_funcs[i](args[i]));
        }
        p = this.rts_apply(BigInt(run_func), p);
        const tid = await eval_func(Number(p));
        if (ret_get_funcs.length) {
          return ret_get_funcs[0](
            BigInt(this.context.scheduler.getTSOret(tid))
          );
        }
      } finally {
        finalizer();
      }
    };
    this.context.callbackStablePtrs.set(cb, sp);
    return cb;
  }

  freeHaskellCallback(sn) {
    const cb = this.context.components.jsvalManager.getJSValzh(sn);
    this.context.stablePtrManager.freeStablePtr(
      this.context.callbackStablePtrs.get(cb)
    );
    this.context.callbackStablePtrs.delete(cb);
    this.context.components.jsvalManager.freeJSValzh(sn);
  }
}

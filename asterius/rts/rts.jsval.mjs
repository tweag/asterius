export class JSValManager {
  constructor(components) {
    this.components = components;
    this.closure2Val = new Map();
    Object.seal(this);
  }

  newJSValzh(v) {
    const c = this.components.heapAlloc.allocate(1);
    this.components.memory.i64Store(
      c,
      this.components.symbolTable.addressOf("stg_JSVAL_info")
    );
    this.closure2Val.set(c, v);
    return c;
  }

  getJSValzh(c) {
    if (!this.closure2Val.has(c)) {
      throw new WebAssembly.RuntimeError(`Invalid JSVal# 0x${c.toString(16)}`);
    }
    return this.closure2Val.get(c);
  }

  freeJSValzh(c) {
    if (!this.closure2Val.delete(c)) {
      throw new WebAssembly.RuntimeError(`Invalid JSVal# 0x${c.toString(16)}`);
    }
  }
}

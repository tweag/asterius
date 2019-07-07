import { Memory } from "./rts.memory.mjs";
import * as rtsConstants from "./rts.constants.mjs";

export class TSOManager {
  constructor(memory, symbol_table, stablePtrManager) {
    this.memory = memory;
    this.symbolTable = symbol_table;
    this.last = 0;
    this.tsos = new Map();
    this.promise = undefined;
    this.exports = undefined;
    this.stablePtrManager = stablePtrManager;
    Object.seal(this);
  }

  newTSO() {
    const tid = ++this.last;
    this.tsos.set(
      tid,
      Object.seal({
        addr: -1,
        ret: 0,
        rstat: -1
      })
    );
    return tid;
  }

  getTSOaddr(i) {
    return this.tsos.get(i).addr;
  }

  getTSOret(i) {
    return this.tsos.get(i).ret;
  }

  getTSOrstat(i) {
    return this.tsos.get(i).rstat;
  }

  setTSOaddr(i, addr) {
    this.tsos.get(i).addr = addr;
  }

  setTSOret(i, ret) {
    this.tsos.get(i).ret = ret;
  }

  setTSOrstat(i, rstat) {
    this.tsos.get(i).rstat = rstat;
  }

  getTSOid(tso) {
    return this.memory.i32Load(tso + rtsConstants.offset_StgTSO_id);
  }

  setPromise(vt, p) {
    this.promise = p.then(
      r => {
        switch (vt) {
          case 0: {
            break;
          }
          case 1: {
            this.memory.i32Store(this.symbolTable.__asterius_ret, r);
            break;
          }
          case 2: {
            this.memory.i64Store(this.symbolTable.__asterius_ret, r);
            break;
          }
          case 3: {
            this.memory.f32Store(this.symbolTable.__asterius_ret, r);
            break;
          }
          case 4: {
            this.memory.f64Store(this.symbolTable.__asterius_ret, r);
            break;
          }
          default:
            throw new WebAssembly.RuntimeError(
              `setPromise: invalid value type ${vt}`
            );
        }
      },
      err => {
        this.memory.memset(this.symbolTable.__asterius_regs, 0, 1024);
        this.memory.memset(this.symbolTable.__asterius_ret, 0, 8);
        this.memory.i64Store(
          this.symbolTable.MainCapability +
            rtsConstants.offset_Capability_r +
            rtsConstants.offset_StgRegTable_rR1,
          this.exports.rts_apply(
            this.symbolTable.base_AsteriusziTypes_makeJSException_closure,
            this.exports.rts_mkStablePtr(this.stablePtrManager.newJSVal(err))
          )
        );
        this.memory.i64Store(
          this.symbolTable.__asterius_func,
          this.symbolTable.stg_raisezh
        );
      }
    );
  }

  resetPromise() {
    this.promise = undefined;
    this.memory.memset(this.symbolTable.__asterius_func, 0, 8);
    this.memory.memset(this.symbolTable.__asterius_regs, 0, 1024);
    this.memory.memset(this.symbolTable.__asterius_ret, 0, 8);
  }
}

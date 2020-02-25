import { Channel } from "./rts.channel.mjs";
import { Memory } from "./rts.memory.mjs";
import * as rtsConstants from "./rts.constants.mjs";

/**
 * Scheduler.
 *
 * TSO stands for Thread State Object.
 *
 * @property tsos     Contains info (tid, addr, status...) about all the TSOs.
 *
 */
export class Scheduler {
  constructor(memory, symbol_table, stablePtrManager) {
    (this.channel = new Channel()), (this.memory = memory);
    this.symbolTable = symbol_table;
    this.lastTid = 0;
    this.tsos = new Map(); // all the TSOs
    this.exports = undefined;
    this.stablePtrManager = stablePtrManager;
    this.gc = undefined;
    Object.seal(this);
  }

  setGC(gc) {
    this.gc = gc;
  }

  /**
   * Create a new TSO. Called by "createThread"
   *
   * @returns Number TSO ID.
   */
  newTSO() {
    const tid = ++this.lastTid;
    let promise_resolve, promise_reject;
    const ret_promise = new Promise((resolve, reject) => { promise_resolve = resolve; promise_reject = reject; });
    this.tsos.set(
      tid,
      Object.seal({
        addr: -1, // TSO struct address in Wasm memory
        ret: 0, // returned object address in Wasm memory
        rstat: -1, // thread status
        ffiAsyncFunc: undefined, // FFI asynchronous func
        ffiRet: undefined, // FFI returned value
        ffiRetType: 0, // FFI returned value type
        ffiRetErr: undefined, // FFI returned error
        returnPromise: ret_promise,
        promise_resolve: promise_resolve, // Settle the promise used by user
        promise_reject: promise_reject, // code to wait on this thread
        blockingPromise: undefined // Promise used to block on JS FFI code
      })
    );
    return tid;
  }

  getTSOaddr(tid) {
    return this.tsos.get(tid).addr;
  }

  getTSOret(tid) {
    return this.tsos.get(tid).ret;
  }

  getTSOrstat(tid) {
    return this.tsos.get(tid).rstat;
  }

  setTSOaddr(tid, addr) {
    this.tsos.get(tid).addr = addr;
  }

  setTSOret(tid, ret) {
    this.tsos.get(tid).ret = ret;
  }

  setTSOrstat(tid, rstat) {
    this.tsos.get(tid).rstat = rstat;
  }

  getTSOid(tso) {
    return this.memory.i32Load(tso + rtsConstants.offset_StgTSO_id);
  }

  /**
   * Called from a generated safe FFI import call.
   *
   * @param ffiPromise Promise executing the FFI import code asynchronously.
   */
  returnFFIPromise(tid, ffiPromise) {
    const tso_info = this.tsos.get(tid);
    //console.log(`Thread ${tid}: return FFI Promise`);
    tso_info.blockingPromise = ffiPromise;
  }

  /**
   * Called when a thread stops for some reason.
   */
  returnedFromTSO(tid) {
    const tso_info = this.tsos.get(tid);
    const tso = tso_info.addr;
    const reason = Number(
      this.memory.i64Load(
        this.symbolTable.MainCapability +
          rtsConstants.offset_Capability_r +
          rtsConstants.offset_StgRegTable_rRet
      )
    );

    switch (reason) {
      case 1: {
        // HeapOverflow

        this.gc.performGC();

        // put the thread back into the run-queue
        // TODO: we should put it in front if it hasn't exceeded its time splice
        this.channel.put(tid);
        break;
      }
      case 2: {
        // StackOverflow
        const prev_stack = Number(
            this.memory.i64Load(tso + rtsConstants.offset_StgTSO_stackobj)
          ),
          next_stack = this.exports.growStack(prev_stack);
        this.memory.i64Store(
          tso + rtsConstants.offset_StgTSO_stackobj,
          next_stack
        );
        this.channel.put(tid);
        break;
      }
      case 3: {
        // ThreadYielding
        // put the thread back into the run-queue
        this.channel.put(tid);
        break;
      }
      case 4: {
        // ThreadBlocked

        const why_blocked = Number(
          this.memory.i16Load(tso + rtsConstants.offset_StgTSO_why_blocked)
        );

        switch (why_blocked) {
          case Blocked.OnCCall:
          case Blocked.OnCCall_Interruptible: {
            //console.log(`Thread ${tid}: blocked on FFI`);
            // Wait for the FFI blocking promise and then requeue the TSO
            tso_info.blockingPromise.then(
              v => {
                //console.log(`Thread ${tid}: unblocked`);
                const [retTyp, retVal] = v;
                tso_info.ffiRet = retVal;
                tso_info.ffiRetType = retTyp;
                // Store return block symbol
                tso_info.ffiAsyncFunc = this.memory.i64Load(
                  tso + rtsConstants.offset_StgTSO_ffi_func
                );

                this.channel.put(tid);
              },
              e => {
                tso_info.ffiRetErr = e;
                //console.log(`Thread ${tid}: blocking FFI Promise rejected with ${e.stack}`);
                this.channel.put(tid);
              }
            );
            break;
          }

          case Blocked.OnDelay: {
            const us_delay = Number(
              this.memory.i64Load(tso + rtsConstants.offset_StgTSO_block_info)
            );
            tso_info.blockingPromise = new Promise((resolve, reject) => {
              setTimeout(() => resolve(), us_delay / 1000);
            });
            // Wait for the timer blocking promise and then requeue the TSO
            tso_info.blockingPromise.then(
              () => {
                this.channel.put(tid);
              },
              e => {
                throw new WebAssembly.RuntimeError(
                  `Scheduler: blocking TSO Promise rejected with ${e}`
                );
              }
            );
            break;
          }

          case Blocked.OnMVar:
          case Blocked.OnMVarRead: {
            //console.log(`Thread ${tid}: blocked on MVar`);
            break;
          }
          case Blocked.NotBlocked:
          case Blocked.OnBlackHole:
          case Blocked.OnRead:
          case Blocked.OnWrite:
          case Blocked.OnSTM:
          case Blocked.OnDoProc:
          case Blocked.OnMsgThrowTo:
          case Blocked.ThreadMigrating:
          default: {
            throw new WebAssembly.RuntimeError(
              `Unhandled thread blocking reason: ${why_blocked}`
            );
            break;
          }
        }

        // May execute another thread while this one is blocked
        break;
      }
      case 5: {
        // ThreadFinished
        //console.log(`Thread ${tid}: Finished`);
        const what_next = Number(
          this.memory.i16Load(tso + rtsConstants.offset_StgTSO_what_next)
        );
        switch (what_next) {
          case 1: {
            // ThreadRunGHC
            this.channel.put(tid);
            break;
          }
          case 2: {
            // ThreadInterpret
            throw new WebAssembly.RuntimeError(
              "Scheduler: unsupported ThreadInterpret"
            );
          }
          case 3: {
            // ThreadKilled
            tso_info.ret = 0;
            tso_info.rstat = 2; // Killed (SchedulerStatus)
            tso_info.promise_resolve(tid); // rts_eval* functions assume a TID is returned
            break;
          }
          case 4: {
            // ThreadComplete
            const tso_info = this.tsos.get(tid);
            const stackobj = Number(
              this.memory.i64Load(tso + rtsConstants.offset_StgTSO_stackobj)
            );
            const sp = Number(
              this.memory.i64Load(stackobj + rtsConstants.offset_StgStack_sp)
            );
            tso_info.ret = Number(this.memory.i64Load(sp + 8));
            tso_info.rstat = 1; // Success (SchedulerStatus)
            tso_info.promise_resolve(tid); // rts_eval* functions assume a TID is returned
            break;
          }
        }
        break;
      }
      default: {
        throw new WebAssembly.RuntimeError(
          `returnFFIPromise: unsupported thread stopping reason ${reason}`
        );
        break;
      }
    }
  }

  /**
   * Start the scheduler
   */
  run() {
    this.scheduler_loop().catch(
      e => {
        // signal all the TSOs that they won't complete
        for (const [tid, tso_info] of this.tsos) {
          tso_info.promise_reject(`Scheduler died with: ${e.stack}`);
        }
        throw new WebAssembly.RuntimeError(e.stack);
      }
    );
  }

  /**
   * Scheduler loop
   */
  async scheduler_loop() {
    while (true) {
      const tid = await this.channel.take();
      this.scheduler_tick(tid);
    }
  }

  scheduler_tick(tid) {
            this.exports.context.reentrancyGuard.enter(0);
            const tso_info = this.tsos.get(tid);
            const tso = tso_info.addr;

            //console.log(`Thread ${tid}: active`);

            // By default we enter the Haskell code by "returning" to the
            // closure on top of the stack.
            var entryFunc = this.symbolTable.stg_returnToStackTop;

            // Returning from blocking FFI
            if (tso_info.ffiRetErr) {
              //console.log(`Thread ${tid}: FFI error`);

              // Put an exception closure in R1 and use stg_raise# as the entry
              // function.
              this.memory.i64Store(
                this.symbolTable.MainCapability +
                  rtsConstants.offset_Capability_r +
                  rtsConstants.offset_StgRegTable_rR1,
                this.exports.rts_apply(
                  this.symbolTable.base_AsteriusziTypes_makeJSException_closure,
                  this.exports.rts_mkJSVal(
                    this.stablePtrManager.newJSVal(tso_info.ffiRetErr)
                  )
                )
              );
              entryFunc = this.symbolTable.stg_raisezh;
            } else if (tso_info.ffiAsyncFunc) {
              //console.log(`Thread ${tid}: returned from FFI ${tso_info.ffiAsyncFunc}`);

              // the entry function was saved before the safe FFI call
              entryFunc = Number(tso_info.ffiAsyncFunc);

              // Restore FFI async value
              const ffi_ret =
                tso_info.addr + rtsConstants.offset_StgTSO_ffi_return;

              switch (
                tso_info.ffiRetType // tag is encoded with `ffiValueTypesTag`
              ) {
                case 0: {
                  // no returned value
                  this.memory.i64Store(ffi_ret, 0);
                  break;
                }
                case 1: {
                  // JSVal
                  const ptr = this.stablePtrManager.newJSVal(tso_info.ffiRet);
                  //console.log(`Restore after FFI with value: ${tso_info.ffiRet} with type ${typeof tso_info.ffiRet} constructor ${tso_info.ffiRet.constructor} as ${ptr}`);
                  this.memory.i64Store(ffi_ret, ptr);
                  break;
                }
                case 2: {
                  // I32
                  this.memory.i32Store(ffi_ret, tso_info.ffiRet);
                  break;
                }
                case 3: {
                  // I64
                  this.memory.i64Store(ffi_ret, tso_info.ffiRet);
                  break;
                }
                case 4: {
                  // F32
                  this.memory.f32Store(ffi_ret, tso_info.ffiRet);
                  break;
                }
                case 5: {
                  // F64
                  this.memory.f64Store(ffi_ret, tso_info.ffiRet);
                  break;
                }
                default:
                  // FIXME: add support for multiple return values: the tag already
                  // supports it and we get a list of values in tso_info.ffiRet
                  throw new WebAssembly.RuntimeError(
                    `Unsupported FFI return value type tag ${tso_info.ffiRetType} (more than one value?): ${tso_info.ffiRet}`
                  );
                  break;
              }
            }

            tso_info.ffiAsyncFunc = undefined;
            tso_info.ffiRet = undefined;
            tso_info.ffiRetType = 0;
            tso_info.ffiRetErr = undefined;

            // execute the TSO.
            this.exports.scheduleTSO(tso, entryFunc);
            this.returnedFromTSO(tid);
            this.exports.context.reentrancyGuard.exit(0);
  }

  /**
   * Enqueue the TSO in the run-queue and wake-up the scheduler.
   */
  enqueueTSO(tso) {
    const tid = this.getTSOid(tso);

    // When the TSO has just been created, we need to store its address
    const tso_info = this.tsos.get(tid);
    if (tso_info.addr == -1) {
      tso_info.addr = Number(tso);
    }

    // Ensure that we wake up the scheduler at least once to execute this thread
    this.channel.put(tid);
  }

  /**
   * Submit a thread creation command.
   *
   * @param createThread The name of an exported function with prototype:
   *                     TSO * createThread(closure*). E.g. "createIOThread".
   * @param closure      The closure to evaluate in the thread.
   */
  submitCmdCreateThread(createThread, closure) {
    const tso = this.exports[createThread](closure), tid = this.getTSOid(tso), tso_info = this.tsos.get(tid);
    this.enqueueTSO(tso);
    return tso_info.returnPromise;
  }
}

/**
 * Blocked enum type (see rts/Constants.h)
 */
const Blocked = {
  NotBlocked: 0,
  OnMVar: 1,
  OnMVarRead: 14,
  OnBlackHole: 2,
  OnRead: 3,
  OnWrite: 4,
  OnDelay: 5,
  OnSTM: 6,
  OnDoProc: 7,
  OnCCall: 10,
  OnCCall_Interruptible: 11,
  OnMsgThrowTo: 12,
  ThreadMigrating: 13
};

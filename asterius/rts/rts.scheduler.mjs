import { Channel } from "./rts.channel.mjs";
import { Memory } from "./rts.memory.mjs";
import * as rtsConstants from "./rts.constants.mjs";

/**
 * Scheduler.
 *
 * TSO stands for Thread State Object.
 *
 * @property tsos     Contains info (tid, addr, status...) about all the TSOs.
 * @property runQueue Queue of runnable TSOs.
 *
 */
export class Scheduler {
  constructor(memory, symbol_table, stablePtrManager) {
    this.channel = new Channel(),
    this.memory = memory;
    this.symbolTable = symbol_table;
    this.lastTid = 0;
    this.tsos = new Map();         // all the TSOs
    this.runQueue = [];            // Runnable TSO IDs
    this.blockedTSOs = new Set();  // Blocked TSO IDs
    this.completeTSOs = new Set(); // Finished TSO IDs
    this.exports = undefined;
    this.stablePtrManager = stablePtrManager;
    this.gc = undefined;
    this.running = false;
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
    let promise_res = undefined;
    let promise_rej = undefined;
    let p = new Promise( (res,rej) =>
                  { promise_res = res
                  ; promise_rej = rej
                  });
    this.tsos.set(
      tid,
      Object.seal({
        addr: -1,         // TSO struct address in Wasm memory
        ret: 0,           // returned object address in Wasm memory
        rstat: -1,        // thread status
        ffiAsyncFunc: 0,  // FFI asynchronous func
        ffiRet:undefined, // FFI returned value
        ffiRetType:0,     // FFI returned value type
        ffiRetErr:undefined, // FFI returned error
        promise: p,       // Promise used by user code to wait on this thread
        promise_resolve: promise_res,
        promise_reject:  promise_rej,
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
  returnFromTSO(tid) {
    const tso_info = this.tsos.get(tid);
    const tso = tso_info.addr;
    const reason = Number(this.memory.i64Load(
                            this.symbolTable.MainCapability +
                              rtsConstants.offset_Capability_r +
                              rtsConstants.offset_StgRegTable_rRet
                          ));

    switch(reason) {
      case 1: { // HeapOverflow

        this.gc.performGC();

        // put the thread back into the run-queue
        // TODO: we should put it in front if it hasn't exceeded its time splice
        this.runQueue.push(tid);
        this.submitCmdYield();
        break;
      }
      case 2: { // StackOverflow
        throw new WebAssembly.RuntimeError("StackOverflow");
        break;
      }
      case 3: { // ThreadYielding
        // put the thread back into the run-queue
        this.runQueue.push(tid);
        this.submitCmdYield();
        break;
      }
      case 4: { // ThreadBlocked
        this.blockedTSOs.add(tid);

        const why_blocked = Number(this.memory.i16Load(tso + rtsConstants.offset_StgTSO_why_blocked));

        switch (why_blocked) {
          case Blocked.OnCCall:
          case Blocked.OnCCall_Interruptible: {
            //console.log(`Thread ${tid}: blocked on FFI`);
            // Wait for the FFI blocking promise and then requeue the TSO
            tso_info.blockingPromise.then
              (v => {
                    //console.log(`Thread ${tid}: unblocked`);
                    const [retTyp,retVal] = v;
                    tso_info.ffiRet       = retVal;
                    tso_info.ffiRetType   = retTyp;
                    // Store return block symbol
                    tso_info.ffiAsyncFunc = this.memory.i64Load(tso + rtsConstants.offset_StgTSO_ffi_func);

                    this.blockedTSOs.delete(tid);
                    this.runQueue.push(tid);
                    this.submitCmdYield();
                  }
              , e => {
                    tso_info.ffiRetErr = e;
                    //console.log(`Thread ${tid}: blocking FFI Promise rejected with ${e.stack}`);
                  }
              )
            break;
          }

          case Blocked.OnDelay: {
            const us_delay = Number(this.memory.i64Load(tso + rtsConstants.offset_StgTSO_block_info));
            tso_info.blockingPromise = new Promise((resolve,reject) => {
                setTimeout(() => resolve(), us_delay / 1000);
              });
            // Wait for the timer blocking promise and then requeue the TSO
            tso_info.blockingPromise.then
              ( () => { this.blockedTSOs.delete(tid);
                        this.runQueue.push(tid);
                        this.submitCmdYield();
                      }
              , e => { throw new WebAssembly.RuntimeError(`Scheduler: blocking TSO Promise rejected with ${e}`); }
              )
            break;
          }

          case Blocked.NotBlocked:
          case Blocked.OnMVar:
          case Blocked.OnMVarRead:
          case Blocked.OnBlackHole:
          case Blocked.OnRead:
          case Blocked.OnWrite:
          case Blocked.OnSTM:
          case Blocked.OnDoProc:
          case Blocked.OnMsgThrowTo:
          case Blocked.ThreadMigrating:
          default:
            {
            throw new WebAssembly.RuntimeError(`Unhandled thread blocking reason: ${why_blocked}`);
            break;
            }
        }

        // May execute another TSO during while this one is blocked
        this.submitCmdYield();
        break;
      }
      case 5: { // ThreadFinished
        //console.log(`Thread ${tid}: Finished`);
        const what_next = Number(this.memory.i16Load(tso + rtsConstants.offset_StgTSO_what_next));
        switch(what_next) {
          case 1: { // ThreadRunGHC
            this.runQueue.push(tid);
            this.submitCmdYield();
            break;
          }
          case 2: { // ThreadInterpret
            throw new WebAssembly.RuntimeError("Scheduler: unsupported ThreadInterpret");
            this.completeTSOs.add(tid);
            this.submitCmdYield();
            break;
          }
          case 3: { // ThreadKilled
            tso_info.ret   = 0;
            tso_info.rstat = 2; // Killed (SchedulerStatus)
            tso_info.promise_resolve(tid); // rts_eval* functions assume a TID is returned
            this.completeTSOs.add(tid);
            this.submitCmdYield();
            break;
          }
          case 4: { // ThreadComplete
            const tso_info = this.tsos.get(tid);
            const stackobj = Number(this.memory.i64Load(tso + rtsConstants.offset_StgTSO_stackobj));
            const sp       = Number(this.memory.i64Load(stackobj + rtsConstants.offset_StgStack_sp));
            tso_info.ret   = Number(this.memory.i64Load(sp + 8));
            tso_info.rstat = 1; // Success (SchedulerStatus)
            tso_info.promise_resolve(tid); // rts_eval* functions assume a TID is returned
            this.completeTSOs.add(tid);
            this.submitCmdYield();
            break;
          }
        }
        break;
      }
      default: {
        throw new WebAssembly.RuntimeError(`returnFFIPromise: unsupported thread stopping reason ${reason}`);
        break;
      }
    }
  }

  /**
   * Start the scheduler
   */
  run(exports) {
    exports.context.reentrancyGuard.enter(0);
    return this.scheduler_loop(exports).then
      ( () => { exports.context.reentrancyGuard.exit(0); }
      , e  => {
          // signal all the TSOs that they won't complete
          for (var [tid,tso_info] of this.tsos) {
            tso_info.promise_reject(`Scheduler died with: ${e.stack}`);
          }
          exports.context.reentrancyGuard.exit(0);
          throw new WebAssembly.RuntimeError(e);
      }
      );
  }

  /**
   * Scheduler loop
   */
  async scheduler_loop(e) {
    while (true) {
      // read a command from the channel
      this.running = false;
      const cmd = await this.channel.take();
      this.running = true;

      switch (cmd.type) {

        case Cmd.createThread: {
          // call any "createThread" variant. This calls newTSO to get a fresh
          // ThreadId.
          const tso = e[cmd.createThread](cmd.closure);
          const tid = this.getTSOid(tso);
          const tso_info = this.tsos.get(tid);
          //console.log(`Thread ${tid}: created`);

          // Link the Promise returned synchronously on command submission with
          // the TSO promise.
          tso_info.promise.then
            (r => cmd.resolve(r)
            ,e => cmd.reject(e)
            );

          // Add the thread into the run-queue
          this.enqueueTSO(tso);

          break;
        }

        case Cmd.yield: {
          if (this.runQueue.length > 0) {
            const tid = this.runQueue.shift();
            const tso_info = this.tsos.get(tid);
            const tso = tso_info.addr;

            //console.log(`Thread ${tid}: active`);

            // Note that __asterius_func points to the first function that will
            // be executed into scheduleTSO (it initializes the stgRun
            // trampolining loop).

            // Returning from blocking FFI
            if (tso_info.ffiRetErr) {
              //console.log(`Thread ${tid}: FFI error`);
              this.memory.i64Store(
                this.symbolTable.MainCapability +
                  rtsConstants.offset_Capability_r +
                  rtsConstants.offset_StgRegTable_rR1,
                this.exports.rts_apply(
                  this.symbolTable.base_AsteriusziTypes_makeJSException_closure,
                  this.exports.rts_mkStablePtr(this.stablePtrManager.newJSVal(tso_info.ffiRetErr))
                )
              );
              this.memory.i64Store(
                this.symbolTable.__asterius_func,
                this.symbolTable.stg_raisezh
              );
            }
            else if (tso_info.ffiAsyncFunc) {
              //console.log(`Thread ${tid}: returned from FFI ${tso_info.ffiAsyncFunc}`);
              this.memory.i64Store(
                this.symbolTable.__asterius_func,
                tso_info.ffiAsyncFunc
              );

              // Restore FFI async value
              const ffi_ret  = tso_info.addr + rtsConstants.offset_StgTSO_ffi_return;

              switch (tso_info.ffiRetType) { // tag is encoded with `ffiValueTypesTag`
                case 0: { // no returned value
                   this.memory.i64Store(ffi_ret, 0);
                   break;
                }
                case 1: { // JSVal
                   const ptr = this.stablePtrManager.newJSVal(tso_info.ffiRet);
                   //console.log(`Restore after FFI with value: ${tso_info.ffiRet} with type ${typeof tso_info.ffiRet} constructor ${tso_info.ffiRet.constructor} as ${ptr}`);
                   this.memory.i64Store(ffi_ret, ptr);
                   break;
                }
                case 2: { // I32
                   this.memory.i32Store(ffi_ret, tso_info.ffiRet);
                   break;
                }
                case 3: { // I64
                   this.memory.i64Store(ffi_ret, tso_info.ffiRet);
                   break;
                }
                case 4: { // F32
                   this.memory.f32Store(ffi_ret, tso_info.ffiRet);
                   break;
                }
                case 5: { // F64
                   this.memory.f64Store(ffi_ret, tso_info.ffiRet);
                   break;
                }
                default:
                  // FIXME: add support for multiple return values: the tag already
                  // supports it and we get a list of values in tso_info.ffiRet
                  throw  new WebAssembly.RuntimeError(`Unsupported FFI return value type tag ${tso_info.ffiRetType} (more than one value?): ${tso_info.ffiRet}`);
                  break;
              }
            }
            else {
              // We enter the Haskell code by "returning" to the closure on top
              // of the stack.
              this.memory.i64Store(
                this.symbolTable.__asterius_func,
                this.symbolTable.stg_returnToStackTop
              );
            }

            tso_info.ffiAsyncFunc = 0;
            tso_info.ffiRet       = undefined;
            tso_info.ffiRetType   = 0;
            tso_info.ffiRetErr    = undefined;


            // execute the TSO.
            e.scheduleTSO(tso);
            this.returnFromTSO(tid);
          }
          break;
        }

        default: {
          throw new WebAssembly.RuntimeError(`Unrecognized scheduler command type: ${cmd.type}`);
          break;
        }
      }
    }
  }

  /**
   * Add it into the run-queue
   */
  enqueueTSO(tso) {
    const tid = this.getTSOid(tso);
    const tso_info = this.tsos.get(tid);
    tso_info.addr = Number(tso);

    // Add the thread into the run-queue
    this.runQueue.push(tid);

    // Ensure that we yield at least once to execute this thread
    this.submitCmdYield();
  }

  /**
   * Submit a thread creation command.
   *
   * @param createThread The name of an exported function with prototype:
   *                     TSO * createThread(closure*). E.g. "createIOThread".
   * @param closure      The closure to evaluate in the thread.
   */
  submitCmdCreateThread(createThread,closure) {
    return new Promise((resolve, reject) =>
      this.channel.put(
          { type: Cmd.createThread
          , createThread : createThread
          , closure: closure
          , resolve: resolve
          , reject: reject
          })
    );
  }

  /**
   * Submit a yield command.
   *
   */
  submitCmdYield() {
    this.channel.put(
      { type: Cmd.yield
      });
  }
}

/**
 * Scheduler command types enum
 */
const Cmd =
  { createThread: 1 // create a new TSO
  , yield: 2        // switch to another TSO from the run-queue
  };

/**
 * Blocked enum type (see rts/Constants.h)
 */
const Blocked =
  { NotBlocked:            0
  , OnMVar:                1
  , OnMVarRead:            14
  , OnBlackHole:           2
  , OnRead:                3
  , OnWrite:               4
  , OnDelay:               5
  , OnSTM:                 6
  , OnDoProc:              7
  , OnCCall:               10
  , OnCCall_Interruptible: 11
  , OnMsgThrowTo:          12
  , ThreadMigrating:       13
  };

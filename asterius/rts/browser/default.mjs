/**
 * @file Implements browser-specific functionality.
 */

class Posix {
  constructor(memory, rtsConstants) {
    this.memory = memory;
    Object.seal(this);
  }
  getProgArgv(argc, argv) {
    this.memory.i64Store(argc, 0);
    this.memory.i64Store(argv, 0);
  }
  get_errno() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: get_errno");
  }
  set_errno() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: set_errno");
  }
  open() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: open");
  }
  close() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: close");
  }
  stat() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: stat");
  }
  fstat() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: fstat");
  }
  opendir() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: opendir");
  }
  readdir() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: readdir");
  }
  closedir() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: closedir");
  }
  getenv() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: getenv");
  }
  access() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: access");
  }
  getcwd() {
    throw WebAssembly.RuntimeError("Unsupported rts interface: getcwd");
  }
}

export default {
  /**
   * A custom Time interface, used in {@link TimeCBits}.
   */
  Time: {
    /**
     * Returns the current timestamp, where 0 represents
     * the time origin of the document.
     * @returns A [seconds, nanoseconds] Array.
     */
    getCPUTime: () => {
      const ms = performance.now(),
            s = Math.floor(ms / 1000.0),
            ns = Math.floor(ms - s * 1000) * 1000000;
      return [s, ns];
    },
    /**
     * Returns the current timestamp, where 0 represents UNIX Epoch.
     * @returns A [seconds, nanoseconds] Array.
     */
    getUnixEpochTime: () => {
      const ms = Date.now(),
            s = Math.floor(ms / 1000.0),
            ns = Math.floor(ms - s * 1000) * 1000000;
      return [s, ns];
    },
    /**
     * The resolution of the timestamps in nanoseconds.
     * Note! Due to the Spectre attack, browsers do not
     * provide high-resolution timestamps anymore.
     * See https://developer.mozilla.org/en-US/docs/Web/API/Performance/now
     * and https://spectreattack.com.
     * We fallback to a resolution of 1ms.
     */
    resolution: 1000000
  },
  posix: Posix
};

// The content below is MIT licensed, adapted from
// https://github.com/YuzuJS/setImmediate

// Copyright (c) 2012 Barnesandnoble.com, llc, Donavon West, and Domenic
// Denicola

(function (global, undefined) {
  "use strict";

  if (global.setImmediate) {
      return;
  }

  var nextHandle = 1; // Spec says greater than zero
  var tasksByHandle = {};
  var currentlyRunningATask = false;
  var doc = global.document;
  var registerImmediate;

  function setImmediate(callback) {
    // Callback can either be a function or a string
    if (typeof callback !== "function") {
      callback = new Function("" + callback);
    }
    // Copy function arguments
    var args = new Array(arguments.length - 1);
    for (var i = 0; i < args.length; i++) {
        args[i] = arguments[i + 1];
    }
    // Store and register the task
    var task = { callback: callback, args: args };
    tasksByHandle[nextHandle] = task;
    registerImmediate(nextHandle);
    return nextHandle++;
  }

  function clearImmediate(handle) {
      delete tasksByHandle[handle];
  }

  function run(task) {
      var callback = task.callback;
      var args = task.args;
      switch (args.length) {
      case 0:
          callback();
          break;
      case 1:
          callback(args[0]);
          break;
      case 2:
          callback(args[0], args[1]);
          break;
      case 3:
          callback(args[0], args[1], args[2]);
          break;
      default:
          callback.apply(undefined, args);
          break;
      }
  }

  function runIfPresent(handle) {
      // From the spec: "Wait until any invocations of this algorithm started before this one have completed."
      // So if we're currently running a task, we'll need to delay this invocation.
      if (currentlyRunningATask) {
          // Delay by doing a setTimeout. setImmediate was tried instead, but in Firefox 7 it generated a
          // "too much recursion" error.
          setTimeout(runIfPresent, 0, handle);
      } else {
          var task = tasksByHandle[handle];
          if (task) {
              currentlyRunningATask = true;
              try {
                  run(task);
              } finally {
                  clearImmediate(handle);
                  currentlyRunningATask = false;
              }
          }
      }
  }

  function installNextTickImplementation() {
      registerImmediate = function(handle) {
          process.nextTick(function () { runIfPresent(handle); });
      };
  }

  function canUsePostMessage() {
      // The test against `importScripts` prevents this implementation from being installed inside a web worker,
      // where `global.postMessage` means something completely different and can't be used for this purpose.
      if (global.postMessage && !global.importScripts) {
          var postMessageIsAsynchronous = true;
          var oldOnMessage = global.onmessage;
          global.onmessage = function() {
              postMessageIsAsynchronous = false;
          };
          global.postMessage("", "*");
          global.onmessage = oldOnMessage;
          return postMessageIsAsynchronous;
      }
  }

  function installPostMessageImplementation() {
      // Installs an event handler on `global` for the `message` event: see
      // * https://developer.mozilla.org/en/DOM/window.postMessage
      // * http://www.whatwg.org/specs/web-apps/current-work/multipage/comms.html#crossDocumentMessages

      var messagePrefix = "setImmediate$" + Math.random() + "$";
      var onGlobalMessage = function(event) {
          if (event.source === global &&
              typeof event.data === "string" &&
              event.data.indexOf(messagePrefix) === 0) {
              runIfPresent(+event.data.slice(messagePrefix.length));
          }
      };

      if (global.addEventListener) {
          global.addEventListener("message", onGlobalMessage, false);
      } else {
          global.attachEvent("onmessage", onGlobalMessage);
      }

      registerImmediate = function(handle) {
          global.postMessage(messagePrefix + handle, "*");
      };
  }

  function installMessageChannelImplementation() {
      var channel = new MessageChannel();
      channel.port1.onmessage = function(event) {
          var handle = event.data;
          runIfPresent(handle);
      };

      registerImmediate = function(handle) {
          channel.port2.postMessage(handle);
      };
  }

  function installReadyStateChangeImplementation() {
      var html = doc.documentElement;
      registerImmediate = function(handle) {
          // Create a <script> element; its readystatechange event will be fired asynchronously once it is inserted
          // into the document. Do so, thus queuing up the task. Remember to clean up once it's been called.
          var script = doc.createElement("script");
          script.onreadystatechange = function () {
              runIfPresent(handle);
              script.onreadystatechange = null;
              html.removeChild(script);
              script = null;
          };
          html.appendChild(script);
      };
  }

  function installSetTimeoutImplementation() {
      registerImmediate = function(handle) {
          setTimeout(runIfPresent, 0, handle);
      };
  }

  // If supported, we should attach to the prototype of global, since that is where setTimeout et al. live.
  var attachTo = Object.getPrototypeOf && Object.getPrototypeOf(global);
  attachTo = attachTo && attachTo.setTimeout ? attachTo : global;

  // Don't get fooled by e.g. browserify environments.
  if ({}.toString.call(global.process) === "[object process]") {
      // For Node.js before 0.9
      installNextTickImplementation();

  } else if (canUsePostMessage()) {
      // For non-IE10 modern browsers
      installPostMessageImplementation();

  } else if (global.MessageChannel) {
      // For web workers, where supported
      installMessageChannelImplementation();

  } else if (doc && "onreadystatechange" in doc.createElement("script")) {
      // For IE 6–8
      installReadyStateChangeImplementation();

  } else {
      // For older browsers
      installSetTimeoutImplementation();
  }

  attachTo.setImmediate = setImmediate;
  attachTo.clearImmediate = clearImmediate;
}(typeof self === "undefined" ? typeof global === "undefined" ? this : global : self));

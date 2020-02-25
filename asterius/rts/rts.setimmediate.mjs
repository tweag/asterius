export function setImmediate(f, ...args) {
  return globalThis.setImmediate
    ? globalThis.setImmediate(f, ...args)
    : globalThis.setTimeout(f, 0, ...args);
}

export function clearImmediate(i) {
  (globalThis.clearImmediate
    ? globalThis.clearImmediate
    : globalThis.clearTimeout)(i);
}

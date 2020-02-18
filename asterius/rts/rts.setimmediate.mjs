export function setImmediate(f, ...args) {
  return globalThis.setImmediate
    ? globalThis.setImmediate(f, ...args)
    : setTimeout(f, 0, ...args);
}

export function clearImmediate(i) {
  globalThis.clearImmediate ? globalThis.clearImmediate(i) : clearTimeout(i);
}

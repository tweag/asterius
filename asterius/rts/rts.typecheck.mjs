export function isI32(v) {
  if (!Number.isSafeInteger(v)) {
    throw WebAssembly.RuntimeError(`${v} not a safe integer`);
  }
  if (v < 0 || v > 0xffffffff) {
    throw WebAssembly.RuntimeError(`${v} range error`);
  }
  return v;
}

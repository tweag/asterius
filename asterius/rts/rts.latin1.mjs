import * as settings from "./rts.settings.mjs";

function safeCodePointAt(s, i) {
  const c = s.codePointAt(i);
  if (c >= 0 && c < 256)
    return c;
  else
    throw new WebAssembly.RuntimeError(
        "encodeLatin1: code point out of range, original string: " + s +
        ", index: " + i);
}

export function encodeLatin1(s) {
  const buf = new Uint8Array(s.length);
  if (settings.debug)
    for (let i = 0; i < s.length; ++i) buf[i] = safeCodePointAt(s, i);
  else
    for (let i = 0; i < s.length; ++i) buf[i] = s.codePointAt(i);
  return buf.buffer;
};

export function decodeLatin1(buf) {
  return new Uint8Array(buf).reduce(
      (tot, byte) => tot + String.fromCodePoint(byte), "");
};

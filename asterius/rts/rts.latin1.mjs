export function encodeLatin1(s) {
  const buf = new Uint8Array(s.length);
  for (let i = 0; i < s.length; ++i) buf[i] = s.codePointAt(i);
  return buf.buffer;
}

export function decodeLatin1(buf) {
  return new Uint8Array(buf).reduce(
    (tot, byte) => tot + String.fromCodePoint(byte),
    ""
  );
}

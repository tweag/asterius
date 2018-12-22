export function encodeUTF32(s) {
  const buf = new Uint32Array(s.length);
  let i = 0, j = 0;
  for (; i < s.length;) {
    const char_code = s.charCodeAt(i), code_point = s.codePointAt(i);
    buf[j++] = code_point;
    i += char_code === code_point ? 1 : 2;
  }
  return buf.slice(0, j).buffer;
};

export function decodeUTF32(buf) {
  return new Uint32Array(buf).reduce(
      (tot, byte) => tot + String.fromCodePoint(byte), "");
};

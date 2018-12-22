export function encodeUTF16(s) {
  const buf = new Uint16Array(s.length);
  for (let i = 0; i < s.length; ++i) buf[i] = s.charCodeAt(i);
  return buf.buffer;
};

export function decodeUTF16(buf) {
  return new Uint16Array(buf).reduce(
      (tot, byte) => tot + String.fromCharCode(byte), "");
};

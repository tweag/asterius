const encoder = new TextEncoder(),
  decoder = new TextDecoder("utf-8", { fatal: true });

export function encodeUTF8(s) {
  return encoder.encode(s).buffer;
}

export function decodeUTF8(buf) {
  return decoder.decode(buf);
}

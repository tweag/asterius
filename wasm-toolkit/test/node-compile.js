"use strict";

const fs = require("fs").promises;

async function compileWasmFromFilePath(p) {
  const buf = await fs.readFile(p);
  return WebAssembly.compile(buf);
}

process.on("unhandledRejection", err => {
  throw err;
});

compileWasmFromFilePath(process.argv[2]);

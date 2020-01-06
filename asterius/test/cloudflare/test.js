"use strict";

const fs = require("fs").promises;
const path = require("path");
const Cloudworker = require("@dollarshaveclub/cloudworker");
const wasm = require("@dollarshaveclub/cloudworker/lib/wasm");
const test = require("ava");

const scriptPath = path.join(__dirname, "cloudflare.js");
const wasmPath = path.join(__dirname, "cloudflare.wasm");

test.beforeEach(t => {
  return Promise
    .all([fs.readFile(scriptPath, "utf8"), wasm.loadPath(wasmPath)])
    .then(values => {
      const [script, wasm] = values;
      const bindings = {wasm: wasm};
      t.context.cw = new Cloudworker(script, {bindings});
    })
});

test('get: successful response', async t => {
  const req = new Cloudworker.Request('https://example.com/');
  const res = await t.context.cw.dispatch(req);
  t.is(res.status, 200);
  t.is(await res.text(), 'Hello from Haskell');
});

test("post: missing name", async t => {
  const req = new Cloudworker.Request(
    'https://example.com/',
    {method: "POST", body: '{"hello": "cloudflare"}'})
  const res = await t.context.cw.dispatch(req);
  t.is(res.status, 400);
});

test('post: successful response', async t => {
  const req = new Cloudworker.Request(
    'https://example.com/',
    {method: "POST", body: '{"name": "cloudflare"}'})
  const res = await t.context.cw.dispatch(req);
  t.is(res.status, 200);
  t.is(await res.text(), 'Hello cloudflare');
});

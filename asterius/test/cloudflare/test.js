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

test('get: successful response', t => {
  const req = new Cloudworker.Request(
    'https://example.com/')
  return t.context.cw.dispatch(req).then((res) => {
    t.is(res.status, 200);
    res.text().then((body) =>{
      t.is(body, 'Hello from Haskell');
    })
  })
});

test("post: missing name", t => {
  const req = new Cloudworker.Request(
    'https://example.com/',
    {method: "POST", body: '{"hello": "cloudflare"}'})
  return t.context.cw.dispatch(req).then((res) => {
    t.is(res.status, 400)
  })
});

test('post: successful response', t => {
  const req = new Cloudworker.Request(
    'https://example.com/',
    {method: "POST", body: '{"name": "cloudflare"}'})
  return t.context.cw.dispatch(req).then((res) => {
    t.is(res.status, 200);
    res.text().then((body) =>{
      t.is(body, "Hello cloudflare");
    })
  })
});

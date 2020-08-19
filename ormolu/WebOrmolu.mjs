import * as rts from "./rts.mjs";
import module from "./WebOrmolu.wasm.mjs";
import req from "./WebOrmolu.req.mjs";

module
  .then(m => rts.newAsteriusInstance(Object.assign(req, { module: m })))
  .then(i => {
    const txt = document.getElementById("textarea");
    const btn = document.getElementById("button");
    btn.addEventListener("click", () => {
      i.exports.webOrmolu(txt.value).then(
        r => {
          txt.value = r;
        },
        err => {
          txt.value = err;
        }
      );
    });
    btn.disabled = false;
    btn.textContent = "Ormolize";
  });

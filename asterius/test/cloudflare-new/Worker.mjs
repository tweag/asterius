import * as rts from "./rts.mjs";
import req from "./Worker.req.mjs";

addEventListener("fetch", (event) => {
  event.respondWith(
    rts
      .newAsteriusInstance(Object.assign(req, { module: wasm }))
      .then((i) => i.exports.handleFetch(event))
      .catch(
        (err) =>
          new Response(`${err}`, {
            status: 500,
            statusText: "Internal Server Error",
          })
      )
  );
});

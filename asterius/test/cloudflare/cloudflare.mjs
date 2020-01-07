import * as rts from "./rts.mjs";
import cloudflare from "./cloudflare.req.mjs";

const asteriusInstance = new Promise((resolve, reject) => {
  // Cloudflare Workers makes Wasm bindings available as global variables.
  // The binding name is configurable by the user when using the API.
  // It gets hardcoded to 'wasm' when using the Wrangler CLI (as of v1.6.0).
  rts.newAsteriusInstance(Object.assign(cloudflare, { module: wasm }))
    .then(i => {
      i.exports.hs_init();
      resolve(i);
    })
    .catch(e => reject(e));
});

addEventListener("fetch", event => {
  event.respondWith(asteriusInstance
    .then(i => i.exports.handleFetch(event))
    .catch(e => new Response(e)))
})

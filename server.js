import { Application, Router } from "https://deno.land/x/oak/mod.ts";

import mockData from "./mock-data.json" assert { type: "json" }

const router = new Router();

const sleep = (ms) => async (_ctx, next) => {
  await (new Promise((resolve, _reject) => {
    setTimeout(() => {
      resolve();
    }, ms);
  })).then(next);
};

router
  .get("/api/v1/fs", sleep(3000), (ctx) => {
    ctx.response.body = mockData;
  });

const app = new Application();

app.use(async (ctx, next) => {
  try {
    await ctx.send({
      root: `${Deno.cwd()}/static`,
      index: "index.html",
    });
  } catch {
    await next();
  }
});

app.use(router.routes());

console.log(`Server listening on port 8080...`);

await app.listen({
  port: 8080,
});

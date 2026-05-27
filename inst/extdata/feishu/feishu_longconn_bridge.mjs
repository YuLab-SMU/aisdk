#!/usr/bin/env node

/**
 * Feishu long-connection bridge for local aisdk usage.
 *
 * This script uses Feishu's official Node SDK long connection mode and forwards
 * plaintext event payloads to a local aisdk Feishu loopback handler.
 *
 * Required env:
 *   FEISHU_APP_ID
 *   FEISHU_APP_SECRET
 *
 * Optional env:
 *   FEISHU_LOOPBACK_URL=http://127.0.0.1:8788/feishu/webhook
 */

const appId = process.env.FEISHU_APP_ID || "";
const appSecret = process.env.FEISHU_APP_SECRET || "";
const loopbackUrl =
  process.env.FEISHU_LOOPBACK_URL || "http://127.0.0.1:8788/feishu/webhook";

if (!appId || !appSecret) {
  console.error("FEISHU_APP_ID and FEISHU_APP_SECRET must be set.");
  process.exit(1);
}

async function loadLarkSdk() {
  try {
    return await import("@larksuiteoapi/node-sdk");
  } catch (error) {
    if (error?.code === "ERR_MODULE_NOT_FOUND") {
      console.error("");
      console.error("[bridge] Missing Node dependency: @larksuiteoapi/node-sdk");
      console.error("[bridge] Install it in the bridge directory with:");
      console.error("");
      console.error("  npm install");
      console.error("");
      console.error("[bridge] Then start the bridge with:");
      console.error("");
      console.error("  node feishu_longconn_bridge.mjs");
      console.error("");
      process.exit(1);
    }
    throw error;
  }
}

async function forwardEvent(eventType, data) {
  const body = {
    schema: "2.0",
    header: {
      event_type: eventType,
      event_id:
        data?.header?.event_id ||
        data?.message?.message_id ||
        `${eventType}-${Date.now()}`,
    },
    event: data,
  };

  const response = await fetch(loopbackUrl, {
    method: "POST",
    headers: {
      "content-type": "application/json",
    },
    body: JSON.stringify(body),
  });

  const text = await response.text();
  console.log(
    `[bridge] forwarded ${eventType} -> ${response.status} ${response.statusText} ${text}`,
  );
}

async function main() {
  const lark = await loadLarkSdk();

  const eventDispatcher = new lark.EventDispatcher({}).register({
    "im.message.receive_v1": async (data) => {
      await forwardEvent("im.message.receive_v1", data);
    },
  });

  const client = new lark.WSClient({
    appId,
    appSecret,
    logLevel: lark.LoggerLevel.info,
  });

  console.log(`[bridge] connecting via long connection, loopback=${loopbackUrl}`);
  await client.start({
    eventDispatcher,
  });
}

await main();

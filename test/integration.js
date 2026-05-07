#!/usr/bin/env node
// Integration test for Alloy protocol.
//
// Usage:
//   # Run all tests (starts its own daemon on port 17120):
//   node test/integration.js
//
//   # Test only bridge or TCP mode:
//   node test/integration.js bridge
//   node test/integration.js tcp
//
// Starts a temporary alloyd with a test config, then runs tests against it.

const { spawn, execSync } = require("child_process");
const net = require("net");
const path = require("path");
const fs = require("fs");
const os = require("os");

const BUILD_DIR = path.join(__dirname, "../_build/default/bin");
const ALLOYD_BIN = path.join(BUILD_DIR, "server/main.exe");
const ALLOY_BIN = path.join(BUILD_DIR, "client/main.exe");
const TEST_PORT = 17120;
const TEST_ADDR = `127.0.0.1:${TEST_PORT}`;

let passed = 0;
let failed = 0;

function assert(condition, msg) {
  if (condition) {
    passed++;
    console.log(`  ✓ ${msg}`);
  } else {
    failed++;
    console.log(`  ✗ ${msg}`);
  }
}

function assertEqual(actual, expected, msg) {
  if (JSON.stringify(actual) === JSON.stringify(expected)) {
    passed++;
    console.log(`  ✓ ${msg}`);
  } else {
    failed++;
    console.log(`  ✗ ${msg}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

// ─── Test daemon management ─────────────────────────────────────────────────

function createTestConfig() {
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "alloy-test-"));
  const configPath = path.join(tmpDir, "config.json");
  const config = {
    listen: [TEST_ADDR],
    allowed_networks: ["127.0.0.0/8", "::1/128"],
    tenants: {
      "test-tenant": {
        label: "Test",
        color: "#00ff00",
      },
    },
    rules: [
      {
        pattern: "https://routed[.]example[.]com/.*",
        target: "test-tenant",
        enabled: true,
      },
    ],
    defaults: {
      unmatched: "local",
      cooldown_seconds: 1,
      browser_launch_timeout: 5,
    },
  };
  fs.writeFileSync(configPath, JSON.stringify(config, null, 2));
  return { tmpDir, configPath };
}

function startDaemon(configPath) {
  return new Promise((resolve, reject) => {
    const proc = spawn(ALLOYD_BIN, ["--config", configPath], {
      stdio: ["ignore", "pipe", "pipe"],
    });
    let started = false;

    proc.stderr.on("data", (d) => {
      const line = d.toString();
      // Uncomment for debugging:
      // process.stderr.write(`[alloyd] ${line}`);
      if (!started && line.includes("listening")) {
        started = true;
        resolve(proc);
      }
    });

    proc.on("error", reject);
    proc.on("exit", (code) => {
      if (!started) reject(new Error(`daemon exited with code ${code}`));
    });

    // Fallback: assume started after a short delay
    setTimeout(() => {
      if (!started) {
        started = true;
        resolve(proc);
      }
    }, 1000);
  });
}

// ─── Native Messaging framing ───────────────────────────────────────────────

function writeNativeMessage(stream, obj) {
  const json = JSON.stringify(obj);
  const buf = Buffer.alloc(4 + json.length);
  buf.writeUInt32LE(json.length, 0);
  buf.write(json, 4);
  stream.write(buf);
}

function createNativeMessageReader(stream) {
  let buffer = Buffer.alloc(0);
  const pending = [];
  let waiting = null;

  stream.on("data", (chunk) => {
    buffer = Buffer.concat([buffer, chunk]);
    drain();
  });

  function drain() {
    while (buffer.length >= 4) {
      const len = buffer.readUInt32LE(0);
      if (buffer.length < 4 + len) break;
      const json = buffer.subarray(4, 4 + len).toString("utf8");
      buffer = buffer.subarray(4 + len);
      try {
        const msg = JSON.parse(json);
        if (waiting) {
          const resolve = waiting;
          waiting = null;
          resolve(msg);
        } else {
          pending.push(msg);
        }
      } catch (e) {
        console.error("Bad JSON from bridge:", json);
      }
    }
  }

  return {
    read(timeoutMs = 5000) {
      if (pending.length > 0) {
        return Promise.resolve(pending.shift());
      }
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          waiting = null;
          reject(new Error("read timeout"));
        }, timeoutMs);
        waiting = (msg) => {
          clearTimeout(timer);
          resolve(msg);
        };
      });
    },
  };
}

// ─── TCP framing (newline-delimited JSON) ───────────────────────────────────

function createTcpClient(host, port) {
  return new Promise((resolve, reject) => {
    const socket = net.createConnection({ host, port }, () => {
      let buffer = "";
      const pending = [];
      let waiting = null;

      socket.on("data", (chunk) => {
        buffer += chunk.toString();
        let idx;
        while ((idx = buffer.indexOf("\n")) !== -1) {
          const line = buffer.slice(0, idx);
          buffer = buffer.slice(idx + 1);
          try {
            const msg = JSON.parse(line);
            if (waiting) {
              const w = waiting;
              waiting = null;
              w(msg);
            } else {
              pending.push(msg);
            }
          } catch (e) {
            console.error("Bad JSON from daemon:", line);
          }
        }
      });

      resolve({
        send(obj) {
          socket.write(JSON.stringify(obj) + "\n");
        },
        read(timeoutMs = 5000) {
          if (pending.length > 0) return Promise.resolve(pending.shift());
          return new Promise((resolve, reject) => {
            const timer = setTimeout(() => {
              waiting = null;
              reject(new Error("tcp read timeout"));
            }, timeoutMs);
            waiting = (msg) => {
              clearTimeout(timer);
              resolve(msg);
            };
          });
        },
        close() {
          socket.end();
        },
      });
    });
    socket.on("error", reject);
  });
}

// ─── Bridge tests ───────────────────────────────────────────────────────────

async function testBridge() {
  console.log("\n=== Bridge integration tests ===\n");

  const proc = spawn(ALLOY_BIN, ["bridge"], {
    stdio: ["pipe", "pipe", "pipe"],
  });
  proc.stderr.on("data", (d) => {
    // Uncomment for debugging:
    // process.stderr.write(`[bridge stderr] ${d}`);
  });

  const reader = createNativeMessageReader(proc.stdout);

  // 1. Register (id=0 per protocol convention)
  console.log("Register:");
  writeNativeMessage(proc.stdin, {
    id: 0,
    command: ["Register", { brand: "Integration Test", address: TEST_ADDR, name: "test-node" }],
  });

  const regResp = await reader.read();
  // Should be ["Response", {id: 0, response: ["Ok_registered", {tenant_id: "test-node"}]}]
  assertEqual(regResp[0], "Response", "register response is Response");
  assertEqual(regResp[1].id, 0, "register response id=0");
  assertEqual(regResp[1].response[0], "Ok_registered", "register response is Ok_registered");
  assertEqual(regResp[1].response[1].tenant_id, "test-node", "tenant_id=test-node");

  // 2. Expect Config_updated push (id=0)
  console.log("\nConfig push:");
  const pushMsg = await reader.read();
  assertEqual(pushMsg[0], "Push", "config push is Push");
  assertEqual(pushMsg[1].id, 0, "push id=0");
  assertEqual(pushMsg[1].push[0], "Config_updated", "push is Config_updated");
  assert(Array.isArray(pushMsg[1].push[1].registered_tenants), "has registered_tenants list");
  assert(pushMsg[1].push[1].registered_tenants.includes("test-node"), "test-node is registered");

  // 3. Status (extension sends Wire.request with its own ID)
  console.log("\nStatus:");
  writeNativeMessage(proc.stdin, { id: 1, command: ["Status"] });
  const statusResp = await reader.read();
  assertEqual(statusResp[0], "Response", "status is Response");
  assertEqual(statusResp[1].id, 1, "status id=1");
  assertEqual(statusResp[1].response[0], "Ok_status", "status response is Ok_status");
  const statusInfo = statusResp[1].response[1];
  assert(Array.isArray(statusInfo.registered_tenants), "status has registered_tenants");
  assert(typeof statusInfo.uptime_seconds === "number", "status has uptime_seconds");

  // 4. Get_config
  console.log("\nGet_config:");
  writeNativeMessage(proc.stdin, { id: 2, command: ["Get_config"] });
  const cfgResp = await reader.read();
  assertEqual(cfgResp[0], "Response", "get_config is Response");
  assertEqual(cfgResp[1].id, 2, "get_config id=2");
  assertEqual(cfgResp[1].response[0], "Ok_config", "response is Ok_config");
  const config = cfgResp[1].response[1];
  assert(Array.isArray(config.listen), "config has listen");
  assert(Array.isArray(config.rules), "config has rules");
  assert(typeof config.defaults === "object", "config has defaults");

  // 5. Test URL routing
  console.log("\nTest URL:");
  writeNativeMessage(proc.stdin, { id: 3, command: ["Test", { url: "https://example.com" }] });
  const testResp = await reader.read();
  assertEqual(testResp[0], "Response", "test is Response");
  assertEqual(testResp[1].id, 3, "test id=3");
  const testResult = testResp[1].response;
  assert(testResult[0] === "Ok_test", `test response is Ok_test (got ${testResult[0]})`);

  // 6. Add rule (may trigger a config push before or after the response)
  console.log("\nAdd rule:");
  writeNativeMessage(proc.stdin, {
    id: 4,
    command: ["Add_rule", { rule: { pattern: "https://test-integration[.]example[.]com/.*", target: "test-node", enabled: true } }],
  });

  // Read two messages: one Response + one Push (order not guaranteed)
  const addMsg1 = await reader.read();
  const addMsg2 = await reader.read();
  const addResp = [addMsg1, addMsg2].find((m) => m[0] === "Response");
  const addPush = [addMsg1, addMsg2].find((m) => m[0] === "Push");
  assert(addResp !== undefined, "add_rule got Response");
  assert(addPush !== undefined, "add_rule triggered config Push");
  assertEqual(addResp[1].id, 4, "add_rule response id=4");
  assertEqual(addResp[1].response[0], "Ok_unit", "add_rule result is Ok_unit");

  // 7. Test the new rule
  console.log("\nTest new rule:");
  writeNativeMessage(proc.stdin, {
    id: 5,
    command: ["Test", { url: "https://test-integration.example.com/page" }],
  });
  const testResp2 = await reader.read();
  assertEqual(testResp2[1].id, 5, "test2 id=5");
  assertEqual(testResp2[1].response[0], "Ok_test", "test is Ok_test");
  assertEqual(testResp2[1].response[1][0], "Match", "test result is Match");
  assertEqual(testResp2[1].response[1][1].tenant, "test-node", "matched tenant=test-node");

  // 8. Delete the rule we added (find its index)
  console.log("\nDelete rule:");
  writeNativeMessage(proc.stdin, { id: 6, command: ["Get_config"] });
  const cfg2Resp = await reader.read();
  assertEqual(cfg2Resp[1].id, 6, "get_config2 id=6");
  const rules = cfg2Resp[1].response[1].rules;
  const ruleIdx = rules.findIndex(
    (r) => r.pattern === "https://test-integration[.]example[.]com/.*"
  );
  assert(ruleIdx >= 0, `found test rule at index ${ruleIdx}`);

  writeNativeMessage(proc.stdin, { id: 7, command: ["Delete_rule", { index: ruleIdx }] });
  // Response + config push (order not guaranteed)
  const delMsg1 = await reader.read();
  const delMsg2 = await reader.read();
  const delResp = [delMsg1, delMsg2].find((m) => m[0] === "Response");
  assert(delResp !== undefined, "delete_rule got Response");
  assertEqual(delResp[1].id, 7, "delete_rule id=7");
  assertEqual(delResp[1].response[0], "Ok_unit", "delete_rule result is Ok_unit");

  // 9. Correlation ID ordering — fire two commands quickly
  console.log("\nCorrelation ID ordering:");
  writeNativeMessage(proc.stdin, { id: 8, command: ["Status"] });
  writeNativeMessage(proc.stdin, { id: 9, command: ["Get_config"] });
  const r1 = await reader.read();
  const r2 = await reader.read();
  // Both should be Responses with the IDs we assigned
  assertEqual(r1[0], "Response", "rapid-fire r1 is Response");
  assertEqual(r2[0], "Response", "rapid-fire r2 is Response");
  assertEqual(r1[1].id, 8, "r1 id=8");
  assertEqual(r2[1].id, 9, "r2 id=9");
  // First should be Status, second Get_config (FIFO through bridge)
  assertEqual(r1[1].response[0], "Ok_status", "r1 is Status response");
  assertEqual(r2[1].response[0], "Ok_config", "r2 is Get_config response");

  proc.stdin.end();
  proc.kill();

  console.log("");
}

// ─── Direct TCP tests ───────────────────────────────────────────────────────

async function testTcp() {
  console.log("\n=== Direct TCP integration tests ===\n");

  const [host, portStr] = TEST_ADDR.split(":");
  const port = parseInt(portStr, 10);

  // 1. Register connection
  console.log("Register via TCP:");
  const client = await createTcpClient(host, port);

  client.send({
    id: 1,
    command: ["Register", { brand: "TCP Test" }],
  });

  const regResp = await client.read();
  assertEqual(regResp[0], "Response", "tcp register is Response");
  assertEqual(regResp[1].id, 1, "tcp register id=1");
  assertEqual(regResp[1].response[0], "Ok_registered", "tcp register Ok_registered");

  // Should get Config_updated push
  const push = await client.read();
  assertEqual(push[0], "Push", "tcp config push");
  assertEqual(push[1].id, 0, "tcp push id=0");

  // 2. Send command through registered connection
  console.log("\nStatus via registered TCP:");
  client.send({
    id: 2,
    command: ["Status"],
  });

  const statusResp = await client.read();
  assertEqual(statusResp[0], "Response", "tcp status is Response");
  assertEqual(statusResp[1].id, 2, "tcp status id=2");
  assertEqual(statusResp[1].response[0], "Ok_status", "tcp status Ok_status");

  // 3. One-shot connection (no Register)
  console.log("\nOne-shot TCP command:");
  const oneshot = await createTcpClient(host, port);
  oneshot.send({
    id: 10,
    command: ["Get_config"],
  });

  const oneshotResp = await oneshot.read();
  assertEqual(oneshotResp[0], "Response", "oneshot is Response");
  assertEqual(oneshotResp[1].id, 10, "oneshot id=10");
  assertEqual(oneshotResp[1].response[0], "Ok_config", "oneshot Ok_config");
  oneshot.close();

  client.close();

  console.log("");
}

// ─── Main ───────────────────────────────────────────────────────────────────

async function main() {
  const mode = process.argv[2] || "both";

  // Build first
  console.log("Building...");
  try {
    execSync("dune build @all", { cwd: path.join(__dirname, ".."), stdio: "inherit" });
  } catch (e) {
    console.error("Build failed");
    process.exit(1);
  }

  // Start test daemon
  console.log("Starting test daemon...");
  const { tmpDir, configPath } = createTestConfig();
  let daemon;
  try {
    daemon = await startDaemon(configPath);
    console.log(`Daemon running on ${TEST_ADDR} (pid ${daemon.pid})`);

    if (mode === "bridge" || mode === "both") await testBridge();
    if (mode === "tcp" || mode === "both") await testTcp();
  } catch (e) {
    console.error(`\nFATAL: ${e.message}`);
    failed++;
  } finally {
    if (daemon) daemon.kill();
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }

  console.log(`\nResults: ${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
}

main();

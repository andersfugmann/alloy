const {
  createMock,
  triggerPortMessage,
  triggerPortDisconnect,
  connectSubPort,
} = require("./chrome_mock");

let mock;

beforeEach(async () => {
  jest.resetModules();
  mock = createMock();
  global.chrome = mock.chrome;
  global.console.log = jest.fn();
  require("../main.bc.js");
  // Allow Lwt microtasks to process (bridge handshake + registration)
  await new Promise((resolve) => setTimeout(resolve, 10));
});

afterEach(() => {
  delete global.chrome;
});

describe("multiplexer port connection", () => {
  test("sub-port connects and registers onMessage listener", async () => {
    const subPort = connectSubPort(mock.listeners);
    await new Promise((resolve) => setTimeout(resolve, 10));
    expect(subPort.onMessage.addListener).toHaveBeenCalledTimes(1);
    expect(subPort.onDisconnect.addListener).toHaveBeenCalledTimes(1);
  });

  test("request through sub-port is forwarded to native port", async () => {
    const subPort = connectSubPort(mock.listeners);
    const nativePort = mock.ports[0];
    nativePort.postMessage.mockClear();
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Send a status request frame through the sub-port
    triggerPortMessage(subPort, { correlation_id: 1, payload: { command: "status", params: null } });
    await new Promise((resolve) => setTimeout(resolve, 10));

    // The request should be forwarded to the native port as a frame
    expect(nativePort.postMessage).toHaveBeenCalledTimes(1);
    const forwarded = nativePort.postMessage.mock.calls[0][0];
    expect(forwarded).toHaveProperty("correlation_id");
    expect(forwarded).toHaveProperty("payload");
    // The payload should contain the original command
    expect(forwarded.payload).toHaveProperty("command", "status");
  });

  test("response from native port is forwarded back to sub-port", async () => {
    const subPort = connectSubPort(mock.listeners);
    const nativePort = mock.ports[0];
    nativePort.postMessage.mockClear();
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Send request through sub-port
    triggerPortMessage(subPort, { correlation_id: 42, payload: { command: "status", params: null } });
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Get the ID assigned by the client for the forwarded request
    const forwarded = nativePort.postMessage.mock.calls[0][0];

    // Simulate server response on native port
    const statusPayload = { registered_tenants: ["t1"], uptime_seconds: 99 };
    triggerPortMessage(nativePort, {
      correlation_id: forwarded.correlation_id,
      payload: ["Ok", statusPayload],
    });
    await new Promise((resolve) => setTimeout(resolve, 10));

    // The sub-port should receive a response with the original correlation_id
    expect(subPort.postMessage).toHaveBeenCalled();
    const response = subPort.postMessage.mock.calls[0][0];
    expect(response.correlation_id).toBe(42);
    expect(response.payload[0]).toBe("Ok");
  });

  test("broadcast push is forwarded to all connected sub-ports", async () => {
    const subPort1 = connectSubPort(mock.listeners);
    const subPort2 = connectSubPort(mock.listeners);
    const nativePort = mock.ports[0];
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Simulate a Navigate push from the server (id=0 means push)
    triggerPortMessage(nativePort, {
      correlation_id: 0,
      payload: ["Navigate", "https://pushed.example.com"],
    });
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Both sub-ports should receive the broadcast
    expect(subPort1.postMessage).toHaveBeenCalled();
    expect(subPort2.postMessage).toHaveBeenCalled();

    const msg1 = subPort1.postMessage.mock.calls[0][0];
    const msg2 = subPort2.postMessage.mock.calls[0][0];
    expect(msg1.correlation_id).toBe(0);
    expect(msg2.correlation_id).toBe(0);
    expect(msg1.payload[0]).toBe("Navigate");
    expect(msg2.payload[0]).toBe("Navigate");
  });

  test("disconnected sub-port stops receiving broadcasts", async () => {
    const subPort = connectSubPort(mock.listeners);
    const nativePort = mock.ports[0];
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Disconnect the sub-port
    triggerPortDisconnect(subPort);
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Send a broadcast
    triggerPortMessage(nativePort, {
      correlation_id: 0,
      payload: ["Navigate", "https://example.com"],
    });
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Disconnected port should not receive anything
    expect(subPort.postMessage).not.toHaveBeenCalled();
  });

  test("multiple sub-ports can proxy requests independently", async () => {
    const subPort1 = connectSubPort(mock.listeners);
    const subPort2 = connectSubPort(mock.listeners);
    const nativePort = mock.ports[0];
    nativePort.postMessage.mockClear();
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Both ports send requests
    triggerPortMessage(subPort1, { correlation_id: 1, payload: { command: "status", params: null } });
    triggerPortMessage(subPort2, { correlation_id: 1, payload: { command: "get_rules", params: null } });
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Both should be forwarded with different IDs
    expect(nativePort.postMessage).toHaveBeenCalledTimes(2);
    const fwd1 = nativePort.postMessage.mock.calls[0][0];
    const fwd2 = nativePort.postMessage.mock.calls[1][0];
    expect(fwd1.correlation_id).not.toBe(fwd2.correlation_id);

    // Respond to both (in reverse order to test correlation_id routing)
    triggerPortMessage(nativePort, { correlation_id: fwd2.correlation_id, payload: ["Ok", []] });
    triggerPortMessage(nativePort, { correlation_id: fwd1.correlation_id, payload: ["Ok", { registered_tenants: [], uptime_seconds: 0 }] });
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Each sub-port should get a response with its original correlation_id
    expect(subPort1.postMessage).toHaveBeenCalled();
    expect(subPort2.postMessage).toHaveBeenCalled();
    const resp1 = subPort1.postMessage.mock.calls[0][0];
    const resp2 = subPort2.postMessage.mock.calls[0][0];
    expect(resp1.correlation_id).toBe(1);
    expect(resp2.correlation_id).toBe(1);
  });
});

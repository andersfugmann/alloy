const {
  createMock,
  triggerNavigation,
  triggerCommitted,
  triggerPortMessage,
  triggerPortDisconnect,
} = require("./chrome_mock");

let mock;

beforeEach(async () => {
  jest.resetModules();
  mock = createMock();
  global.chrome = mock.chrome;
  global.console.log = jest.fn();
  require("../main.bc.js");
  // Allow Lwt microtasks to process (Client.init waits for Registered push)
  await new Promise((resolve) => setTimeout(resolve, 10));
});

afterEach(() => {
  delete global.chrome;
});

describe("navigation interception", () => {
  test("sends OPEN command for top-level navigation", () => {
    const port = mock.ports[0];
    // Clear calls from initial Register + Get_config
    port.postMessage.mockClear();
    triggerNavigation(mock.listeners, "https://example.com", 1, 0);

    expect(port.postMessage).toHaveBeenCalledTimes(1);
    const msg = port.postMessage.mock.calls[0][0];
    // Wire.frame format: {correlation_id, payload}
    expect(msg).toHaveProperty("correlation_id");
    expect(msg).toHaveProperty("payload");
    const payload = msg.payload;
    expect(payload).toHaveProperty("command", "open");
    expect(payload.params).toEqual({ url: "https://example.com" });
  });

  test("ignores sub-frame navigations", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerNavigation(mock.listeners, "https://example.com", 1, 1);

    expect(port.postMessage).not.toHaveBeenCalled();
  });

  test("ignores chrome:// URLs", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerNavigation(mock.listeners, "chrome://settings", 1, 0);

    expect(port.postMessage).not.toHaveBeenCalled();
  });

  test("ignores about: URLs", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerNavigation(mock.listeners, "about:blank", 1, 0);

    expect(port.postMessage).not.toHaveBeenCalled();
  });

  test("handles NAVIGATE push by opening a tab", () => {
    const port = mock.ports[0];
    // Wire.frame push format: {correlation_id: 0, payload}
    triggerPortMessage(port, { correlation_id: 0, payload: ["Navigate", "https://pushed.example.com"] });

    expect(mock.chrome.tabs.create).toHaveBeenCalledWith({
      url: "https://pushed.example.com",
    });
  });

  test("re-routes on commit after a server redirect", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerCommitted(mock.listeners, "https://final.example.com", 1, 0, [
      "server_redirect",
    ]);

    expect(port.postMessage).toHaveBeenCalledTimes(1);
    const msg = port.postMessage.mock.calls[0][0];
    expect(msg.payload).toHaveProperty("command", "open");
    expect(msg.payload.params).toEqual({ url: "https://final.example.com" });
  });

  test("re-routes on commit after a client redirect", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerCommitted(mock.listeners, "https://final.example.com", 1, 0, [
      "client_redirect",
    ]);

    expect(port.postMessage).toHaveBeenCalledTimes(1);
    expect(port.postMessage.mock.calls[0][0].payload.command).toBe("open");
  });

  test("ignores commit without redirect qualifier", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerCommitted(mock.listeners, "https://example.com", 1, 0, ["from_address_bar"]);

    expect(port.postMessage).not.toHaveBeenCalled();
  });

  test("ignores commit on sub-frame", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerCommitted(mock.listeners, "https://example.com", 1, 1, [
      "server_redirect",
    ]);

    expect(port.postMessage).not.toHaveBeenCalled();
  });

  test("does not send commands when disconnected", async () => {
    const port = mock.ports[0];
    triggerPortDisconnect(port);
    // Multiple microtask ticks needed for the Lwt promise chain:
    // stream close → Client.close → Client.closed → handle_disconnect
    for (let i = 0; i < 5; i++) {
      await new Promise((resolve) => setTimeout(resolve, 10));
    }

    port.postMessage.mockClear();
    triggerNavigation(mock.listeners, "https://example.com", 1, 0);
    triggerNavigation(mock.listeners, "https://another.com", 2, 0);
    expect(port.postMessage).not.toHaveBeenCalled();
  });
});

describe("response handling", () => {
  test("processes Local response without creating tabs", () => {
    const port = mock.ports[0];
    port.postMessage.mockClear();
    triggerNavigation(mock.listeners, "https://example.com", 1, 0);

    // The client assigned id=1 for the first command after init
    const sentMsg = port.postMessage.mock.calls[0][0];
    // Wire.frame response format: {correlation_id, payload: ["Ok", <json>]}
    triggerPortMessage(port, { correlation_id: sentMsg.correlation_id, payload: ["Ok", ["Local"]] });

    // Should not create a tab for local routing
    expect(mock.chrome.tabs.create).not.toHaveBeenCalled();
  });
});

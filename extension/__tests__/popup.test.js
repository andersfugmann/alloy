const {
  createMock,
  sendPopupMessage,
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

describe("popup messages", () => {
  test("protocol command returns error when disconnected", async () => {
    const { triggerPortDisconnect } = require("./chrome_mock");
    triggerPortDisconnect(mock.ports[0]);

    // Allow Lwt microtask to process disconnect
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Send frame-format status command
    const response = await sendPopupMessage(mock.listeners, {
      id: 1,
      payload: { command: "status", params: null },
    });

    // Wire.frame response with Failure
    expect(response).toHaveProperty("id");
    expect(response.payload).toEqual(["Failure", "Not connected"]);
  });

  test("unknown action returns error", async () => {
    const response = await sendPopupMessage(mock.listeners, {
      action: "nonexistent",
    });

    expect(response).toHaveProperty("error", "unknown action");
  });

  test("invalid message returns error", async () => {
    const response = await sendPopupMessage(mock.listeners, {
      foo: "bar",
    });

    expect(response).toHaveProperty("error", "invalid message");
  });

  test("reconnect attempts new connection", async () => {
    const { triggerPortDisconnect } = require("./chrome_mock");
    triggerPortDisconnect(mock.ports[0]);

    await new Promise((resolve) => setTimeout(resolve, 10));

    const response = await sendPopupMessage(mock.listeners, {
      action: "reconnect",
    });

    // connect() is async: returns immediately, port set via event stream
    // Verify reconnection was attempted
    expect(mock.chrome.runtime.connectNative).toHaveBeenCalledTimes(2);
    expect(response).toHaveProperty("connected");
  });
});

describe("context menus", () => {
  test("creates context menus on install", () => {
    // onInstalled was triggered during module load
    mock.listeners.onInstalled.forEach((cb) => cb({ reason: "install" }));

    expect(mock.chrome.contextMenus.create).toHaveBeenCalledWith(
      expect.objectContaining({ id: "open_in" })
    );
    expect(mock.chrome.contextMenus.create).toHaveBeenCalledWith(
      expect.objectContaining({ id: "send_to" })
    );
  });
});

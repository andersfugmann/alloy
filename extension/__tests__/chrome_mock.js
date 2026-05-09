// Chrome API mock for testing the compiled extension.
// Captures event listeners so tests can trigger events programmatically.

function createMock() {
  const listeners = {
    onBeforeNavigate: [],
    onContextMenuClicked: [],
    onInstalled: [],
    onStartup: [],
    onMessage: [],
    onConnect: [],
  };

  const ports = [];

  function createPort() {
    const msgListeners = [];
    const disconnectListeners = [];
    const port = {
      postMessage: jest.fn((msg) => {
        // Handle bridge handshake: respond to connect request
        if (msg && msg.msg === "connect") {
          setTimeout(() => {
            msgListeners.forEach((cb) =>
              cb({ msg: "connected", result: ["Connected", { status: "connected", hostname: "test-host" }] })
            );
          }, 0);
          return;
        }
        // Auto-respond with Registered push when Register frame is received
        if (msg && msg.id === 0 && msg.payload && msg.payload.command === "register") {
          setTimeout(() => {
            msgListeners.forEach((cb) =>
              cb({ id: 0, tenant: "", payload: ["Registered", { tenant_id: "test_tenant" }] })
            );
          }, 0);
        }
      }),
      onMessage: { addListener: jest.fn((cb) => msgListeners.push(cb)) },
      onDisconnect: { addListener: jest.fn((cb) => disconnectListeners.push(cb)) },
      _msgListeners: msgListeners,
      _disconnectListeners: disconnectListeners,
    };
    ports.push(port);
    return port;
  }

  const chrome = {
    runtime: {
      connectNative: jest.fn(() => createPort()),
      connect: jest.fn(() => createPort()),
      onInstalled: {
        addListener: jest.fn((cb) => listeners.onInstalled.push(cb)),
      },
      onStartup: {
        addListener: jest.fn((cb) => listeners.onStartup.push(cb)),
      },
      onMessage: {
        addListener: jest.fn((cb) => listeners.onMessage.push(cb)),
      },
      onConnect: {
        addListener: jest.fn((cb) => listeners.onConnect.push(cb)),
      },
      getURL: jest.fn((path) => `chrome-extension://test/${path}`),
      lastError: null,
      sendMessage: jest.fn((_msg, cb) => { if (cb) cb(null); }),
    },
    tabs: {
      create: jest.fn(),
      remove: jest.fn(),
      get: jest.fn((tabId, cb) => cb({ id: tabId, url: "https://example.com", title: "Example" })),
      query: jest.fn((_query, cb) => cb([{ url: "https://example.com", id: 1 }])),
    },
    storage: {
      local: {
        get: jest.fn((_keys, cb) => cb({})),
        set: jest.fn((_items, cb) => { if (cb) cb(); }),
      },
    },
    webNavigation: {
      onBeforeNavigate: {
        addListener: jest.fn((cb) => listeners.onBeforeNavigate.push(cb)),
      },
    },
    contextMenus: {
      create: jest.fn(),
      removeAll: jest.fn((cb) => { if (cb) cb(); }),
      onClicked: {
        addListener: jest.fn((cb) => listeners.onContextMenuClicked.push(cb)),
      },
    },
    action: {
      setIcon: jest.fn(),
    },
  };

  // Provide globals that js_of_ocaml code may access
  global.navigator = global.navigator || {};
  global.navigator.userAgentData = { brands: [{ brand: "Google Chrome" }] };
  global.performance = global.performance || { now: () => 0 };

  return { chrome, listeners, ports };
}

// Simulate a navigation event
function triggerNavigation(listeners, url, tabId, frameId) {
  listeners.onBeforeNavigate.forEach((cb) =>
    cb({ url, tabId, frameId })
  );
}

// Simulate a native port message arriving
function triggerPortMessage(port, msg) {
  port._msgListeners.forEach((cb) => cb(msg));
}

// Simulate port disconnect
function triggerPortDisconnect(port) {
  port._disconnectListeners.forEach((cb) => cb());
}

// Create a sub-port and trigger onConnect so the multiplexer registers it.
// Returns the port object for further interaction.
function connectSubPort(listeners) {
  const msgListeners = [];
  const disconnectListeners = [];
  const port = {
    postMessage: jest.fn(),
    onMessage: { addListener: jest.fn((cb) => msgListeners.push(cb)) },
    onDisconnect: { addListener: jest.fn((cb) => disconnectListeners.push(cb)) },
    _msgListeners: msgListeners,
    _disconnectListeners: disconnectListeners,
  };
  listeners.onConnect.forEach((cb) => cb(port));
  return port;
}

module.exports = {
  createMock,
  triggerNavigation,
  triggerPortMessage,
  triggerPortDisconnect,
  connectSubPort,
};

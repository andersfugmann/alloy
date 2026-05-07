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
        // Auto-respond with Registered push when Register frame is received
        if (msg && msg.id === 0 && msg.payload && msg.payload.command === "register") {
          setTimeout(() => {
            msgListeners.forEach((cb) =>
              cb({ id: 0, tenant: null, payload: ["Registered", { tenant_id: "test_tenant" }] })
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

// Send a popup message and capture the response
function sendPopupMessage(listeners, message) {
  return new Promise((resolve) => {
    listeners.onMessage.forEach((cb) => {
      cb(message, {}, resolve);
    });
  });
}

module.exports = {
  createMock,
  triggerNavigation,
  triggerPortMessage,
  triggerPortDisconnect,
  sendPopupMessage,
};

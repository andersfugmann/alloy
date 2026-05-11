# Alloy

URL routing for Linux desktops. A daemon on the host routes URLs between
isolated browser instances in different tenants (the host itself, or
systemd-nspawn containers), identified by hostname.

## Features

- Rule-based URL routing between browser profiles in different tenants
  (host or containers).
- On-demand browser launch for unregistered target tenants.
- Shared browsing history collected from every tenant, with cross-tenant
  search from the extension side panel. Firefox history can be imported
  via `alloy import-firefox`.
- Browser extension (Chromium / Edge) with popup, options page, history
  side panel, and context-menu actions for routing and rule management.
- CLI and `xdg`-compatible desktop entry so Alloy can be set as the
  default browser.

## Server (alloyd)

`alloyd` is the host-side daemon. It listens on one or more TCP addresses
(loopback by default) and accepts two kinds of connections:

- *Registered* connections, opened by the per-tenant bridge on startup.
  These are long-lived and read-only from the client side; the daemon
  pushes `NAVIGATE` messages on them when a URL should open in that
  tenant.
- *Command* connections, used for one-shot requests (open a URL, query
  status, read or update configuration and rules).

The daemon owns the tenant registry, the cooldown map, and the
configuration and rules files. When a URL arrives, it evaluates the
ordered rule list, resolves the target tenant, and either keeps the URL
local (responding `LOCAL`) or pushes it to the target tenant's
registered connection (responding `REMOTE`). If the target tenant is
not registered, the daemon can launch its browser via the configured
`browser_cmd` and wait for it to register.

See [SPEC.md](SPEC.md) for the full protocol specification.

## Installation

Two `.deb` packages are provided via the
[releases workflow](../../actions/workflows/deb.yml).

### Host

```bash
sudo dpkg -i alloyd_<version>_amd64.deb
systemctl --user enable --now alloyd
```

### Each Tenant / Container

```bash
sudo dpkg -i alloy_<version>_amd64.deb
```

Installs the bridge/CLI, native messaging manifests (Chromium and Edge),
and a `.desktop` entry for use as the default URL handler.

**Browser extension (all browsers):** Load the extension manually:

1. Open `chrome://extensions` (or `edge://extensions`)
2. Enable **Developer mode** (toggle in top-right)
3. Click **Load unpacked**
4. Select `/usr/share/alloy/extension`

The extension updates in place when the package is upgraded — restart
the browser (or click the reload ↻ button on the extensions page) to
pick up changes.

The daemon must be reachable from each tenant over TCP. For containers,
ensure the container's network can reach the host on the configured port
(default 7120), or add the container's subnet to `allowed_networks`:

```json
{
  "listen": [
    { "host": "0.0.0.0", "port": 7120 },
    { "host": "::", "port": 7120 }
  ],
  "allowed_networks": ["127.0.0.0/8", "::1/128", "10.0.0.0/8"]
}
```

## Configuration

The daemon reads `~/.config/alloy/config.json` (or a path given as its
first argument). Routing rules live in a separate file,
`~/.config/alloy/rules.json`. See [`config.example.json`](config.example.json)
and [`rules.example.json`](rules.example.json).

### `config.json`

```json
{
  "listen": [
    { "host": "127.0.0.1", "port": 7120 },
    { "host": "::1", "port": 7120 }
  ],
  "allowed_networks": ["127.0.0.0/8", "::1/128"],
  "tenants": {
    "host-machine": {
      "label": "Host",
      "color": "#4285F4",
      "brand": "Google Chrome"
    },
    "work-container": {
      "browser_cmd": "machinectl shell work-container /usr/bin/chromium",
      "label": "Work",
      "color": "#EA4335"
    }
  },
  "defaults": {
    "unmatched": "local",
    "cooldown_seconds": 2,
    "browser_launch_timeout": 10
  }
}
```

| Field | Description |
|-------|-------------|
| `listen` | List of `{ host, port }` records to listen on (default loopback IPv4 and IPv6 on port 7120). |
| `allowed_networks` | CIDR list of networks allowed to connect (default loopback only). |
| `tenants` | Hostname to `{ browser_cmd?, label, color, brand? }`. Keys must match actual hostnames. `brand` is optional and auto-populated on registration. `browser_cmd` is only needed for tenants the daemon should be able to launch (typically containers). |
| `defaults.unmatched` | `"local"` or a tenant hostname for unmatched URLs. |
| `defaults.cooldown_seconds` | Suppress repeated (tenant, URL) routing within this window. |
| `defaults.browser_launch_timeout` | Seconds to wait for browser registration after running `browser_cmd`. |

### `rules.json`

```json
[
  { "pattern": "https://github[.]com/.*", "target": "work-container", "enabled": true },
  { "pattern": "https://mail[.]google[.]com/.*", "target": "host-machine", "enabled": true }
]
```

Rules are regex patterns evaluated top-to-bottom; the first enabled
match wins. Both files can be edited directly or modified through the
extension (config and rules views, plus the *Add routing rule* and
*Delete matching rule* context-menu items).

## Usage

### CLI

```bash
alloy open <url>              # Route a URL through the daemon
alloy import-firefox          # Import Firefox history into Alloy
```

Connection defaults to `127.0.0.1:7120`. Override with `--host`/`-H`
and `--port`/`-p`.

To set Alloy as the default URL handler:

```bash
xdg-settings set default-web-browser alloy.desktop
```

### Browser Extension

The extension runs as a Chromium service worker that communicates with
the daemon through the native messaging bridge.

#### Navigation Interception

Every top-frame HTTP/HTTPS navigation is sent to the daemon as an `OPEN`
command (the bridge injects the tenant ID transparently). Based on the
response:

| Response | Behaviour |
|----------|-----------|
| **LOCAL** | Navigation proceeds normally in the current browser. |
| **REMOTE** | The URL has been pushed to the target tenant's browser; logged to the service worker console. |
| **ERR** | Error is logged; navigation proceeds. |

Internal URLs (`chrome://`, `about:`, `chrome-extension://`) are ignored.

#### Receiving URLs

When the daemon pushes a `NAVIGATE` message (another tenant routed a URL
here), the extension opens a new tab with that URL.

#### Popup

Click the extension icon to open a small panel:

- **Connection indicator** — green dot (connected) or red dot
  (disconnected) showing the native messaging host status.
- **View status** — queries the daemon for registered tenants and
  uptime; displays the JSON response.
- **View config** — queries the daemon for the current configuration;
  displays the JSON response.
- **Reconnect** — re-establishes the native messaging connection if it
  was lost.

#### Context Menus

Right-click context menus provide quick routing actions:

| Context | Menu item | Action |
|---------|-----------|--------|
| **Link** | *Open in tenant…* | Routes the link URL through the daemon. |
| **Page** | *Send page to tenant…* | Routes the current page URL through the daemon. |
| **Page** | *Add routing rule…* | Opens a dialog to create a new rule (regex + tenant). The pattern is pre-filled from the current page's origin. |
| **Page** | *Delete matching rule* | Tests the current URL against rules and deletes the matching rule. |

## Building from Source

```bash
opam install . --deps-only --with-test
cd extension && npm install && cd ..
```

A `Makefile` wraps the common `dune` and packaging invocations. Run
`make help` for the list of available targets.

`debian/rules` calls `dune build --profile release` for optimised output.

## License

See [LICENSE](LICENSE) for details.

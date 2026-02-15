# wlsunset-sni

Standalone StatusNotifierItem (SNI) / AppIndicator tray icon for controlling `wlsunset`.

## Features

- Shows a tray icon whose tooltip reflects `wlsunset` state.
- Left click opens a menu:
  - Start/stop
  - Select mode (auto / forced high / forced low)
  - Set temperature presets / reset

## Usage

```sh
wlsunset-sni --command 'wlsunset -l 38.9 -L -77.0'
```

## Notes

This program exports an `org.kde.StatusNotifierItem` and a `com.canonical.dbusmenu` menu.


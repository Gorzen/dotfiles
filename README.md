# My NixOS config

## Rationale

### Goal

Declaratively define my OS (both the system and user configs) and do it in a reproducible way!

### Tools

- NixOS for system config
- Home-manager for user config

### Design

*Minimize obfuscations and config sizes*

- The whole config should be declarative and built with NixOS
- Configure programs in their standard format (e.g. not in nix itself)
  - Home-manager should only put user config files at the correct place, not generate them per se
- Use minimal options from home-manager (mostly `xdg.file`)
- Try to minimise config sizes - focus on essentials
  - The config should be stable without constant tinkering

## Install


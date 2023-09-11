# My NixOS config

## Rationale

### Goal

Declaratively define my OS (both the system and user configs) and do it in a reproducible way!

### Tools

- NixOS for system config
- Home-manager for user config

### Design

*Minimize obfuscations and config sizes*

- Focus on essentials (config what is useful)
- Avoid hacks (all declarative)
- Configure programs in their standard format (e.g. not in nix itself)
  - Home-manager should only put user config files at the correct place, not generate them
- Use minimal options from home-manager (mostly `xdg.file`)

## Install


# My NixOS config 🚀⭐

## Rationale

### Goal

Declaratively define my OS (both the system and user configs) and do it in a reproducible way!

### Tools

- NixOS for system config
- Home-manager for user config

### Design

*Minimize obfuscations and config sizes - try to avoid hacks*

- The whole config should be declarative and built with NixOS
- Configure programs in their standard format (e.g. not in nix itself)
  - Home-manager should only put user config files at the correct place, not generate them per se
- Try to minimise config sizes - focus on essentials
  - The config should be stable without constant tinkering

## Install

*Warning: This is my personal config. Don't expect this to work oob for you. But, I'm very happy if you find some useful bits in it* 🙌

Example installation commands:

1. Follow [NixOS install instructions](https://nixos.org/manual/nixos/stable/#sec-installation-manual) to:
   1. Connect to the internet
   2. Create partitions
   3. Mount partitions
   4. Generate default NixOS config (in `/mnt/etc/nixos/`)
2. `cd /mnt/etc/nixos`
3. `nix-shell -p git`
4. `mv configuration.nix hardware-configuration.nix /home/nixos`
5. `git clone https://github.com/Gorzen/dotfiles.git .`
6. `mv /home/nixos/hardware_configuration.nix .`
7. Add [home-manager](https://nix-community.github.io/home-manager/index.html#sec-install-nixos-module) channel
8. Add [nixos-hardware](https://github.com/NixOS/nixos-hardware) channel
9. `nix-channel --update`
10. Make changes if necessary to `configuration.nix`
11. `nixos-install`
12. Set root password when prompted (at the end of installation)
13. `reboot`
14. Go to a console tty
15. Login as root
16. `passwd lulu`
17. Go back to login manager
18. Enjoy!

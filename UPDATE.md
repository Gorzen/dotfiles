# Update

Nix is the system's package manager but there are other programs that handles packages/versions that need update

## NixOS - Nix

```sh
# Update all channels and rebuild a generation with all nix packages upgraded
sudo nixos-rebuild --upgrade-all switch
```

## Firware

Use `fwupdmgr` to update firmare.

Use this alias to upgrade:

```sh
alias firmware-update='fwupdmgr refresh; fwupdmgr get-updates; fwupdmgr update'
```

## Rustup - Rust

Install and set default toolchain as stable

```sh
rustup default stable
```

Update Rust toolchains

```sh
# self-update is disabled in nix's build of rustup
# any updates to rustup will need to be fetched with nix
rustup update
```


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

## NeoVim

### Lazy

In nvim use the `Lazy` command to update packages

### nvim-metals

In nvim use the `MetalsUpdate` command to update metals

Note: nvim-metals needs to be enabled (launch nvim in a folder with scala files)


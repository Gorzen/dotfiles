# TODO

- [ ] ls colors (nnn?)
- [ ] Find some way to allow changing config for specific hardware? Ignored file where you can import some file you want?
- [ ] Fix zsh shortcuts ctrl-l ctrl-w etc
- [ ] Own terminal color theme?
- [ ] Re-enable nixos-hardware
- [ ] Add bar to nvim
- [ ] Add file tree to nvim
- [ ] Clean system packages, sort in categories
- [ ] Clean configuration.nix, sort it
- [ ] Set rofi theme
- [ ] Add dunst pretty theme
- [ ] Put wallpapers in a git repo?
- [ ] Add lock
- [ ] Lock when closing lid
- [ ] Install languagues I use in my system
  - [ ] Haskell (kind of done, but maybe would be better to use ghcup, although that doesn't exist in NixOS packages)
  - [ ] Rust (rustup)
  - [ ] Scala (scala-cli + sbt ?) ?
  - [ ] Python (python312 + poetry ?)
  - [ ] Rest of languages, get in a nix-shell
- [ ] xmobar
  - [ ] Time & date
  - [ ] Sound (pavucontrol on click)
  - [ ] cpu? memory?
  - [ ] audio/media being played?
  - [ ] Window settings (file, options, ...)?
  - [ ] Battery capacity and status (check default xmobar config arch wiki, it contained battery and uptime estimate)
  - [ ] External disks (list, mount, unmount, power-off)
  - [ ] Prettify xmobar
- [ ] xmonad
  - [ ] Keybinding to make window floating
  - [ ] Way to easily resize floating windows
  - [ ] Colors borders
  - [ ] Gaps
  - [ ] Prettify tabbed layout
  - [ ] Some easy way to put window back on top? Not drawn beneath xmboar after recompiling and restarting xmonad
  - [ ] Add keybindings for screenshots
- [ ] Add a central place to define mutltiple useful nix-shells? Plus a script to easily use them?
- [ ] Have feedback when increasing/decreasing/muting sound
- [ ] Show desktop environments in SDDM even if only 1
- [ ] Try lazygit
- [ ] Warning notification on low battery
- [ ] Is tabbed layout useful since can change apps in fullscreen layout? If keep app names are in xmobar, fullscreen is basically tabbed. If not, can have tabbed.
- [ ] Show menu when pressing power button on laptop

Could have xmobarrc and one hs file for xmonad (file imported in xmonad.hs) generated in nix for system wide colors and bar size and trayer padding (colors could be used by nvim and wezterm as well). Then I could have my own system wide color theme. Could also use it in rofi or dmenu or others.
For local compilation of xmonad, write placeholder file with everything undefined. That way it compiles for hls. Or generate both files? In home and in config.

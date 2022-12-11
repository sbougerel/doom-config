# My Doom Emacs Setup

## Installation

### Archlinux

Install Emacs with nativecomp first and dependencies:
```bash
yay -Sy emacs-nativecomp fd ripgrep cmake
```

Clone config:
``` bash
git clone https://github.com/sbougerel/doom-config.git ~/.doom.d
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
```

Add `$HOME/.emacs.d/bin` to `$PATH` and finish installation: 
``` bash
doom install
```

Verify everything looks good:
``` bash
doom doctor
```

### MacOS

TODO

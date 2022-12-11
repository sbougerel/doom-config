# My Doom Emacs Setup

## Installation

### Archlinux

Install Emacs with nativecomp first and dependencies:
```bash
yay -Sy emacs-nativecomp fd ripgrep cmake
```

### MacOS

``` bash
brew install emacs-plus@28 fd ripgrep cmake
```

### Common steps

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

## LSP dependencies

### Archlinux

Add dependencies for `Rust`, `Python`:
``` bash
yay -Sy rust-src pyright
```

### MacOS

Add dependencies for `Python`, `sh`:
``` bash
brew install pyright pipevn shellcheck 
```

# My Doom Emacs Setup

## Installation

### Archlinux

Install Emacs with native compilation first and dependencies:

```sh
yay -Sy emacs-nativecomp fd ripgrep cmake
```

### MacOS

```sh
brew install emacs-plus@28 fd ripgrep cmake
```

### Common steps

Clone configuration:

```sh
git clone https://github.com/sbougerel/doom-config.git ~/.doom.d
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
```

Add `$HOME/.emacs.d/bin` to `$PATH` and finish installation:

```sh
doom install
```

Verify everything looks good:

```sh
doom doctor
```

Continue below to add required system packages for each functionality.

## Checkers

### Spell

Archlinux:

```sh
pacman -S aspell \
  aspell-en \
  hunspell \
  hunspell-en_us \
  hunspell-en_gb
```

## Tools

### Language Server Protocol

Archlinux:

```sh
pacman -S npm
```

## Languages

### Org-mode

Archlinux:

```sh
pacman -S texlive-core \
  texlive-bin \
  texlive-science \
  gnuplot \
  sqlite3
```

### Text

Archlinux:

```sh
pacman -S prettier
```

### Markdown

Archlinux:

```sh
pacman -S proselint marked
```

### Python

Archlinux:

```sh
pacman -S pyright \
  python-pip \
  python-pipenv \
  python-black \
  mypy \
  python-pyflakes \
  python-isort \
  python-pytest \
  python-nose
```

MacOS:

```sh
brew install pyright pipenv black mypy
```

### Rust

Archlinux:

```sh
pacman -S rustup lldb
rustup default stable
rustup component add rust-analyzer
mkdir ~/.cargo/bin
ln  -s `rustup which --toolchain stable rust-analyzer` ~/.cargo/bin/
export PATH=$HOME/.cargo/bin:$PATH # Also add to init scripts
```

### shell

Archlinux:

```sh
pacman -S shellcheck shfmt
```

MacOS:

```sh
brew install shellcheck
```

### CC

Archlinux:

```sh
pacman -S glslang
```

### Golang

```sh
# TODO
```

### Debugger

Archlinux:

```sh
pacman -S gdb lldb nodejs llvm
# AUR package required next
yay -S lldb-mi-git
```

### Profiling

Archlinux:

```sh
pacman -S valgrind graphviz
# Utility to convert callgrind
pip install gprof2dot
```

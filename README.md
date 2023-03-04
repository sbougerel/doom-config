# My Doom Emacs Setup

## Installation

### Archlinux

Install Emacs with nativecomp first and dependencies:

```bash
yay -Sy emacs-nativecomp fd ripgrep cmake
```

### MacOS

```bash
brew install emacs-plus@28 fd ripgrep cmake
```

### Common steps

Clone config:

```bash
git clone https://github.com/sbougerel/doom-config.git ~/.doom.d
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
```

Add `$HOME/.emacs.d/bin` to `$PATH` and finish installation:

```bash
doom install
```

Verify everything looks good:

```bash
doom doctor
```

## Language dependencies

### Org-mode

Archlinux

```bash
pacman -S texlive-core \
  texlive-bin \
  texlive-science \
  gnuplot \
  sqlite3
```

### Text, markdown

Archlinux

```bash
pacman -S hunspell hunspell-en_GB prettier
```

### Python

Archlinux:

```bash
pacman -S pyright python-pip python-pipenv python-black mypy
```

MacOS:

```bash
brew install pyright pipenv black mypy
```

### Rust

Archlinux:

```bash
pacman -S rustup lldb
rustup default stable
```

### shell

Archlinux:

```bash
pacman -S shellcheck shfmt
```

MacOS:

```bash
brew install shellcheck
```

### Golang

TODO

### Debugger

Archlinux:

```bash
pacman -S gdb lldb nodejs llvm
# AUR package required next
yay -S lldb-mi-git
```

### Profiling

Archlinux:

```bash
pacman -S valgrind graphviz
# Utility to convert callgrind
pip install gprof2dot
```

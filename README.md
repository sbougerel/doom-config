# My Doom Emacs Setup

I switched to Doom recently, abandoning my old setup, which was becoming more expensive to maintain. I'm loving Doom so far, it lets me focus on the stuff I want to configure, while all the packaging, stability work and testing is done by the community. I don't think I'll switch back, so I might just nuke my old repository out of existence once I have transferred most of it's functionality.

## Fonts

I like the elegance of both JetBrains and DejaVu monospaced font. The DejaVu fonts are slightly more compact, without sacrificing legibility, however JetBrains fonts really shine with the use of ligatures, and this font's ligatures work well in Emacs.

For the variable pitch font I use Source Serif Pro which has spacing and height close to (but not exactly matching) JetBrains' fonts.

Archlinux:

```sh
pacman -S ttf-jetbrains-mono-nerd adobe-source-serif-fonts
```

MacOs:

``` sh
cd `mktemp -d`
curl -L 'https://fonts.google.com/download?family=Source%20Serif%20Pro' -o 1.zip
curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/JetBrainsMono.zip -o 2.zip
unzip -o 1.zip
unzip -o 2.zip
mv *.ttf ~/Library/Fonts/
```

## Emacs Installation

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

MacOS:

```sh
brew install aspell
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

MacOS:

```sh
brew install pngpaste
brew install graphviz
brew install gnuplot
brew install sqlite
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
pip install pytest
pip install nose
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

MacOS:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add rust-src
rustup component add rust-analyzer
ln -s `rustup which --toolchain stable rust-analyzer` $HOME/.cargo/bin/rust-analyzer
```

### shell

Archlinux:

```sh
pacman -S shellcheck shfmt
```

MacOS:

```sh
brew install shellcheck
brew install shfmt
```

### CC

Archlinux:

```sh
pacman -S glslang
```

MacOS:

```sh
brew install glslang
```

### Golang

MacOS:

```sh
brew install go
go install github.com/cweill/gotests/gotests@latest
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

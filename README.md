# My Doom Emacs Setup

I switched to Doom recently, abandoning my old setup, which was becoming more expensive to maintain. I'm loving Doom so far, it lets me focus on the stuff I want to configure, while benefiting from the Doom community resources. Since I don't think I'll switch back, I'll just nuke my old repository out of existence once I have transferred most of its functionality.

## Emacs Installation

Install Emacs with native compilation and dependencies first.

Archlinux:

```sh
yay -Sy emacs-nativecomp fd ripgrep cmake unzip
```

MacOS additionally requires GnuPG from `homebrew`, to manage my secrets:

```sh
brew install emacs-plus@28 fd ripgrep cmake gnupg
```

Common steps across all systems:

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

## Fonts Installation

I like the elegance of JetBrains Mono and its ligatures. For the variable pitch font I use Source Serif Pro which has height close to (but not exactly matching) JetBrains' Mono font.

Archlinux:

```sh
pacman -S ttf-jetbrains-mono-nerd adobe-source-serif-fonts
```

MacOs:

```sh
cd `mktemp -d`
curl -L 'https://fonts.google.com/download?family=Source%20Serif%20Pro' -o 1.zip
curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/JetBrainsMono.zip -o 2.zip
unzip -o 1.zip
unzip -o 2.zip
mv *.ttf ~/Library/Fonts/
```

## Syntax parsing

As of Emacs 29, tree-sitter is becoming more widely adopted, however it relies on user actions for now. My setup is a deterministic approach rather than a lazy-loading approach, which leaves less room for surprises and slow downs. Execute the following post installation to fetch parsers:

``` emacs-lisp
(treesit-auto-install-all)
```

## Checkers

### Spell

Archlinux:

```sh
pacman -S aspell \
  aspell-en \
  hunspell \
  hunspell-en_us \
  hunspell-en_gb
# The following bootstraps the personal English dictionary:
mkdir -p ~/.emacs.d/.local/etc/ispell
echo personal_ws-1.1 en 0 > ~/.emacs.d/.local/etc/ispell/.pws
```

MacOS:

```sh
brew install aspell
# The following bootstraps the personal English dictionary:
mkdir -p ~/.emacs.d/.local/etc/ispell
echo personal_ws-1.1 en 0 > ~/.emacs.d/.local/etc/ispell/.pws
```

### Grammar

Archlinux:

```sh
pacman -S languagetool
```

MacOS:

```sh
brew install languagetool
```

## Tools

### Language Server Protocol

Archlinux:

```sh
pacman -S npm
```

## Languages

### Text

Archlinux:

```sh
pacman -S prettier
```

### Org-mode

Archlinux:

```sh
pacman -S texlive-core \
  texlive-bin \
  texlive-science \
  textlive-plaingeneric \
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

### Markdown

Install `proselint` even if I disabled it by default as it's too pedantic. Manually enable it with `C-u C-c ! x` when necessary.

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
  python-nose \
  python-debuggpy
```

MacOS:

```sh
brew install pyright pipenv black mypy
pip install pytest
pip install nose
pip install debugpy
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

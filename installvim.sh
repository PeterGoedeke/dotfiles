#!/usr/bin/env bash

location="$HOME/dotfiles"
config="$location/.config"

function kitty_themes {
  mkdir -p "$HOME/.config/kitty"

  KITTY_THEMES_DIR="$HOME/.config/kitty/kitty-themes"
  if [[ ! -d "$KITTY_THEMES_DIR" ]]; then
    git clone --depth 1 https://github.com/dexpota/kitty-themes.git \
      "$KITTY_THEMES_DIR"
  fi
}

function link_files {
  mkdir -p "$HOME/.config/nvim"
  mkdir -p "$HOME/.config/tmux"
  mkdir -p "$HOME/.config/kitty"

  ln -bs "$config/nvim/init.lua" "$HOME/.config/nvim/init.lua"
  ln -bs "$config/kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"
  ln -bs "$config/tmux/tmux.conf" "$HOME/.config/tmux/tmux.conf"
  ln -bs "$location/.zshrc" "$HOME/.zshrc"
  ln -bs "$location/.zshlocal" "$HOME/.zshlocal"
}

function install_mac {
  brew install neovim tmux nnn fzf git-delta lazygit autojump
  brew install --cask kitty
}

function install_arch {
  sudo pacman -S --noconfirm \
    kitty \
    neovim \
    tmux \
    nnn \
    fzf \
    git-delta \
    lazygit \
    autojump-rs
}

function install_redhat {
  sudo dnf copr enable atim/lazygit -y

  sudo dnf -qy install \
    kitty \
    neovim \
    tmux \
    fzf \
    git-delta \
    lazygit \
    autojump
}

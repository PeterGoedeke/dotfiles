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
}

function install_mac {
  brew install tmux lazygit neovim fzf autojump nnn
  brew install --cask kitty
}

function install_arch {
  sudo pacman -S --noconfirm \
    kitty \
    tmux \
    lazygit \
    neovim \
    fzf \
    nnn \
    autojump-rs
}

function install_redhat {
  sudo dnf copr enable atim/lazygit -y

  sudo dnf -qy install \
    kitty \
    tmux \
    lazygit \
    neovim \
    fzf \
    autojump
}

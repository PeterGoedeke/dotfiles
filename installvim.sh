#!/usr/bin/env bash

location="$HOME/dotfiles"
config="$location/.config"

# function install_dependencies {
#   # install kitty, tmux, pip3, ranger-fm, pynvim, neovim
# }

function link_files {
  mkdir -p "$HOME/.config/{nvim,tmux,kitty}"

  ln -bs "$config/nvim/init.lua" "$HOME/.config/nvim/init.lua"
  ln -bs "$config/kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"
  ln -bs "$config/tmux/tmux.conf" "$HOME/.config/tmux/tmux.conf"
}

link_files

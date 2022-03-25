#!/usr/bin/env bash

location="$HOME/dotfiles"

ln -s "$location/.zshrc" "$HOME/.zshrc"
ln -s "$location/.zsh_aliases" "$HOME/.zsh_aliases"
ln -s "$location/.p10k.zsh" "$HOME/.p10k.zsh"

ln -s "$location/.gitconfig" "$HOME/.gitconfig"
ln -s "$location/.gitignore" "$HOME/.gitignore"

ln -s "$location/.doom.d/config.el" "$HOME/.doom.d/config.el"
ln -s "$location/.doom.d/init.el" "$HOME/.doom.d/init.el"
ln -s "$location/.doom.d/packages.el" "$HOME/.doom.d/packages.el"

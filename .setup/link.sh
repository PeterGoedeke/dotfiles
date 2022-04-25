#!/usr/bin/env bash

location="$HOME/dotfiles"

rm "$HOME/.zshrc"
rm "$HOME/.zsh_aliases"
rm "$HOME/.p10k.zsh"

rm "$HOME/.gitconfig"
rm "$HOME/.gitignore"

rm "$HOME/.doom.d/config.el"
rm "$HOME/.doom.d/init.el"
rm "$HOME/.doom.d/packages.el"


ln -s "$location/.zshrc" "$HOME/.zshrc"
ln -s "$location/.zsh_aliases" "$HOME/.zsh_aliases"
ln -s "$location/.p10k.zsh" "$HOME/.p10k.zsh"

ln -s "$location/.gitconfig" "$HOME/.gitconfig"
ln -s "$location/.gitignore" "$HOME/.gitignore"

ln -s "$location/.doom.d/config.el" "$HOME/.doom.d/config.el"
ln -s "$location/.doom.d/init.el" "$HOME/.doom.d/init.el"
ln -s "$location/.doom.d/packages.el" "$HOME/.doom.d/packages.el"

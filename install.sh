#!/usr/bin/env bash

location="$HOME/dotfiles"
config="$location/.config"

function install_packages {
  sudo pacman -Syu \
    # development
    alacritty
    emacs-nativecomp

    # cli tools
    fd
    fish
    flameshot
    fzf
    gnu-netcat
    hspell
    htop
    hunspell
    man-db
    net-tools
    openssh
    ripgrep
    speedtest-cli
    sudo
    tree
    unzip
    vim
    wget
    xclip
    zsa-wally

    # system utilities
    alsa-utils
    blueman
    bluez bluez-utils
    networkmanager
    pamixer
    pavucontrol
    pulseaudio pulseaudio-bluetooth
    nvidia nvidia-settings

    # window manager & utilities
    awesome
    autorandr
    dracula-gtk-theme
    gnome-themes-extra
    gtk-engines
    picom
    redshift
    xorg

    # fonts
    nerd-fonts-meslo
    noto-fonts noto-fonts-cjk noto-fonts-emoji
    ttf-fira-code
    ttf-roboto
    ttf-ubuntu-font-family

    # development tools
    base-devel
    bazel
    clang
    cmake
    docker
    docker-compose
    gcc
    git
    jre-openjdk
    make
    nodejs
    npm
    python
    rust-analyzer
    rustup

    # applications
    cheese
    discord
    eog
    evince
    firefox
    gedit
    gimp
    libreoffice-still
    nextcloud-client
    obs-studio
    pcmanfm
    spotify
    vlc
}

function install_yay {
  sudo git clone https://aur.archlinux.org/yay.git /opt/yay-git
  sudo chown -R $USER:$USER /opt/yay-git
  makepkg -sip /opt/yay-git PKGBUILD
}

function install_yay_packages {
  yay -Syu --answerdiff None --answerclean None --removemake \
    autojump \
    google-chrome \
    mongodb-compass \
    onlyoffice-bin \
    postman-bin \
    vcpkg
}

function setup_fisher {
  curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
  fisher install IlanCosman/tide@v5
  fisher install PatrickF1/fzf.fish
}

function install_doom {
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
  ~/.emacs.d/bin/doom install
}

function link_config_files {
  # link home directory config files

  ln -bs "$location/.gitconfig" "$HOME/.gitconfig"
  ln -bs "$location/.gitignore" "$HOME/.gitignore"
  ln -bs "$location/.editorconfig" "$HOME/.editorconfig"
  ln -bs "$location/.xprofile" "$HOME/.xprofile"

  # link ~/.config files

  mkdir -p $HOME/.config/alacritty
  mkdir -p $HOME/.config/awesome
  mkdir -p $HOME/.config/fish
  mkdir -p $HOME/.config/picom
  mkdir -p $HOME/.config/rofi

  ln -bs "$config/alacritty/alacritty.yml" "$HOME/.config/alacritty/alacritty.yml"
  ln -bs "$config/awesome/rc.lua" "$HOME/.config/awesome/rc.lua"
  ln -bs "$config/fish/config.fish" "$HOME/.config/fish/config.fish"
  ln -bs "$config/picom/picom.conf" "$HOME/.config/.picom/picom.conf"
  ln -bs "$config/rofi/config.rasi" "$HOME/.config/.rofi/config.rasi"

  # link doom files

  mkdir -p $HOME/.doom.d

  ln -bs "$location/.doom.d/config.el" "$HOME/.doom.d/config.el"
  ln -bs "$location/.doom.d/init.el" "$HOME/.doom.d/init.el"
  ln -bs "$location/.doom.d/packages.el" "$HOME/.doom.d/packages.el"
  ln -bs "$location/.doom.d/snippets" "$HOME/.doom.d/snippets"
}

install_packages
install_yay
install_yay_packages
setup_fisher
link_config_files
install_doom

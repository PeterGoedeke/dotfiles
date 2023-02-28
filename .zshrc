export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"

HYPHEN_INSENSITIVE="true"

# install custom zsh plugins
ZSH_AUTOSUGGESTIONS_DIR="$HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions"
if [[ ! -d "$ZSH_AUTOSUGGESTIONS_DIR" ]]; then
  git clone https://github.com/zsh-users/zsh-autosuggestions "$ZSH_AUTOSUGGESTIONS_DIR"
fi

ZSH_HISTORY_SEARCH_DIR="$HOME/.oh-my-zsh/custom/plugins/zsh-history-substring-search"
if [[ ! -d "$ZSH_HISTORY_SEARCH_DIR" ]]; then
  git clone https://github.com/zsh-users/zsh-history-substring-search "$ZSH_HISTORY_SEARCH_DIR"
fi

ZSH_SYNTAX_HIGHLIGHTING_DIR="$HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting"
if [[ ! -d "$ZSH_SYNTAX_HIGHLIGHTING_DIR" ]]; then
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$ZSH_SYNTAX_HIGHLIGHTING_DIR"
fi

# install nnn plugins
if [[ ! -d "$HOME/.config/nnn/plugins" ]]; then
  mkdir -p "$HOME/.config/nnn/plugins"
  sh -c \
    "$(curl -Ls https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs)"
fi

plugins=(
    autojump
    bazel
    colored-man-pages
    colorize
    copybuffer
    copyfile
    copypath
    docker
    dirhistory
    git
    zsh-autosuggestions
    zsh-history-substring-search
    zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh


function is_bin_in_path {
  if [[ -n $ZSH_VERSION ]]; then
    builtin whence -p "$1" &> /dev/null
  else  # bash:
    builtin type -P "$1" &> /dev/null
  fi
}

alias n="nnn"
alias vim="nvim"
alias v="nvim"
is_bin_in_path "bat" && alias cat="bat" || echo "bat is not available"

export NNN_OPTS="aAeHR"
# enable nnn plugins
export NNN_PLUG="i:preview-tui;r:gitroot;n:bulknew"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# fzf history completion from:
# https://github.com/junegunn/fzf/blob/master/shell/key-bindings.zsh
__fzfcmd() {
  [ -n "${TMUX_PANE-}" ] && { [ "${FZF_TMUX:-0}" != 0 ] || [ -n "${FZF_TMUX_OPTS-}" ]; } &&
    echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
}

# CTRL-R - Paste the selected command from history into the command line
fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
  selected=( $(fc -rl 1 | awk '{ cmd=$0; sub(/^[ \t]*[0-9]+\**[ \t]+/, "", cmd); if (!seen[cmd]++) print $0 }' |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} ${FZF_DEFAULT_OPTS-} -n2..,.. --scheme=history --bind=ctrl-r:toggle-sort,ctrl-z:ignore ${FZF_CTRL_R_OPTS-} --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
  local ret=$?
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle reset-prompt
  return $ret
}
zle     -N            fzf-history-widget
bindkey -M emacs '^R' fzf-history-widget
bindkey -M vicmd '^R' fzf-history-widget
bindkey -M viins '^R' fzf-history-widget

# device specific configuration (e.g. $JAVA_HOME) 
source ~/.zshlocal

export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# in your .bashrc/.zshrc/*rc
alias bathelp='bat --plain --language=help'
help() {
    "$@" --help 2>&1 | bathelp
}

# p10k prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# oh-my-zsh configuration
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"
HYPHEN_INSENSITIVE="true"
DISABLE_UPDATE_PROMPT="true"
ENABLE_CORRECTION="false"

# oh-my-zsh plugins
plugins=(
    autojump
    colored-man-pages
    colorize
    copybuffer
    copyfile
    copypath
    dirhistory
    git
    zsh-autosuggestions
    zsh-completions
    zsh-history-substring-search
    zsh-syntax-highlighting
)

# enable oh-my-zsh
autoload -U compinit && compinit
source $ZSH/oh-my-zsh.sh
source /etc/zsh_command_not_found

# enable fzf keybindings
source /usr/share/doc/fzf/examples/key-bindings.zsh

# allow history navigation with emacs keybindings
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

source $HOME/.zsh_aliases

# add doom scripts to the path
export PATH=~/.emacs.d/bin:$PATH

# set $JAVA_HOME for maven
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/


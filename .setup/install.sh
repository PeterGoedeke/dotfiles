if sudo apt update -y && sudo apt upgrade -y
then
    sudo apt-get install -y \
        git\
        zsh\
        python3
elif sudo yum update
then
    sudo yum -y install zsh
fi

mkdir .packages
git clone git://github.com/wting/autojump.git $HOME/.packages/autojump
cd $HOME/.packages/autojump
python3 install.py
cd $HOME

sed -i "s/\#\!\/usr\/bin\/env\ python/\#\!\/usr\/bin\/env\ python3/" ~/.autojump/bin/autojump

git clone --depth 1 https://github.com/junegunn/fzf.git $HOME/.packages/fzf
$HOME/.packages/fzf/install --all

wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh

git clone --bare https://github.com/PeterGoedeke/dotfiles $HOME/.cfg
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
config config --local status.showUntrackedFiles no
config checkout -f

git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-history-substring-search ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-history-substring-search
git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions

zsh
source $HOME/.zshrc

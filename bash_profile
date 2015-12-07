if [[ -a $HOME/homebrew/bin ]]; then
    export PATH=$HOME/homebrew/bin:$PATH
fi

if [[ -a $HOME/.local/bin ]]; then
    export PATH=$HOME/.local/bin:$PATH
fi

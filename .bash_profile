if [[ -a $HOME/.local/bin ]]; then
    export PATH=$HOME/.local/bin:$PATH
fi

if [[ -a $HOME/.multirust/bin ]]; then
    export PATH=$HOME/.multirust/bin:$HOME/.multirust/cargo/bin:$PATH
fi

if [[ -a /Applications/CoqIDE_8.4pl5.app/Contents/Resources/bin ]]; then
    export PATH=/Applications/CoqIDE_8.4pl5.app/Contents/Resources/bin:$PATH
fi

if [[ -a /usr/local/Cellar/openssl/1.0.2g ]]; then
    export OPENSSL_ROOT_DIR=/usr/local/Cellar/openssl/1.0.2g
    export OPENSSL_INCLUDE_DIR=$OPENSSL_ROOT_DIR/include
    export OPENSSL_LIB_DIR=$OPENSSL_ROOT_DIR/lib
fi

if [[ -a $HOME/gopath ]]; then
    export GOPATH=$HOME/gopath
    export PATH=$GOPATH/bin:$PATH
fi

if [[ -a /Applications/Postgres.app/Contents/Versions/9.4/bin ]]; then
    export PATH=/Applications/Postgres.app/Contents/Versions/9.4/bin:$PATH
fi

alias ls="ls -G"

if [[ -a $HOME/.local/bin ]]; then
    export PATH=$HOME/.local/bin:$PATH
fi

if [[ -a $HOME/.multirust/bin ]]; then
    export PATH=$HOME/.multirust/bin:$PATH
fi

if [[ -a /Applications/CoqIDE_8.4pl5.app/Contents/Resources/bin ]]; then
    export PATH=/Applications/CoqIDE_8.4pl5.app/Contents/Resources/bin:$PATH
fi

if [[ -a $HOME/.nix-profile/ ]]; then
    export OPENSSL_INCLUDE_DIR=$HOME/.nix-profile/include
    export OPENSSL_LIB_DIR=$HOME/.nix-profile/lib
    export OPENSSL_ROOT_DIR=$HOME/.nix-profile
fi

if [[ -a $HOME/gopath ]]; then
    export GOPATH=$HOME/gopath
    export PATH=$GOPATH/bin:$PATH
fi

alias ls="ls -G"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi


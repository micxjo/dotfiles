#!/bin/bash

HOMEBREW_PATH=$HOME/homebrew

if [[ ! -a $HOMEBREW_PATH ]]; then
    git clone "https://github.com/Homebrew/homebrew.git" $HOMEBREW_PATH
fi

export PATH=$HOMEBREW_PATH/bin:$PATH

brew install emacs --with-cocoa
brew install wget openssl unrar rlwrap ack haskell-stack elixir
brew install sloccount cloc icu4c htop-osx

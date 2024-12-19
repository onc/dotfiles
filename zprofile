# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:

export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8

export EDITOR="vim"
export CLICOLOR=1
export GPG_TTY=$(tty)

if [[ $(uname) == "Darwin" ]]; then
    export OS_TYPE="macOS" 
elif command -v apt > /dev/null; then
    export OS_TYPE="ubuntu" 
else
    export OS_TYPE="" 
fi

# Homebrew. Must go first so something like pyenv can be found.
if [ OS_TYPE="macOS" ]; then
    if [ -f /opt/homebrew/bin/brew ]; then
        export HOMEBREW_NO_ANALYTICS=1
        export HOMEBREW_AUTO_UPDATE_SECS=604800 # 1 week
        export HOMEBREW_NO_ENV_HINTS=1

        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
fi

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Java / Spark
if [ OS_TYPE="macOS" ]; then
    export SPARK_LOCAL_HOSTNAME="localhost"
    export JAVA_HOME="/opt/homebrew/opt/openjdk@17"
    [[ -d $JAVA_HOME/bin ]] && export PATH="$JAVA_HOME/bin:$PATH"
    export JAVA_TOOL_OPTIONS="-Djavax.net.ssl.trustStoreType=KeychainStore"
fi

# kubernetes
# export KUBECONFIG="$HOME/.kube/config"

# rancher
if [ OS_TYPE="macOS" ]; then
    [[ -d $HOME/.rd/bin ]] && export PATH="$HOME/.rd/bin:$PATH"
fi

if [ OS_TYPE="macOS" ]; then
    if [ -d $HOME/.ssl ]; then
        # export SSL_CERT_FILE="$HOME/.ssl/allCAbundle.pem"
        # export REQUESTS_CA_BUNDLE="$SSL_CERT_FILE"
        export SSL_CERT_FILE=""
        export REQUESTS_CA_BUNDLE=""
    fi
elif [ OS_TYPE="ubuntu" ]; then
    if [ -f /etc/ssl/certs/ca-certificates.crt ]; then
        export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
    fi
fi

export KUBECONFIG="$HOME/.kube/config"

# Ruby
# export GEM_HOME=$(ruby -e 'print Gem.user_dir')
# export PATH="`ruby -e 'print Gem.user_dir'`/bin:${PATH}"
#
# if which rbenv > /dev/null; then
#     eval "$(rbenv init -)";
# fi

# Nodejs
# export NPM_PACKAGES="${HOME}/.npm"
# export PATH="${NPM_PACKAGES}/bin:${PATH}"

# rust/cargo
# export PATH="${HOME}/.cargo/bin:${PATH}"
# export RUST_SRC_PATH="${HOME}/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"

# ghc
# export PATH="${HOME}/.cabal/bin:${PATH}"

# go
# export GOPATH="${HOME}/Repos/go"
# export GOROOT="/usr/local/opt/go/libexec"
# export PATH=$PATH:$GOPATH/bin
# export PATH=$PATH:$GOROOT/bin

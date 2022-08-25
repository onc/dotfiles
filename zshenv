export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='vim'
fi

# Export paths
export PATH="${HOME}/Applications:${PATH}"
export PATH="/usr/local/sbin:$PATH"

# Python
export PATH="$(python3 -m site --user-base)/bin:${PATH}"

# Java
# export JAVA_HOME=$(/usr/libexec/java_home)

# Ruby
export GEM_HOME=$(ruby -e 'print Gem.user_dir')
export PATH="`ruby -e 'print Gem.user_dir'`/bin:${PATH}"

# Nodejs
export NPM_PACKAGES="${HOME}/.npm"
export PATH="${NPM_PACKAGES}/bin:${PATH}"

# rust/cargo
# export PATH="${HOME}/.cargo/bin:${PATH}"
# export RUST_SRC_PATH="${HOME}/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"

# ghc
export PATH="${HOME}/.cabal/bin:${PATH}"

# go
# export GOPATH="${HOME}/Repos/go"
# export GOROOT="/usr/local/opt/go/libexec"
# export PATH=$PATH:$GOPATH/bin
# export PATH=$PATH:$GOROOT/bin

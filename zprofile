# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:
#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Browser
#

if [[ -z "$BROWSER" && "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

if [[ -z "$EDITOR" ]]; then
  export EDITOR='vim'
fi
if [[ -z "$VISUAL" ]]; then
  export VISUAL='vim'
fi
if [[ -z "$PAGER" ]]; then
  export PAGER='less'
fi

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  $HOME/{,s}bin(N)
  /opt/{homebrew,local}/{,s}bin(N)
  /usr/local/{,s}bin(N)
  $path
)

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X to enable it.
if [[ -z "$LESS" ]]; then
  export LESS='-g -i -M -R -S -w -z-4'
fi

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if [[ -z "$LESSOPEN" ]] && (( $#commands[(i)lesspipe(|.sh)] )); then
  # export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
  export LESSOPEN='| ~/.lessfilter %s 2>&-'
fi

#
# Export certificate bundle file
#

if [ -f /etc/ssl/certs/ca-certificates.crt ]; then
    export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
fi

#
# Java
#

if [[ -d $(brew --prefix)/opt/openjdk@17 ]]; then
    export JAVA_HOME=$(brew --prefix)/opt/openjdk@17
    if [[ "$OSTYPE" == darwin* ]]; then
        export JAVA_TOOL_OPTIONS="-Djavax.net.ssl.trustStoreType=KeychainStore"
        export SPARK_LOCAL_HOSTNAME="localhost"
    fi
elif [[ -d /usr/lib/jvm/java-11-openjdk-amd64 ]]; then
    export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
fi

export PATH="$JAVA_HOME/bin:$PATH"

#
# Liquibase
#

if command -v liquibase > /dev/null; then
    [[ "$OSTYPE" == darwin* ]] && export LIQUIBASE_HOME=$(brew --prefix)/opt/liquibase/libexec
    export JAVA_OPTS=--add-opens=java.base/java.nio=ALL-UNNAMED
fi

#
# Rancher
#

if [[ -d $HOME/.rd/bin ]]; then
    export PATH="$HOME/.rd/bin:$PATH"
    # presence of docker.sock indicates that rancher is running. Otherwise, the rdctl shell commands below, will not
    # work and through errors.
    if [[ -e $HOME/.rd/docker.sock ]]; then
        # see: https://testcontainers-python.readthedocs.io/en/latest/#configuration
        # https://docs.rancherdesktop.io/how-to-guides/using-testcontainers/
        visualization_type=$(rdctl list-settings | jq -r '.experimental.virtualMachine.type')
        if [[ $visualization_type == vz ]]; then
            export DOCKER_HOST=unix://$HOME/.rd/docker.sock
            export TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE=/var/run/docker.sock
            export TESTCONTAINERS_HOST_OVERRIDE=$(rdctl shell ip a show vznat | awk '/inet / {sub("/.*",""); print $2}')
        elif [[ $visualization_type == qemu ]]; then
            export TESTCONTAINERS_HOST_OVERRIDE=$(rdctl shell ip a show rd0 | awk '/inet / {sub("/.*",""); print $2}')
        fi
    fi
fi

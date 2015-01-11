#!/bin/bash

executable_p ()
{
    do_echo=false
    executable=$1
    if [ "$1" = -v ] && [ -n "$2" ]; then
        executable=$2
        do_echo=true
    fi

    if ! which "$executable" >/dev/null 2>&1; then
        if [ "$do_echo" = true ]; then
            echo executable not found: $executable
        fi
        return 1
    fi
}

usage ()
{
    echo "usage: ./autogen.sh [--install-deps]"
    if [ $# -gt 0 ]; then
        exit $1
    fi
}

DEPS=$(cat <<EOF
gcc
g++
make
automake
autoconf
libpng-dev
libz-dev
libpoppler-glib-dev
EOF
    )

MAYBE_DEPS=libpoppler-private-dev

INSTALL_DEPS=false

for arg; do
    case $arg in
        --install-deps) INSTALL_DEPS=true;;
        *) usage 1;
    esac
done

if [ "$INSTALL_DEPS" = true ]; then
    if ! { executable_p -v aptitude &&  executable_p -v sudo; }; then
        echo "Don't know how to install packages on this system."
        echo "Packages required:"
        for d in $DEPS; do echo $d; done
        echo
        echo "Packages which maybe required:"
        for d in $MAYBE_DEPS; do echo $d; done
        echo
    else
        echo "Installing dependencies..."
        for d in $MAYBE_DEPS; do
            if aptitude show $d >/dev/null 2>&1; then
                DEPS="$DEPS $d"
            fi
        done
        sudo aptitude install $DEPS
    fi
fi

executable_p -v autoreconf || exit 0

echo "Running autoreconf..."

autoreconf -i

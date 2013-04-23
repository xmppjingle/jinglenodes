#!/bin/bash
#
# Authors: Jorge Espada <jespada@yuilop.com> & Manuel Rubio <manuel@yuilop.com>
#
# You need to have installed rubygems, fpm[0] gem (gem install fpm) and build-essential
# [0] https://github.com/jordansissel/fpm/wiki

USER=jnode
GROUP=jnode
PROJECT=jinglenodes

INSTDIR=$(pwd)/installdir
TAG=$(git describe --always --tag)

if [ ! -z "$1" ]; then
    TAG="$1"
fi

#clean compile and make the package
rm -rf deps/*
rm -rf apps/*/logs
rm -rf apps/*/.eunit
rm -rf apps/*/doc
rebar clean get-deps compile && rebar doc eunit skip_deps=true && rebar generate

if [[ $? -ne 0 ]]; then
    echo "Please check dependencies, compilation went wrong"
    exit 1
fi

rm -rf $INSTDIR
mkdir -p $INSTDIR/$PROJECT
cp -a rel/$PROJECT $INSTDIR/

#build the package
pushd $INSTDIR
tar -c --owner=$USER --group=$GROUP -j -f $PROJECT-$TAG.tar.bz2 $PROJECT


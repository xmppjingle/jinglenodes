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
FPM=$(gem which fpm | sed 's/\/lib\/fpm.rb/\/bin\/fpm/g')
TAG=$(git describe --always --tag)

if [ ! -z "$1" ]; then
    TAG="$1"
fi

#check if gem and fpm are installed
echo "You must have rubygems, fpm, and build-essential installed..."

gem list --local | grep fpm

if [[ $? -ne 0 ]]; then
    echo "Please verify the output of: gem list --local | grep fpm , remember you need tubygems and fpm installed"
    exit 1
fi

#clean compile and make the package
rm -rf deps/*
rm -rf apps/*/logs
rm -rf apps/test/*.beam
rebar clean get-deps compile generate

if [[ $? -ne 0 ]]; then
    echo "Please check dependencies, compelation went wrong"
    exit 1
fi

rm -rf $INSTDIR
mkdir -p $INSTDIR/$PROJECT
cp -a rel/$PROJECT/* $INSTDIR/$PROJECT

#build the package
pushd $INSTDIR
$FPM -s dir -t deb -n $PROJECT -v $TAG -C $INSTDIR --description "JingleNodes Erlang Server" -p jinglenodes-VERSION_ARCH.deb --config-files /opt/$PROJECT/etc/app.config --prefix /opt --deb-user $USER --deb-group $GROUP --url http://www.yuilop.com/ --vendor Yuilop --maintainer '"Manuel Rubio" <manuel@yuilop.com>' $PROJECT


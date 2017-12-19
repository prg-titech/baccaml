#!/bin/bash

eval "$(opam config env)"

for OPT in "$@"
do
  case "$OPT" in
    '-a' | '--all' )
      make test
      exit 0
      ;;
    '-p' | '--pypy' )
      make pypytest
      exit 0
      ;;
    '-s' | '--simple' )
      make simple1test
      exit 0
      ;; 
    * )
      if [[ ! -z "$1" ]] && [[ ! "$1" =~ ^-+ ]]; then
        param+=( "$1" )
        shift 1
      fi
      ;;
  esac
done

if [ -z "$param" ]; then
  rm -rf /tmp/_build && mkdir /tmp/_build
  make test
fi

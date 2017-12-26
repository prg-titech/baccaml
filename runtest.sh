#!/bin/bash

eval "$(opam config env)"

usage () {
    echo "USAGE: ./runtest.sh"
    echo "options: "
    echo "       -h: show help message"
    echo "       -a: run all test"
    exit 0
}

for OPT in "$@"
do
  case "$OPT" in
    '-a' | '--all' )
      make test
      exit 0
      ;;
    '-h' | '--help' )
      usage
      ;;
    * )
      if [[ ! -z "$1" ]] && [[ ! "$1" =~ ^-+ ]]; then
        param+=( "$1" )
        shift 1
      fi
      ;;
  esac
done

if [ -z "${param}" ]; then
  rm -rf /tmp/_build && mkdir /tmp/_build
  make test
fi

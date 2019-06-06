#!/bin/bash

deps=(dune core menhir oUnit tuareg merlin)

opam update

opam install -y ${deps[@]}

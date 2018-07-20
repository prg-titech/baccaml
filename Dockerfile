FROM ocaml/opam:ubuntu

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /tmp

COPY Makefile .

RUN opam init && \
    make setup && \
    eval $(opam config env)

FROM ocaml/opam:alpine

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /tmp

COPY Makefile .

RUN touch ~/.profile && sudo apk update && \
    sudo apk add m4 pcre-dev make patch libc-dev gcc && \
    opam init -a && \
    make setup && \
    eval $(opam config env)

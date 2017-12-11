FROM ocaml/opam:ubuntu

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /tmp

RUN opam init && \
    opam install -y ounit

COPY . .

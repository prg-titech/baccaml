FROM ocaml/opam:ubuntu

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /tmp

RUN opam init && \
    opam install -y ounit core && \
    eval $(opam config env)

COPY . .

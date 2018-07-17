FROM ocaml/opam:ubuntu

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /tmp

RUN opam init && \
    opam install -y \
    ounit \
    core \
    ppx_deriving \
    fmt \
    logs \
    menhir \
    sequence \
    jbuilder && \
    eval $(opam config env)

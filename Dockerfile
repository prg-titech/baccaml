FROM ocaml/opam:ubuntu

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /tmp

RUN opam init && \
    opam install -y \
    ounit \
    core \
    ppx_deriving \
    ppx_fields_conv \
    fmt \
    logs \
    menhir \
    sequence \
    jbuilder && \
    eval $(opam config env)

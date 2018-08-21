FROM ocaml/opam2

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /tmp

RUN sudo apt-get install -y m4
RUN opam init
RUN opam update
RUN opam install -y core menhir oUnit ppx_deriving logs fmt stringext dune
RUN eval $(opam env)

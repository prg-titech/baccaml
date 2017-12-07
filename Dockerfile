FROM ubuntu:latest

MAINTAINER Yusuke Izawa <yuizalp@gmail.com>

WORKDIR /app

RUN apt-get -y update && \
    apt-get -y install \
    opam \
    m4

RUN opam init && \
    opam install -y ounit

COPY . .

FROM ubuntu:16.04

RUN dpkg --add-architecture i386
RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install gcc:i386 libc6-dev:i386

RUN apt-get -y install make wget vim
RUN apt-get -y install libncurses5-dev:i386

# Install 32 bit gforth (working around missing dependency emacsen-commen:i386)
RUN apt-get -y install libffcall1:i386 libltdl7:i386 gforth-common:i386 gforth-lib:i386
RUN wget http://ftp.de.debian.org/debian/pool/main/g/gforth/gforth_0.7.2+dfsg1-1.1_i386.deb
#RUN apt-get -y download gforth:i386
RUN dpkg --force-all -i gforth_0.7.2+dfsg1-1.1_i386.deb

ADD . /usr/local/flk
WORKDIR /usr/local/flk

RUN make depend
RUN make clean

ENV TERM=vt100
RUN make FORTH32="gforth -e"
RUN make FORTH32="gforth -e" install

VOLUME /usr/local/flk

CMD /usr/local/bin/flk

FROM ubuntu:18.04
RUN apt-get -q update
RUN apt-get -qqy install nasm perl perl-modules curl sudo less
RUN apt-get -qqy install gdb
RUN apt-get -qqy install libdevel-nytprof-perl
RUN yes | cpan install POD::Markdown
RUN curl https://raw.githubusercontent.com/spencertipping/ni/develop/ni \
       > /usr/bin/ni \
 && chmod 755 /usr/bin/ni

VOLUME /phi
WORKDIR /phi

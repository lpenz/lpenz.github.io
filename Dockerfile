FROM debian:buster
MAINTAINER Leandro Lisboa Penz <lpenz@lpenz.org>

# install debian packages:
ENV DEBIAN_FRONTEND noninteractive
RUN set -e -x; \
    apt-get update; \
    apt-get install -y --no-install-recommends locales \
        graphviz txt2tags \
        haskell-platform \
        r-cran-ggplot2 r-cran-reshape \
        inkscape \
        python3-pip python3-setuptools python3-wheel \
        python3-mako python3-yaml python3-nose flake8 \
        linkchecker \
        scons python-yaml \
        git

# setup sudo and locales
RUN set -e -x; \
    echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen; locale-gen; \
    sed -i '/drop_privileges/d' /usr/bin/linkchecker; \
    sed -i '/pam_rootok.so$/aauth sufficient pam_permit.so' /etc/pam.d/su
ENV LC_ALL=en_US.UTF-8 \
    HOME=/tmp

CMD ./tests

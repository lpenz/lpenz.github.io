FROM debian:buster
MAINTAINER Leandro Lisboa Penz <lpenz@lpenz.org>

# install debian packages:
ENV DEBIAN_FRONTEND noninteractive
RUN set -e -x; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        graphviz txt2tags \
        haskell-platform \
        r-cran-ggplot2 r-cran-reshape \
        inkscape \
        python-pip python-setuptools python-wheel \
        python-mako python-yaml python-nose flake8 \
        linkchecker \
        git scons

# setup sudo and locales
RUN set -e -x; \
    locale-gen en_US.UTF-8 || true; \
    sed -i '/drop_privileges/d' /usr/bin/linkchecker; \
    sed -i '/pam_rootok.so$/aauth sufficient pam_permit.so' /etc/pam.d/su

# install pip packages:
RUN set -e -x; \
    pip install \
        py3kwarn==0.4.4

CMD ./tests

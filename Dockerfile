FROM ubuntu:16.04
MAINTAINER Leandro Lisboa Penz <lpenz@lpenz.org>

# install debian packages:
ENV DEBIAN_FRONTEND noninteractive
RUN set -e -x; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        graphviz txt2tags \
        haskell-platform \
        r-cran-ggplot2 r-cran-reshape \
        python-pip python-setuptools python-wheel \
        python-mako python-yaml python-nose flake8 \
        linkchecker \
        git scons \
        gosu sudo

# setup sudo and locales
RUN set -e -x; \
    locale-gen en_US.UTF-8 || true; \
    sed -i '/drop_privileges/d' /usr/bin/linkchecker; \
    echo 'ALL ALL=NOPASSWD:ALL' > /etc/sudoers.d/all; \
    chmod 0400 /etc/sudoers.d/all

# install pip packages:
RUN set -e -x; \
    pip install \
        py3kwarn==0.4.4

# setup entrypoint with user UID/GID from host
RUN set -x -e; \
    (\
    echo '#!/bin/bash'; \
    echo 'MY_UID=${MY_UID:-1000}'; \
    echo 'set -x -e'; \
    echo 'useradd -M -u "$MY_UID" -o user'; \
    echo 'chown user:user /home/user'; \
    echo 'cd /home/user/work'; \
    echo 'exec gosu user "${@:-/bin/bash}"'; \
    ) > /usr/local/bin/entrypoint.sh; \
    chmod a+x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

# If your UID is 1000, you can simply run the container as
# otherwise, run it as:
# docker run -it --rm -v $PWD:/home/user/work -e MY_UID=$UID lpenz.github.io

CMD ./tests

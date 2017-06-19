FROM ubuntu:16.04

MAINTAINER agatan

RUN apt-get update -qq -y && apt install wget curl gcc make zlib1g-dev libgc-dev -qq -y && \
    wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
    echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-3.9 main" >> /etc/apt/sources.list && \
    echo "deb-src http://apt.llvm.org/xenial/ llvm-toolchain-xenial-3.9 main" >> /etc/apt/sources.list && \
    apt-get update -y && \
    apt-get install -y llvm-3.9-dev && \
    ln -s /usr/bin/llvm-config-3.9 /usr/bin/llvm-config && \
    ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/lib/x86_64-linux-gnu/libstdc++.so && \
    rm -rf /var/lib/apt/lists/*

RUN curl https://sh.rustup.rs -sSf > /tmp/rustup.sh && \
    chmod +x /tmp/rustup.sh && \
    /tmp/rustup.sh -v -y --default-toolchain nightly && \
    rm /tmp/rustup.sh && \
    rm -rf /root/.rustup/downloads /root/.rustup/tmp
ENV PATH /root/.cargo/bin:$PATH

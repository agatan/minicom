FROM ubuntu:16.04

MAINTAINER agatan

# Setup
RUN apt-get update -qq -y && \
    apt-get install -qq -y git wget curl unzip \
                           g++ aspcud  cmake python m4 pkg-config libffi-dev && \
    apt-get clean -qq -y && \
    rm -rf /var/lib/apt/lists/*

# Download LLVM 3.9.1
# https://llvm.org/svn/llvm-project/llvm/tags/RELEASE_391/final/CMakeLists.txt
RUN wget -q http://www.llvm.org/releases/3.9.1/llvm-3.9.1.src.tar.xz -O - | tar -xJ
RUN cd llvm-3.9.1.src && \
    mkdir build && \
    cd build && \
    cmake -G 'Unix Makefiles' \
          -DCMAKE_INSTALL_PREFIX=/usr/local \
          -DCMAKE_BUILD_TYPE=Release \
          -DLLVM_TARGETS_TO_BUILD="X86" \
          -LLVM_TARGETS_WITH_JIT="X86" \
          -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="WebAssembly" \
          -LLVM_ENABLE_TERMINFO=OFF \
          -LLVM_ENABLE_ZLIB=OFF \
          -LLVM_INSTALL_TOOLCHAIN_ONLY=ON \
          -DLLVM_BUILD_TOOLS=OFF \
          -DLLVM_INCLUDE_UTILS=OFF \
          -LLVM_BUILD_UTILS=OFF \
          -DLLVM_INCLUDE_EXAMPLES=OFF \
          -DLLVM_INCLUDE_TESTS=OFF \
          -DLLVM_INCLUDE_GO_TESTS=OFF \
          -LLVM_INCLUDE_DOCS=OFF \
          -LLVM_BUILD_DOCS=OFF \
          ..

# Build LLVM for static lib
RUN cd llvm-3.9.1.src/build && \
    cmake --build . --target package/fast -- -j4 && \
    cmake --build . --target llvm-config/fast -- -j4
RUN cd llvm-3.9.1.src/build && \
    cmake --build . --target install && \
    cp bin/llvm-config /usr/local/bin/.

# Clean up LLVM src
RUN rm -rf llvm-3.9.1.src

RUN curl https://sh.rustup.rs -sSf > /tmp/rustup.sh && \
    chmod +x /tmp/rustup.sh && /tmp/rustup.sh -v -y && \
    rm /tmp/rustup.sh
ENV PATH /root/.cargo/bin:$PATH
RUN rustup install nightly

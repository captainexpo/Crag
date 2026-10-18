# Building
FROM fedora:43 AS build

RUN dnf install -y --setopt=install_weak_deps=False \
        gcc gcc-c++ cmake make binutils llvm llvm-devel \
    && dnf clean all

WORKDIR /src
COPY . .

RUN cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
    && cmake --build build -j"$(nproc)"

# Running
FROM fedora:43

# c toolchain is needed
RUN dnf install -y --setopt=install_weak_deps=False \
        gcc glibc-devel binutils llvm-libs \
    && dnf clean all

COPY --from=build /src/build/crag /opt/crag/crag
COPY --from=build /src/build/lib/libruntime.a /opt/crag/lib/libruntime.a
COPY --from=build /src/stdlib /usr/local/lib/crag/stdlib
RUN ln -s /opt/crag/crag /usr/local/bin/crag

WORKDIR /work
ENTRYPOINT ["crag"]
CMD ["--help"]

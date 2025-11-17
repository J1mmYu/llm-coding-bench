FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 python3-pip build-essential git curl ca-certificates \
    gfortran gnucobol gnat \
    swi-prolog \
    octave \
    && rm -rf /var/lib/apt/lists/*

# 安装 Python 依赖（目前只用标准库，如需加依赖可在此处 pip 安装）
WORKDIR /work

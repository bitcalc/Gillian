FROM node:latest

LABEL maintaner "Sacha \"Giltho\" Ayoun"

ARG DEBIAN_FRONTEND=noninteractive

ENV LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib

RUN apt-get update && apt-get install -y apt-utils

RUN apt-get install -y \
  build-essential \
  curl \
  git \
  zsh \
  m4

RUN npm install -g esy@0.6.6 --unsafe-perm

RUN mkdir /app

WORKDIR /app

RUN git clone https://github.com/GillianPlatform/javert-test262.git test262

RUN git clone https://github.com/GillianPlatform/collections-c-for-gillian.git collections-c

RUN git clone https://github.com/GillianPlatform/Gillian.git

WORKDIR /app/Gillian

RUN esy

CMD [ "zsh" ]
FROM openjdk:8

ENV SBT_VERSION 1.2.7
ENV PROTO_NUMBER 3.6.1
ENV PROTO_VERSION 3.6.1-linux-x86_64

## Install Graphviz
RUN apt-get update && apt-get install -y graphviz

# Install sbt
RUN curl -L -o sbt-$SBT_VERSION.deb http://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
dpkg -i sbt-$SBT_VERSION.deb && rm sbt-$SBT_VERSION.deb && apt-get update && apt-get install sbt && sbt sbtVersion

# Install ProtoBuffer
RUN wget https://github.com/protocolbuffers/protobuf/releases/download/v$PROTO_NUMBER/protoc-$PROTO_VERSION.zip \
&& unzip protoc-3.6.1-linux-x86_64.zip -d protoc/ && cp protoc/bin/protoc /usr/local/bin \
&& cp -r protoc/include/google /usr/local/include

# Define working directory
ADD . /app

WORKDIR /app

# Build sbt fat jar
RUN sbt assembly

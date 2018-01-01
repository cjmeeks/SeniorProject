FROM ubuntu:16.04

EXPOSE 8000

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq5 libgmp10 ca-certificates netbase && \
    apt-get clean

RUN mkdir server
RUN mkdir client
RUN mkdir db
COPY ./server/bin /server/
COPY client/dist/ /client/
COPY db/migrate/ /db/
ENTRYPOINT ["./server"]
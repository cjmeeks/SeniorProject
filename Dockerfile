FROM ubuntu:16.04

EXPOSE 8000

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq5 libgmp10 ca-certificates netbase && \
    apt-get clean
WORKDIR /server
COPY server/bin /server/
COPY client/dist/ /client/dist
COPY db/migrate/ /db/
ENTRYPOINT ["/server/app.exe"]
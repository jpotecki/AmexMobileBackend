FROM debian:jessie

RUN apt-get update && apt-get install -y libcurl4-gnutls-dev libgmp-dev

COPY /home/travis/build/jpotecki/AmexMobileBackend/dist/AmexMobileBackend-exe /usr/local/bin/AmexMobileBackend

EXPOSE 8080

ENTRYPOINT ["/usr/local/bin/AmexMobileBackend"]
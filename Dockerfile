FROM haskell:8

COPY . /opt/AmexMobileBackend

WORKDIR /opt/AmexMobileBackend

EXPOSE 8080

RUN stack setup

RUN stack build

CMD ["stack","exec","AmexMobileBackend-exe"]
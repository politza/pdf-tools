ADD . /epdfinfo
WORKDIR /epdfinfo
RUN make -s distclean || true
CMD ["sh", "./test/docker/lib/run-tests"]

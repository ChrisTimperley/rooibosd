FROM squareslab/rooibos:latest

RUN rm -rf /tmp/*

# install rvt's opium fork
ENV OPIUM_REV 600e0b1cf7ef79d81b0b9836e45ba736a24f6e2b
RUN git clone https://github.com/rvantonder/opium /tmp/opium \
 && cd /tmp/opium \
 && git checkout ${OPIUM_REV} \
 && opam pin add opium . \
 && rm -rf /tmp/opium

WORKDIR /tmp/rooibosd
COPY rooibosd.opam /tmp/rooibosd/
COPY Makefile /tmp/rooibosd/
COPY src /tmp/rooibosd/

RUN sudo chown -R $(whoami) . \
 && eval $(opam config env) \
 && make \
 && make install \
 && sudo ln -s "$(which rooibosd)" /usr/bin/rooibosd \
 && rm -rf /tmp/rooibosd

ENTRYPOINT ["/usr/bin/rooibosd"]
CMD ["-p", "8888"]

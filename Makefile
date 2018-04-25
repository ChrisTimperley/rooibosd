all:
	jbuilder build @install @DEFAULT

install:
	jbuilder install

test:
	jbuilder runtest

clean:
	jbuilder clean

uninstall:
	jbuilder uninstall

docker-build:
	docker build -t squareslab/rooibosd .

docker-push:
	docker push squareslab/rooibosd

.PHONY: all install test clean uninstall docker-build docker-push

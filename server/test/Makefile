# +-----------------------------------------------------------+
# * Docker tests for the autobuild script
# +-----------------------------------------------------------+

# List of available OS.
DOCKER_OS = $(patsubst %.Dockerfile.in, %, \
		$(notdir $(wildcard docker/templates/*.Dockerfile.in)))

# Arguments to pass to docker build .
DOCKER_BUILD_ARGS = -q

# Advice make not to delete these "intermediate" files.
.PRECIOUS: docker/%.Dockerfile docker/.%.build

.PHONY: all test check docker/build

all: docker/test
test: docker/test
check: docker/test

# Create the Dockerfile
docker/%.Dockerfile: docker/templates/%.Dockerfile.in \
		     docker/templates/Dockerfile.in
	@echo Creating Dockerfile for target $*
	cat $^ > $@

# Build the Dockerfile
docker/.%.build: docker/%.Dockerfile ../autobuild docker/lib
	@echo Building target $*
	docker build $(DOCKER_BUILD_ARGS) -t epdfinfo/$* -f $< ../
	touch $@

# Run the Dockerfile
docker/%: docker/.%.build
	@echo Running tests on target $*
	docker run epdfinfo/$*

# Run all Dockerfiles
docker/test: docker/build $(patsubst %, docker/%, $(DOCKER_OS))

# Build all Dockerfiles
docker/build: $(patsubst %, docker/.%.build, $(DOCKER_OS))

clean:
	rm -f -- docker/.[^.]*.build
	rm -f -- docker/*.Dockerfile

print:
	@for os in $(DOCKER_OS); do echo $$os; done | sort

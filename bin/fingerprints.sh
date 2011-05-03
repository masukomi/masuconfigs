#!/bin/sh

# outputs the fingerprints for all your public ssh keys

find ~/.ssh/*.pub \
	-exec basename '{}' \; \
	-exec ssh-keygen -l -E sha256 -f '{}' \;

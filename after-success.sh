#!/bin/bash
set -euo pipefail
IFS=$'\n\t'


docker build -t amexbackend .

# just push it as latest
docker tag seat-travis-test janosp/amexbackend 
docker push janosp/amexbackend

# If this is not a pull request, update the branch's docker tag.
#if [ $TRAVIS_PULL_REQUEST = 'false' ]; then
#  docker tag amexbackend janosp/amexbackend:${TRAVIS_BRANCH/\//-} && docker push janosp/amexbackend:${TRAVIS_BRANCH/\//-};
#  # If this commit has a tag, use on the registry too.
#  if ! test -z $TRAVIS_TAG; then
#    docker tag amexbackend janosp/amexbackend:${TRAVIS_TAG} && docker push janosp/amexbackend:${TRAVIS_TAG};
#  fi
#fi
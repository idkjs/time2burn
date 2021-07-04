#! /usr/bin/env bash

set -euo pipefail

### Variables

set +u
if [ -z "$TIME2BURN_CREDENTIALS" ]; then
  echo 'The TIME2BURN_CREDENTIALS environment variable is missing from your .bashrc or .bash_profile. It should contain the path to your service account key file.'
  exit 1
fi
set -u

if ! ls "$TIME2BURN_CREDENTIALS" &> /dev/null; then
  echo "Could not find your GCloud credentials file. Check your TIME2BURN_CREDENTIALS variable: $TIME2BURN_CREDENTIALS"
  return 1
fi

export GOOGLE_APPLICATION_CREDENTIALS="$TIME2BURN_CREDENTIALS"
export GCLOUD_PROJECT="$TIME2BURN_PROJECT"

if which greadlink &> /dev/null; then
  export PROJECT_DIR="$(dirname "${BASH_SOURCE[0]}" | xargs greadlink -f)"
else
  export PROJECT_DIR="$(dirname "${BASH_SOURCE[0]}" | xargs readlink -f)"
fi

## Check if there uncommitted work
if ! (git diff --quiet && git diff --staged --quiet); then
  echo 'Please commit your work in progress before running this script.'
  exit 1
fi

### Authenticate

gcloud auth activate-service-account --key-file="$GOOGLE_APPLICATION_CREDENTIALS"
gcloud config set project "$GCLOUD_PROJECT"
gcloud config set run/region us-central1

### Build and upload

URL="us.gcr.io/$GCLOUD_PROJECT/time2burn"
SHA="$(git rev-list -1 HEAD -- .)"
IMAGE="$URL:$SHA"
docker build . -t "$IMAGE"
docker push "$IMAGE"

### Deploy

gcloud run deploy time2burn --platform=managed --image="$IMAGE"

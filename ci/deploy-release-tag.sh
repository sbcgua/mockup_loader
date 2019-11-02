#!/bin/bash
echo "Checking environment and targets ..."

if [ "$TRAVIS_BRANCH" != "master" ]; then
    echo "Deployment for master only"
    exit 1
fi
if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
    echo "Deployment disabled for pull requests"
    exit 1
fi

echo "Detecting version change ..."

VERSION_FILE=$DEPLOY_VERSION_FILE
VERSION_CONSTANT=$DEPLOY_VERSION_CONST

if [ -z $VERSION_FILE ] || [ -z $VERSION_CONSTANT ]; then
    echo "Version file or constant were not specified"
    echo "  file:" $VERSION_FILE
    echo "  const:" $VERSION_CONSTANT
    echo "Usage: deploy.sh <version_file_path> <version_constant>"
    exit 1
fi

# this does not work on merge, maybe use HEAD HEAD^1 ... maybe just change versions in master directly !
# git diff-tree --no-commit-id --name-only -r HEAD | grep $VERSION_FILE > /dev/null 2>&1
git diff-tree --no-commit-id --name-only -r HEAD | grep $VERSION_FILE
if [ $? -ne 0 ]; then
    echo "No version change detected, skipping tag creation"
    exit 0
fi

VERSION_DIFF=$(git diff HEAD^:$VERSION_FILE HEAD:$VERSION_FILE)

# echo "$VERSION_DIFF" | grep $VERSION_CONSTANT > /dev/null 2>&1
echo "$VERSION_DIFF" | grep $VERSION_CONSTANT
if [ $? -ne 0 ]; then
    echo "No version change detected, skipping tag creation"
    exit 0
fi

VERSION_BEFORE=$(echo "$VERSION_DIFF" | grep "^-.\+\b$VERSION_CONSTANT\b" | grep -E -o "[0-9]+\.[0-9]+\.[0-9]+")
VERSION_AFTER=$(echo "$VERSION_DIFF" | grep "^+.\+\b$VERSION_CONSTANT\b" | grep -E -o "[0-9]+\.[0-9]+\.[0-9]+")

if [ -z $VERSION_BEFORE ] || [ -z $VERSION_AFTER ]; then
    echo "Unexpected version parsing error"
    echo "$VERSION_DIFF" | grep $VERSION_CONSTANT
    exit 1
fi

if [ $VERSION_BEFORE = $VERSION_AFTER ]; then
    echo "No version change detected, skipping tag creation"
    exit 0
fi

TAG="v$VERSION_AFTER"
echo "Version change detected [$VERSION_BEFORE > $VERSION_AFTER], creating a new tag ..."

# DEPLOY

git config user.email "builds@travis-ci.com"
git config user.name "Travis CI"
git tag $TAG || exit 1

# USE SSH DEPLOY KEY

ENCRYPTED_KEY_VAR=encrypted_${ENCRYPTION_LABEL}_key
ENCRYPTED_IV_VAR=encrypted_${ENCRYPTION_LABEL}_iv
ENCRYPTED_KEY=${!ENCRYPTED_KEY_VAR}
ENCRYPTED_IV=${!ENCRYPTED_IV_VAR}

mkdir -p .ssh
openssl aes-256-cbc -K $ENCRYPTED_KEY -iv $ENCRYPTED_IV -in deploy-key.enc -out .ssh/deploy-key -d
chmod 600 .ssh/deploy-key
eval $(ssh-agent -s)
ssh-add .ssh/deploy-key

REPO_PATH=$(git remote -v | grep -m1 '^origin' | sed -Ene 's#.*(https://[^/]+/([^/]+/[^/.]+)).*#\2#p')
REPO_SSH_URL="git@github.com:$REPO_PATH.git"
echo "Pushing to $REPO_SSH_URL"
git remote set-url origin $REPO_SSH_URL
git push origin $TAG || exit 1

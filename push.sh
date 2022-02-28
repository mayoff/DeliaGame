#!/bin/sh

set -e

deploy=""

case "$1" in
    "--deploy")
        deploy=1
        ;;

    "")
        ;;

    *)
        echo "usage: $0 [--deploy]"
        exit 1
        ;;
esac

if [ "$(git branch --show-current)" != "main" ]; then
    echo 1>&1 "error: not on main branch"
    exit 1
fi

if ! git diff --exit-code; then
    echo 1>&2 "error: uncommitted changes"
    exit 1
fi

if ! git diff --exit-code --cached; then
    echo 1>&2 "error: uncommitted cached changes"
    exit 1
fi

make clean scramble public
if ! git diff --exit-code; then
    git commit -m 'make clean scramble publish' -- puzzles.json public/index.html
fi

if ! git diff --exit-code; then
    echo 1>&2 "error: uncommitted changes after make scramble"
    exit 1
fi

git push origin main

if [ "$deploy" = 1 ]; then
    git switch deploy
    git merge --ff main
    git switch main
    git push origin deploy
fi

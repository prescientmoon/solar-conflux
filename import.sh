#!/usr/bin/env bash

# Ensure three arguments are provided
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <language> <repo> <project-name>"
    exit 1
fi

language=$1
repo=$2
name=$3

# Ensure project with that name doesn't exist
if [ -d "$language/$name" ]; then
    echo "Error: Project `$language/$name` already exists"
    exit 1
fi

git fetch $repo
josh-filter ":prefix=$language/$name:unsign" FETCH_HEAD && git checkout FILTERED_HEAD
FILTER_BRANCH_SQUELCH_WARNING=1 \
  git filter-branch --msg-filter "awk \"{print \\\"$language($name): \\\" \\\$0}\"" -f && \
  git rebase --root --committer-date-is-author-date --signoff

hash=$(git log -1 --format='%H')
git switch master
git merge --allow-unrelated $hash --log -m "Add \`$language/$name\`"

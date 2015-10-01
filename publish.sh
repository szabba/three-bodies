#!/usr/bin/env sh
git checkout gh-pages
git ff master
git push
git checkout master
git push
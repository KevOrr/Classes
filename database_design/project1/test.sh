#!/bin/sh -e

set -v
rm nba.db
./project1.py add_coach taggart 2017 Willie Taggart 10 2 1 0 USF
./project1.py add_team USF Tampa Bulls A

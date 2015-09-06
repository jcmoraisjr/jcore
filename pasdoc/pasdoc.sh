#!/bin/sh
set -e
cd `dirname "$0"`
rm -rf output
mkdir output
pasdoc \
  --title "JCore API docs" \
  --introduction overview.txt \
  --include \
    ../core \
    ../core/*.pas \
    ../{opf,opf/drivers,opf/db}/*.pas \
    ../{ws,ws/apphandlers}/*.pas \
  --external-class-hierarchy class-hierarchy.txt \
  --format html \
  --verbosity 1 \
  --staronly \
  --use-tipue-search \
  --write-uses-list \
  --visible-members public,published \
  --graphviz-uses \
  --graphviz-classes \
  --link-gv-uses png \
  --link-gv-classes png \
  --output output
cd output/
echo "Generating uses dependency graph"
dot -Grankdir=RL -Tpng -oGVUses.png GVUses.dot
rm -f GVUses.dot
echo "Generating classes inheritance graph"
dot -Grankdir=RL -Tpng -oGVClasses.png GVClasses.dot
rm -f GVClasses.dot

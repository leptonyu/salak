#!/bin/sh
PRG=`readlink "$0"`
if [ -z "$PRG" ]; then
  PRG=$0
fi
ROOT=`dirname "$PRG"`
cd "$ROOT/.."

if echo "$1" | grep -vq '[0-9][0-9]*\(\.[0-9][0-9]*\)*' ; then
  echo "version not set"
  exit 1
fi


sed -i.bak "s|^version:\(  *\)[0-9][0-9]*\(\.[0-9][0-9]*\)*|version:\1$1|" package.yaml
rm -f package.yaml.bak

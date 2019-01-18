#!/bin/sh
PRG=`readlink "$0"`
if [ -z "$PRG" ]; then
  PRG=$0
fi
ROOT=`dirname "$PRG"`
cd "$ROOT/.."

if [ -z "$HACKAGE_USER" -o -z "$HACKAGE_PASS" ]; then
  echo "HACKAGE_USER or HACKAGE_PASS not set"
  exit 1
fi

update=`git diff-index --name-status HEAD | wc -l`

if [ "$update" -gt 0 ]; then
  echo "git has uncommitted modifications"
  exit 1
fi

name=salak
version=`grep -o '^version:\(  *\)[0-9][0-9]*\(\.[0-9][0-9]*\)*' package.yaml | awk '{print $2}'`

stack haddock

if [ $? -ne 0 ]; then
  echo "stack build failed"
  exit 1
fi

pkg=.

stack sdist $pkg && stack upload $pkg

dist=`stack path --dist-dir 2> /dev/null`
cd "$dist/doc/html"
doc=$name-$version-docs
rm -rf $doc
cp -r $name $doc
tar -c -v -z --format=ustar -f $doc.tar.gz $doc
echo `pwd`/$doc.tar.gz
curl -X PUT -H 'Content-Type: application/x-tar' \
  -H 'Content-Encoding: gzip' \
  --data-binary "@$doc.tar.gz" \
  "https://$HACKAGE_USER:$HACKAGE_PASS@hackage.haskell.org/package/$name-$version/docs"


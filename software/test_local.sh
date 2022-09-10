#!/bin/bash
set -e

../../../software/firemarshal/marshal test -s marshal-configs/mempress-bare-rocc.json
if [ $? != 0 ]; then
  echo "Bare-metal test failed"
fi

../../../software/firemarshal/marshal -d test -s marshal-configs/mempress-linux-test.json
if [ $? != 0 ]; then
  echo "Linux unit test failed"
fi

../../../software/firemarshal/marshal -d test -s marshal-configs/mempress-linux-jtr-test.json
if [ $? != 0 ]; then
  echo "Linux John the Ripper test failed"
fi

#!/bin/sh
set -e

export HOME=/root
cd /root/mempress

echo
echo 'John the Ripper: incremental crack mode [RoCC]'
rm -f long.pot
time /usr/bin/john --format=Raw-MEMPRESS-rocc --pot='long.pot' --incremental --verbosity=4 long.txt

poweroff

#!/bin/sh
set -e

export HOME=/root
cd /root/mempress

echo
echo 'John the Ripper: self-test/benchmark [baseline]'
john --test=1 --format=Raw-MEMPRESS
echo 'John the Ripper: self-test/benchmark [RoCC]'
john --test=1 --format=Raw-MEMPRESS-rocc

echo 'John the Ripper: wordlist crack mode [RoCC]'
rm -f short.pot
time john --wordlist --format=Raw-MEMPRESS-rocc --pot='short.pot' short.txt
echo

poweroff

#!/bin/bash

size=$(find /tmp/repos/rubis2.git -type f | xargs stat -f %z | paste -sd+ - | bc)
sizekb=$(echo "scale=2; $size/(1024)" | bc)
echo "$sizekb" >> dbsize.txt
echo >> keys.db
size=$(find /tmp/repos/graph_imp2.git -type f | xargs stat -f %z | paste -sd+ - | bc)
sizekb=$(echo "scale=2; $size/(1024)" | bc)
echo "$sizekb" >> dbsize.txt
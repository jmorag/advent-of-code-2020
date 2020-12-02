#!/usr/bin/zsh

for x in "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25"
  do cat tests/Days/Day03Spec.hs | sed "s/03/$x/" > tests/Days/Day"$x"Spec.hs
  done

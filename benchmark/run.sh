#!/usr/bin/env bash

existsFile() {
  [[ -f "$1" ]]
}

allTests=(ack tak sieve ary sum sum-tail fib fact prefix_sum random)

echo "[Benchmark] MinCaml"

for target in ${allTests[@]}; do
  echo "executing ${target}";
  cd mincaml--/ && ./build.ml test/${target}_time.ml 2>/dev/null && cd ..
  file=benchmark/${target}_mincaml.data
  if existsFile ${file}; then rm ${file}; fi
  for i in `seq 1 100`; do
    ./mincaml--/test/${target}_time.exe >> benchmark/${target}_mincaml.data
  done
done

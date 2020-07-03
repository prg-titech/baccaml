#!/usr/bin/env bash

existsFile() {
  [[ -f "$1" ]]
}

allTests=(ack tak sieve ary sum sum-tail fib fact prefix_sum random)

runMincaml() {
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
}

runBacCamlTracing() {
  echo "[Benchmark] BacCaml Tracing JIT"

  for target in ${allTests[@]}; do
    echo "executing ${target}"
    file=benchmark/${target}_tj.data
    existsFile ${file} && rm ${file}
    bac-caml mincaml--/test/${target}.ml | dune exec interp/interp_tj.exe -- interp/interp.mcml >> benchmark/${target}_tj.data
  done
}

TARGET=$1

if [ "${TARGET}" = "mincaml" ]; then
  runMincaml
elif [ "${TARGET}" = "tj" ]; then
  runBacCamlTracing
fi

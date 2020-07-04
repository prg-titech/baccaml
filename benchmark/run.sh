#!/usr/bin/env bash

iterNum=100

declare -a allTests
allTests=(fib fib-tail sum sum-tail square square-tail fact ack tak ary prefix_sum sieve)

existsFile() {
  [[ -f "$1" ]]
}

runMincaml() {
  echo "[Benchmark] MinCaml"

  for target in ${allTests[@]}; do
    file=benchmark/${target}_mincaml.data
    if ! existsFile ${file}; then
      # rm ${file}
      echo "executing ${target}"
      for i in `seq 1 100`; do
        cd mincaml--/ && ./build.ml test/${target}_time.ml 2>/dev/null && cd ..
        ./mincaml--/test/${target}_time.exe >> benchmark/${target}_mincaml.data
      done
    fi
  done
}

runBacCamlTracing() {
  echo "[Benchmark] BacCaml Tracing JIT"

  for target in ${allTests[@]}; do
    echo "executing ${target}"
    file=benchmark/${target}_tj.data
    existsFile ${file} && rm ${file}
    if [ "${target}" = "tak" ] || [ "${target}" = "fib" ]; then
      export THOLD_TJ=10
    elif [ "${target}" = "prefix_sum" ]; then
      export THOLD_TJ=1000
    fi
    bac-caml mincaml--/test/${target}.ml | dune exec interp/interp_tj.exe -- interp/interp.mcml >> benchmark/${target}_tj.data
    unset THOLD_TJ
  done
}

# neod manually specification
# - ack, sum, fib, random
runBacCamlMethod() {
  echo "[Benchmark] BacCaml Method JIT"

  for target in ${allTests[@]}; do
    echo "executing ${target}"
    file=benchmark/${target}_mj.data
    existsFile ${file} && rm ${file}
    if [ "${target}" = "random" ]; then
      bac-caml -no-tail mincaml--/test/${target}.ml | dune exec interp/interp_mj.exe -- interp/interp.mcml >> benchmark/${target}_mj.data 2>/dev/null
    else
      bac-caml mincaml--/test/${target}.ml | dune exec interp/interp_mj.exe -- interp/interp.mcml >> benchmark/${target}_mj.data 2>/dev/null
    fi
  done
}

TARGET=$1

if [ "${TARGET}" = "mincaml" ]; then
  runMincaml
elif [ "${TARGET}" = "tj" ]; then
  runBacCamlTracing
elif [ "${TARGET}" = "mj" ]; then
  runBacCamlMethod
fi

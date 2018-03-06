#!/bin/bash
#
# # profile this program
#
# Requires ps2pdf program that is not shipped together with stack's GHC.
#
# You can provide some +RTS arguments, e.g.
#
#  -p -hc -i0.1
#  -p -hy -i0.1
#

PROGNAMES=(et-bench-misc et-bench-spfolds et-tuple)

stack clean
stack build --bench --profile --ghc-options="-fprof-auto -rtsopts -fprof-cafs -O2" --no-run-benchmarks

for PROGNAME in ${PROGNAMES[@]}
do
  rm -f ${PROGNAME}.ps ${PROGNAME}.hp ${PROGNAME}.aux ${PROGNAME}.prof
  time `stack path --dist-dir`/build/${PROGNAME}/${PROGNAME} +RTS ${@:--p -hc -i0.1}
  stack exec hp2ps -- -c ${PROGNAME}.hp
  ps2pdf ${PROGNAME}.ps
  rm -f ${PROGNAME}.ps ${PROGNAME}.hp ${PROGNAME}.aux
done

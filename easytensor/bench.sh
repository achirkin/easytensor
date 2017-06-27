#!/bin/bash

stack build --executable-profiling --bench --library-profiling --ghc-options="-fforce-recomp -fprof-auto -rtsopts -fprof-cafs -O1"

time $(stack path --dist-dir)/build/et-bench-spfolds/et-bench-spfolds +RTS -p -hc -i0.05
stack exec hp2ps -- -c et-bench-spfolds.hp
ps2pdf et-bench-spfolds.ps
mv et-bench-spfolds.hp et-bench-spfolds-hc.hp
mv et-bench-spfolds.ps et-bench-spfolds-hc.ps
mv et-bench-spfolds.pdf et-bench-spfolds-hc.pdf
mv et-bench-spfolds.prof et-bench-spfolds-hc.prof


time $(stack path --dist-dir)/build/et-bench-spfolds/et-bench-spfolds +RTS -p -hy -i0.05
stack exec hp2ps -- -c et-bench-spfolds.hp
ps2pdf et-bench-spfolds.ps
mv et-bench-spfolds.hp et-bench-spfolds-hy.hp
mv et-bench-spfolds.ps et-bench-spfolds-hy.ps
mv et-bench-spfolds.pdf et-bench-spfolds-hy.pdf
mv et-bench-spfolds.prof et-bench-spfolds-hy.prof

#!/bin/bash
S="$1"; P="$2"
for i in 1 2 3 4 5 6; do
  Rscript "$S/investigacion.R" "$P" "$S/parte_$i.csv" $i 6 > "$S/parte_$i.log" 2>&1 &
done
wait
Rscript -e 'a <- commandArgs(TRUE); x <- do.call(rbind, lapply(Sys.glob(file.path(a[1], "parte_*.csv")), read.csv)); write.csv(x, file.path(a[1], "investigacion.csv"), row.names = FALSE); cat("Casos:", nrow(x), "\n")' "$S"

set terminal svg enhanced size 1600,900 font "monospace,14"
set datafile separator ","
set boxwidth 0.8
set style fill solid 0.8
set yrange [0:*]
set grid y
set xtics font ",12"

color1="#DD2233"
color2="#333333"

# Runtime
set output "bench_runtime.svg"
set title "Runtime (5 MB input)"
set ylabel "Runtime (s)"

plot \
    "hq-bench.csv" skip 1 using ($0 % 2 == 0 ? $0 : NaN):2:xtic(1) \
        notitle with boxes lc rgb color1, \
    "" skip 1 using ($0 % 2 == 1 ? $0 : NaN):2:xtic(1) \
        notitle with boxes lc rgb color2

# Peak memory
set output "bench_memory.svg"
set title "Peak RSS (5 MB input)"
set ylabel "Peak RSS (MB)"

plot \
    "hq-bench.csv" skip 1 using ($0 % 2 == 0 ? $0 : NaN):3:xtic(1) \
        notitle with boxes lc rgb color1, \
    "" skip 1 using ($0 % 2 == 1 ? $0 : NaN):3:xtic(1) \
        notitle with boxes lc rgb color2

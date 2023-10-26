
library(microbenchmark)
time <- microbenchmark(
    source("llr_functions.R"),
    times = 1000)
time

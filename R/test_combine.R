out <- foreach(ind = iterators::icount(2000)) %do% {list(matrix(rnorm(10*10000), 10, 10000), rnorm(10))}
system.time(out1 <- lapply(out, `[[`, 1))
system.time(out2 <- lapply(out, `[[`, 2))

system.time(a <- do.call(rbind, out1))
system.time(b <- matrix(unlist(out1), nrow = length(out1), byrow = TRUE))

outs <- foreach(ind = iterators::icount(2)) %do% {list(matrix(rnorm(25), 5, 5), rnorm(5), NULL)}
system.time(outs1 <- lapply(outs, `[[`, 1))
system.time(outs2 <- lapply(outs, `[[`, 2))
system.time(outs3 <- lapply(outs, `[[`, 3))

microbenchmark(times = 5, do.call(c, out2), unlist(out2))

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) unlist(c(x[[i]], lapply(list(...), function(y) y[[i]]))))
}

f1 <- function() {
  oper <- foreach(i=1:1000, .combine='rbind', .multicombine=TRUE) %dopar% {
    list(matrix(5, 5, 500), matrix(4, 6, 600), NULL, 3, 5)
  }
  oper
}


f2 <- function() {
oper2 <- foreach(i=1:1000) %dopar% {
  list(matrix(5, 5, 500), matrix(4, 6, 600), NULL, 3, 5)
}
oper2
}

f3 <- function() {
oper3 <- foreach(i=1:1000, .combine = 'rbind') %dopar% {
  list(matrix(5, 5, 500), matrix(4, 6, 600), NULL, 3, 5)
}
oper3
}

f4 <- function() {
oper4 <- foreach(i=1:1000, .combine = function(...) data.table::rbindlist(list(...)),
                 .multicombine = TRUE) %dopar% {
  list(a = list(matrix(5, 5, 500)), b = list(matrix(4, 6, 600)), c = list(NULL), d = 3, e = 5)
                 }
oper4
}

oper2 <- foreach(i=1:10) %dopar% {
                  list(i+2, i+3, i+4)
                }

comb2 <- function(x, y) {
  c(rbind(x))
}

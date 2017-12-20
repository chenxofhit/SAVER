
calc.cutoff <- function(x, x.est, npred, pred.cells, nworkers, output.se,
                        verbose) {
  iterx <- iterators::iter(as.matrix(x), by = "row",
                           chunksize = ceiling(nrow(x)/nworkers))
  n <- length(pred.cells)
  out <- suppressWarnings(
    foreach::foreach(ix = iterx, .packages = c("glmnet", "SAVER")) {
      if (npred > 100) {
        maxcor <- calc.maxcor(x.est, ix)
      } else {
        maxcor <- NULL
      }

      x.names <- rownames(ix)
      x.est.names <- colnames(x.est)
      est <- matrix(0, nrow(ix), ncol(ix))
      if (output.se == TRUE) {
        se <- matrix(0, nrow(ix), col(ix))
      } else {
        se <- NULL
      }
      lambda.min <- rep(0, nrow(ix))
      sd.cv <- rep(0, nrow(ix))

      for (i in 1:nrow(ix)) {
        sameind <- which(x.est.names == x.names[i])
        y <- ix[i, pred.cells]/sf[pred.cells]
        if (length(sameind) == 1) {
          cv <- tryCatch(
            suppressWarnings(glmnet::cv.glmnet(x.est[pred.cells, -sameind], y,
                                               family="poisson", dfmax = 300,
                                               nfolds = 5)),
            error = function(cond) {
              if (verbose)
                message(cond, "\n")
              return(NA)
            }
          )
        } else {
          cv <- tryCatch(
            suppressWarnings(glmnet::cv.glmnet(x.est[pred.cells, ], y,
                                               family="poisson", dfmax = 300,
                                               nfolds = 5)),
            error = function(cond) {
              if (verbose)
                message(cond, "\n")
              return(NA)
            }
          )
        }
        if (length(cv) == 1) {
          mu <- rep(mean(y), n)
          lambda.min[i] <- maxcor[i]*sd(y)*(n-1)/n
        }
      }
    }
  )
}

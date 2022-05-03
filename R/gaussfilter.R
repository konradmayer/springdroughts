gaussfilter <- function(ts, deltaT) {
  T <- deltaT + 1
  z <- 6 / T
  k <- seq((T / 2 + 0.5) - T, (T / 2 - 0.5), 1)
  x <- k * z

  wk <- mat.or.vec(T, 1)

  for (i in 1:T) {
    wk[i] <- (1 / (sqrt(2 * pi))) * exp(1)^(-(x[i]^2 / 2))
  }

  w <- sum(abs(wk))

  wo <- wk / w

  tf <- mat.or.vec(length(ts), 1)

  for (t in 1:(length(ts) - T + 1)) {
    tseq <- ts[t:(t + T - 1)]
    tf[t + (T - 1) / 2] <- sum(tseq * wo)
  }


  # yearseq<-seq(1780,2007)

  for (t in 1:((T - 1) / 2)) {
    wo_red <- wo[1:(length(wo) - t)]
    x <- sum(wo_red) / 1
    wo_t <- wo_red / x

    tseq <- ts[(length(ts) - (T - 1) + t):length(ts)]

    tf[(length(ts) - ((T - 1) / 2) + t)] <- sum(tseq * wo_t)
  }

  for (t in 1:((T - 1) / 2)) {
    wo_red <- wo[(t + 1):length(wo)]
    x <- sum(wo_red) / 1
    wo_t <- wo_red / x

    tseq <- ts[1:(T - t)]

    tf[(((T - 1) / 2) - t + 1)] <- sum(tseq * wo_t)
  }

  tf
}

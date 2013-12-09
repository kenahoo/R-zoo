# rollmean, rollmax, rollmedian (, rollmad) based on code posted by Jarek Tuszynski at
# https://www.stat.math.ethz.ch/pipermail/r-help/2004-October/057363.html
# ToDo: rollmad, currently rollapply() can be used

rollmeanr <- function(..., align = "right") {
	rollmean(..., align = align)
}

rollmean <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
	UseMethod("rollmean")
}

rollmean.zoo <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {

  if (!missing(na.pad)) warning("na.pad is deprecated. Use fill.")

  align <- match.arg(align)

  if (length(dim(x)) == 2) {
	  # merge is the only zoo specific part of this method
	  
	  out <- do.call("merge", c(lapply(1:NCOL(x), function(i) {
		rollmean(x[, i, drop = TRUE], k, fill = fill, align = align, ...)
	  }), all = FALSE))
	  if (ncol(x) == 1) dim(out) <- c(length(out), 1)
	  colnames(out) <- colnames(x)
	  return(out)
  }

  n <- length(x)
  stopifnot(k <= n)

  ix <- switch(align,
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })

  xu <- unclass(x)
  y <- xu[k:n] - xu[c(1, seq_len(n-k))] # difference from previous
  y[1] <- sum(xu[1:k])		 # find the first
  # sum precomputed differences
  rval <- cumsum(y)/k

  x[ix] <- rval
  na.fill(x, fill = fill, ix)

}

rollmean.default <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		coredata(rollmean(zoo(x), k, fill = fill, align = align, ...))
}

rollmean.ts <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		as.ts(rollmean(as.zoo(x), k, fill = fill, align = align, ...))
}

rollsumr <- function(..., align = "right") {
	rollsum(..., align = align)
}

rollsum <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
	UseMethod("rollsum")
}

rollsum.zoo <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {

  if (!missing(na.pad)) warning("na.pad is deprecated. Use fill.")

  align <- match.arg(align)

  if (length(dim(x)) == 2) {
	  # merge is the only zoo specific part of this method
	  
	  out <- do.call("merge", c(lapply(1:NCOL(x), function(i) {
		rollsum(x[, i, drop = TRUE], k, fill = fill, align = align, ...)
	  }), all = FALSE))
	  if (ncol(x) == 1) dim(out) <- c(length(out), 1)
	  colnames(out) <- colnames(x)
	  return(out)
  }

  n <- length(x)
  stopifnot(k <= n)

  ix <- switch(align,
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })

  xu <- unclass(x)
  y <- xu[k:n] - xu[c(1, seq_len(n-k))] # difference from previous
  y[1] <- sum(xu[1:k])		 # find the first
  # sum precomputed differences
  rval <- cumsum(y)

  x[ix] <- rval
  na.fill(x, fill = fill, ix)

}

rollsum.default <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		coredata(rollsum(zoo(x), k, fill = fill, align = align, ...))
}

rollsum.ts <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		as.ts(rollsum(as.zoo(x), k, fill = fill, align = align, ...))
}

rollmaxr <- function(..., align = "right") {
	rollmax(..., align = align)
}

rollmax <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
	UseMethod("rollmax")
}

rollmax.zoo <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {

  if (!missing(na.pad)) warning("na.pad is deprecated. Use fill.")

  align <- match.arg(align)

  if (length(dim(x)) == 2) {
	  # merge is the only zoo specific part of this method
	  out <- do.call("merge", c(lapply(1:NCOL(x), function(i) {
		rollmax(x[, i, drop = TRUE], k, fill = fill, align = align, ...)
	  }), all = FALSE))
	  if (ncol(x) == 1) dim(out) <- c(length(out), 1)
	  colnames(out) <- if (ncol(x) == ncol(out)) colnames(x)
	  return(out)
  }

  n <- length(x)
  stopifnot(k <= n)

  ix <- switch(align,
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })

  n <- length(x) 
  rval <- rep(0, n) 
  a <- 0
  xc <- coredata(x)
  for (i in k:n) {
  rval[i] <- if (is.na(a) || is.na(rval[i=1]) || a==rval[i-1]) 
      max(xc[(i-k+1):i]) # calculate max of window
  else 
      max(rval[i-1], xc[i]); # max of window = rval[i-1] 
  a <- xc[i-k+1] # point that will be removed from window
  }
  rval <- rval[-seq(k-1)]

  x[ix] <- rval
  na.fill(x, fill = fill, ix)

}

rollmax.default <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		coredata(rollmax(zoo(x), k, fill = fill, align = align, ...))
}

rollmax.ts <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		as.ts(rollmax(as.zoo(x), k, fill = fill, align = align, ...))
}


rollmedianr <- function(..., align = "right") {
	rollmedian (..., align = align)
}

rollmedian <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
	UseMethod("rollmedian")
}

rollmedian.zoo <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {

  if (!missing(na.pad)) warning("na.pad is deprecated. Use fill.")

  align <- match.arg(align)

  if (length(dim(x)) == 2) {
	  # merge is the only zoo specific part of this method
	  out <- do.call("merge", c(lapply(1:NCOL(x), function(i) {
		rollmedian(x[, i, drop = TRUE], k, fill = fill, align = align, ...)
	  }), all = FALSE))
	  if (ncol(x) == 1) dim(out) <- c(length(out), 1)
	  colnames(out) <- colnames(x)
	  return(out)
  }

  n <- length(x)
  stopifnot(k <= n)

  ix <- switch(align,
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })

  m <- k %/% 2
  rval <- runmed(x, k, ...)
  attr(rval, "k") <- NULL
  if(m >= 1) rval <- rval[-c(1:m, (n-m+1):n)]

  x[ix] <- rval
  na.fill(x, fill = fill, ix)

}

rollmedian.default <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		coredata(rollmedian(zoo(x), k, fill = fill, align = align, ...))
}

rollmedian.ts <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, 
	align = c("center", "left", "right"), ...) {
		as.ts(rollmedian(as.zoo(x), k, fill = fill, align = align, ...))
}


rollsdr <- function(..., align = "right") {
  rollsd(..., align = align)
}

rollsd <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE,
                   align = c("center", "left", "right"), ...) {
  UseMethod('rollsd')
}

rollsd.zoo <- function(x, ...) {
  zoo(rollsd(coredata(x), ...), index(x))
}


##' Efficiently computes the rolling-window sample standard deviation
##' of a numeric vector.  Uses the formula
##'   Var(x) = mean(x^2) - mean(x)^2
##' and then adjusts by n/(n-1) for the sample mean.
rollsd.default <- function(x, k, align = c("center", "left", "right"), na.rm=FALSE) {
  stopifnot(is.vector(x))

  n <- length(x)

  align <- match.arg(align)
  if (align != 'right')
    stop("Only 'right' alignment is currently supported")

  v <- cbind(
    s0 = c(0,cumsum(!is.na(x)))
  )

  if (na.rm) {
    x[is.na(x)] <- 0
  }

  v <- cbind( v,
              s1 = c(0,cumsum(x)),
              s2 = c(0,cumsum(x^2))
  )

  if (is.vector(k)) {
    if(!is.numeric(k))
      stop("'k' must be a numeric vector or list of numeric vectors")

    if(length(k)==1)
      v <- diff(v, k)
    else
      v <- v[1+seq_along(k), ] - v[1+seq_along(k) - k, ]

  } else if (is.list(k) && all(lapply(k, is.numeric))) {
    stop("list not handled yet")

  } else {
    stop("'k' must be a numeric vector or list of numeric vectors")
  }

  ret <- suppressWarnings( sqrt( (v[,'s2'] - v[,'s1']^2/v[,'s0'])/(v[,'s0']-1) ) )
  ret[v[,'s0']==1 | v[,'s0']==0] <- NA
  ret
}

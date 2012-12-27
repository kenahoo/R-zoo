##' Like zoo's rollapply(), but the width is the period spanned by the
##' observations, not the number of observations.  This makes a
##' difference for irregular time series.
##'
##' @param width the width of the rolling window, in the units of
##' \code{index(data)}.  When the data is indexed by a \code{POSIXct}
##' object, it's most efficient to use a \code{duration} object for
##' \code{width}, not a \code{period} object.
##' @param ix an alternative index to use for determining the time
##' between two data points.  If the index is e.g. a \code{POSIXct}
##' and the data is large, performance can be much improved by
##' converting \code{width} to a numeric and passing a numeric
##' \code{ix}.
rollwindow <- function(data, width, FUN, ..., partial=FALSE,
                       align=c('center','left','right'), ix=index(data)) {

  align <- match.arg(align)
  lengths <- windowsize(width=width, partial=partial, align=align, ix=ix)
  rollapply(data, lengths, FUN, ..., partial=partial, align=align)
}

windowsize <- function(width, partial=FALSE,
                       align=c('center','left','right'), ix=index(data)) {

  stopifnot(partial)  ## Only partial=TRUE supported so far
  align <- match.arg(align)
  stopifnot(align=='right')  ## Only align='right' supported so far

  ## This can be a big speedup
  if(inherits(ix, 'POSIXct') && inherits(width, 'Duration')) {
    ix <- as.numeric(ix)
    width <- as.numeric(width)
  }

  ## This here can be a big speedup, without losing accuracy
  if(is.POSIXct(ix) && is.duration(width)) {
    ix <- as.numeric(ix)
    width <- as.numeric(width)
  }

  i <- 1
  lengths <- integer(length(ix))
  for (j in seq_along(ix)) {
    while(ix[j] - width > ix[i]) i <- i+1
    lengths[j] <- j-i+1
  }

  return(lengths)
}


rollsd <- function(x, k, fill = if (na.pad) NA, na.pad = FALSE, align=c('center', 'left', 'right'), ...) {
  UseMethod('rollsd')
}


rollsd.zoo <- function(x, ...) {
  zoo(rollsd(coredata(x), ...), index(x))
}


##' Efficiently computes the rolling-window sample standard deviation
##' of a numeric vector.  Uses the formula
##'   Var(x) = mean(x^2) - mean(x)^2
##' and then adjusts by n/(n-1) for the sample mean.
rollsd.default <- function(x, k, fill, align=c('center','left','right'), na.rm=FALSE) {
  if (length(dim(x)) >= 2)
    stop("Can't handle multi-dimensional input yet")

  n <- length(x)
  stopifnot(k <= n)

  align <- match.arg(align)
  if (align != 'right')
    stop("Only 'right' alignment is currently supported")

  nas <- is.na(x)
  x[nas] <- 0
  v <- cbind(
    s0 = c(0,cumsum(!nas)),
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
    stop("k=list not handled yet")

  } else {
    stop("'k' must be a numeric vector or list of numeric vectors")
  }

  ret <- suppressWarnings( sqrt( (v[,'s2'] - v[,'s1']^2/v[,'s0'])/(v[,'s0']-1) ) )
  ret[v[,'s0']==1 | v[,'s0']==0] <- NA
  if (!na.rm)
    ret[ rollany(nas, k, align=align) ] <- NA

  ret
}


rollsd.test <- function() {
  library(testthat)

  x <- 1:5
  k <- c(1,1,3,2,2)

  expect_that(rollsd(x, k, align='right'),
              equals(rollapply(x, k, sd, align='right'))

  x <- c(2,6,4,NA,5,2,8,6)
  k <- c(1,1,2, 2,3,2,3,3)

  expect_that(rollsd(x, k, na.rm=TRUE, align='right'),
              equals(rollapply(x, k, sd, align='right', na.rm=TRUE)))

  expect_that(rollsd(x, k, na.rm=FALSE, align='right'),
              equals(rollapply(x, k, sd, align='right', na.rm=FALSE)))
}

rollany <- function(data, k, ...) {
  rollsum2(as.logical(data), k, ...) > 0
}

rollany.test <- function() {
  library(testthat)
  x <- runif(1000, -1, 1) > 0
  expect_that(rollany(x, 4, align='right'),
              equals(rollapply(x, 4, any, align='right')))
}


##' Equivalent to rollsum() in 'zoo', but accepts a non-scalar 'width'
##' attribute.
rollsum2 <- function(data, width, align=c('center','left','right'), na.rm=FALSE, partial=FALSE) {
  align <- match.arg(align)
  if (align != 'right')
    stop("Only 'right' alignment is currently supported")
  if (na.rm) data[is.na(data)] <- 0
  cs <- cumsum(c(0,data))

  ## Check for constant-width
  if(length(width)==1) {
    if(!partial) return( cs[(width+1):length(cs)] - cs[1:(length(cs)-width)] )

    width <- pmin(seq_along(data), width)
  }

  cs[seq_along(width) + 1] - cs[seq_along(width) - width + 1]
}

rollsum2.test <- function() {
  library(testthat)

  x <- c(2,6,4,3,5,2,8,6)
  w <- c(1,1,2,2,3,2,3,3)

  expect_that(rollsum2(x, w, align='right'),
              equals(rollapply(x, w, sum, align='right')))

  x2 <- x
  x2[4] <- NA
  expect_that(rollsum2(x2, w, align='right', na.rm=TRUE),
              equals(rollapply(x2, w, sum, align='right', na.rm=TRUE)))

  ## Constant width
  expect_that(rollsum2(x, 4, align='right', partial=FALSE),
              equals(rollapply(x, 4, sum, align='right', partial=FALSE)))
  expect_that(rollsum2(x, 4, align='right', partial=TRUE),
              equals(rollapply(x, 4, sum, align='right', partial=TRUE)))


  ## Doesn't currently work:
  expect_that(rollsum2(x, w, align='right', na.rm=FALSE),
              equals(rollapply(x, w, sum, align='right', na.rm=FALSE)))
}

##' Equivalent to rollsum() in 'zoo', but accepts a vector 'width'
##' parameter.
rollmean2 <- function(data, width, align=c('center','left','right'), na.rm=FALSE) {
  rollsum2(data, width, align, na.rm) / rollsum2(!is.na(data), width, align)
}

rollmean2.test <- function() {
  library(testthat)

  x <- c(2,6,4,3,5,2,8,6)
  w <- c(1,1,2,2,3,2,3,3)

  expect_that(rollmean2(x, w, align='right'),
              equals(rollapply(x, w, mean, align='right')))

  x[4] <- NA
  expect_that(rollmean2(x, w, align='right', na.rm=TRUE),
              equals(rollapply(x, w, mean, align='right', na.rm=TRUE)))

  ## Doesn't currently work:
  expect_that(rollmean2(x, w, align='right', na.rm=FALSE),
              equals(rollapply(x, w, mean, align='right', na.rm=FALSE)))
}

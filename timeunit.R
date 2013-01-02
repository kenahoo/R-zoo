# timeunit.R
# Zoo Time Window Functions
# Copyright (C) 2011 by Matthew Clegg
#
# This package contains functions that facilitate making computations over
# fixed-length time windows in zoo data series.  For example, a typical question
# that might be addressed is as follows:
#
# Given a zoo-based object consisting of the daily closing prices of a stock,
# for each date in the time series, what is the volatility over the succeeding
# 30 calendar days? Probably many people would settle for something like:
#    rollapply (log(lag(P))-log(P), 21, sd, align="left") * sqrt(252)
# (where P is the price series).  However, this is an approximation.
# Not all periods of 30 calendar days include precisely 21 trading days.
#
# An alternative might be to use a formulation based upon sapply,
# for example:
#   sapply(seq_along(X), function(i) FUN(X[time(X) >= time(X)[i] & time(X) <= time(X)[i] + delta], ...)
# However, for vectors of moderate length, this can be very inefficient, as
# the asymptotic execution time of this formulation is O(N^2), where N is the
# length of the time series.
#
# The timeunit_* functions provide an efficient method for performing computations
# of this nature.  For moderately sized vectors, the speedup can be more than a
# factor of ten over sapply, and for longer vectors, a 99% speedup is possible.

timeunit_findfirsts <- function (X) {
  # On input, X is a vector that is sorted into non-decreasing order.
  # For each element X[i], finds the minimum j such that X[i] == X[j].
  # Thus, the result vector R satisfies:
  #   R[i] = min{ k: X[i] == X[k] }

  Z <- 1:length(X)
  D <- c(FALSE, X[1:(length(X)-1)] == X[2:length(X)])
  Z[D] <- 0
  cummax(Z)
}

timeunit_left_endpoint_index <- function (TS, delta, closed=TRUE) {
  # On input, TS is an ordered vector of timestamps, numerics or
  # other values which support the operations '-' (scalar minus) and '<'
  # comparison.  Finds the indices of the left-hand endpoints of intervals
  # whose left endpoints are a distance at most delta.  If closed is FALSE,
  # then points at a distance of exactly delta are included, otherwise
  # they are excluded.
  #
  # Thus, for closed=TRUE, the result vector L satisfies:
  #   L[k] = min { k' in 1:length(TS) | TS[k'] >= TS[k] + delta},
  # and for closed=FALSE, the result vector L satisfies:
  #   L[k] = min { k' in 1:length(TS) | TS[k'] > TS[k] + delta}.
  #
  # Note that if delta > 0, then the above sets may be empty for indices
  # near the end of the vector.  NA's are returned in these locations.
  #
  # For closed=TRUE, this is equivalent to:
  #   L = sapply(seq_along(TS), function(i) match(TRUE, TS >= TS[i]+delta)])
  # For closed=FALSE, this is equivalent to:
  #   L = sapply(seq_along(TS), function(i) match(TRUE, TS > TS[i]+delta)])
  #
  # Parameters:
  # TS:            A vector of timestamps, numerics or other compatible values.
  #                TS must have at least one element.
  # delta:         An object of type difftime specifying the size of
  #                the time window.
  #
  # Returns an integer vector of length T representing the indices of the
  # lefthand endpoints of the specified intervals.

  if (length(TS) == 0) return(integer(0))
  if (length(TS) == 1) {
    if ((delta[1] > 0) || (!closed && delta[1] == 0)) {
      return (NA_integer_)
    } else {
      return(1L)
    }
  }

  IX <- findInterval(TS + delta, TS)
  if (closed) {
    IX <- pmax(1L, IX)
    bump <- TS[IX] < TS + delta
    IX[bump] <- IX[bump] + 1L
    IX <- timeunit_findfirsts(TS)[IX]
    IX[TS[IX] < TS + delta] <- NA_integer_
  } else {
    IX <- pmin(IX + 1, length(TS))
    IX[TS[IX] <= TS + delta] <- NA_integer_
  }

  IX
}

timeunit_right_endpoint_index <- function (TS, delta, closed=TRUE) {
  # Zoo Time Window Lag Indices
  #
  # On input, TS is an ordered vector of timestamps, numerics or
  # other values which support the operations '-' (scalar minus) and '<'
  # comparison.  Finds the indices of the right-hand endpoints of intervals
  # whose right endpoints are a distance at most delta.  If closed is TRUE,
  # then points at a distance of exactly delta are included, otherwise
  # they are excluded.
  #
  # Thus, for closed=TRUE, the result vector L satisfies:
  #   L[k] = max { k' in 1:length(TS) | TS[k] <= TS[k'] + delta }
  # and for closed=FALSE, the result vector L satisfies:
  #   L[k] = max { k' in 1:length(TS) | TS[k] < TS[k'] + delta }
  #
  # Note that if delta < 0, then the above sets may be empty for indices
  # near the beginning of the vector.  NA's are returned in these locations.
  #
  # For closed=TRUE, this is equivalent to:
  #   L = sapply(seq_along(TS), function(i) max(which(TS <= TS[i]+delta)))
  # For closed=FALSE, this is equivalent to:
  #   L = sapply(seq_along(TS), function(i) max(which(TS < TS[i]+delta)))
  #
  # Parameters:
  # TS:            A vector of timestamps, numerics or other compatible values.
  #                TS must have at least one element.
  # delta:         An object of type difftime specifying the size of
  #                the time window.
  #
  # Returns an integer vector of length T representing the indices of the
  # righthand endpoints of the specified intervals.

  if (length(TS) == 0) return(integer(0))
  if (length(TS) == 1) {
    if ((delta[1] < 0) || (!closed && delta[1] == 0)) {
      return(NA_integer_)
    } else {
      return(1L)
    }
  }

  IX <- pmax(1L, findInterval(TS + delta, TS))
  if (closed) {
    IX[TS[IX] > TS + delta] <- NA_integer_
  } else {
    IX <- timeunit_findfirsts(TS)[IX]
    bump <- (IX > 1) & (TS[IX] == TS + delta)
    IX[bump] <- IX[bump] - 1L
    IX[TS[IX] >= TS + delta] <- NA_integer_
  }

  IX
}

timeunit_lag_index <- function (TS, delta, closed=TRUE) {
  # Zoo Time Window Lag Indices
  #
  # On input, TS is an ordered vector of timestamps, numerics or
  # other values which support the operations '-' (scalar minus) and '<'
  # comparison.  Constructs a vector of offsetting indexes representing
  # time windows of length at most delta.
  #
  # Parameters:
  # TS:            A vector of timestamps, numerics or other compatible values.
  #                TS must have at least one element.
  # delta:         An object of type difftime specifying the size of
  #                the time window.
  #
  # For delta > 0, the values in the result vector L satisfy:
  #   L[k] = max { k' in 1:length(TS) | TS[k] <= TS[k'] + delta }
  # Conversely, for delta <= 0, then values in the result vector L satisfy:
  #   L[k] = min { k' in 1:length(T) | TS[k'] >= TS[k] + delta }
  #
  # For delta > 0, this is equivalent to:
  #   L = sapply(seq_along(TS), function(i) max(which(TS <= TS[i]+delta)))
  # For delta < 0, this is equivalent to:
  #   L = sapply(seq_along(TS), function(i) match(TRUE, TS >= TS[i]+delta)])
  #
  # If closed=FALSE, then the inequalities in the above definitions are made
  # strict, e.g., points that are at a distance of exactly delta are not
  # included.
  #
  # Returns an integer vector of length T representing the lag indices.

  if (any(delta <= 0) && any(delta > 0)) {
    left_lags <- timeunit_left_endpoint_index(TS, delta, closed)
    right_lags <- timeunit_right_endpoint_index(TS, delta, closed)
    left_lags[delta > 0] <- right_lags[delta > 0]
    return (left_lags)
  } else if (any(delta <= 0)) {
    left_lags <- timeunit_left_endpoint_index(TS, delta, closed)
    return (left_lags)
  } else {  # all(delta > 0)
    right_lags <- timeunit_right_endpoint_index(TS, delta, closed)
    return (right_lags)
  }
}

timeunit_lag <- function (X, delta, result.type = c("values", "indexes", "times"),
                          partial=FALSE, na.pad=FALSE, closed=TRUE) {
  # Zoo Time Window Lag
  #
  # On input, X is a zoo-based vector and delta is a time difference.
  # Constructs a zoo-based vector of lagged values from X.  If delta
  # is positive, then lags are forward, and if delta is negative, then lags
  # refer to times in the past.
  #
  # Thus, for delta > 0, the values in the result vector R satisfy:
  #   R[d] = X[max { d' in time(X) | d' <= time(X[d]) + delta }]
  # Conversely, for delta <= 0, then values in the result vector R satisfy:
  #   R[d] = X[min { d' in time(X) | d' >= time(X[d]) - delta }]
  #
  # For delta > 0, this is equivalent to:
  #   R <- sapply(seq_along(X), function(i) coredata(X)[max(which(time(X) <= time(X[i])+delta))])
  #   R <- zoo(R, order.by=time(X))
  # For delta <= 0, this is equivalent to:
  #   R <- sapply(seq_along(X), function(i) coredata(X)[match(TRUE, time(X) >= time(X[i])+delta)])
  #   R <- zoo(R, order.by=time(X))
  #
  # If closed=FALSE, then the inequalities in the above definitions are made
  # strict, e.g., points that are at a distance of exactly delta are not
  # included.
  #
  # Parameters:
  # X:             A zoo-based vector with a time-based index type.
  # delta:         An object of type difftime specifying the size of
  #                the time window.
  # result.type:   One of "values", "indexes" or "times".  If "values" (default),
  #                then the result is a zoo-vector as described above.  If "indexes",
  #                then the result is an integer vector I of indexes into the original
  #                vector X, where X[I[k]] is the value that would be returned in R[k].
  #                If "times", then the result is a vector T of timestamps, representing
  #                the actual lags that are found.  Thus, T = time(X)[I].
  # partial:       If TRUE, then lagged values are returned for time windows
  #                at the end of the vector which may not be full.
  # na.pad:        If TRUE, then NA's are returned for lagged values which correspond
  #                to partial time windows.  This parameter is only valid if
  #                result.type = "values", and it takes precedence over partial = TRUE.
  #
  # Returns a zoo-based vector of lagged values.

  if (length(X) == 0) {
    return (X)
  }
  
  if (!inherits(X, "zoo")) {
    stop ("X must be a vector of type zoo");
  }

  result.type <- match.arg(result.type)

  TS <- time(X)
  IX <- timeunit_lag_index(TS, delta, closed)

  if (!partial || na.pad) {
    if (delta < 0) {
      imin <- findInterval(TS[1]-delta, TS)
      imax <- length(TS)
    } else {
      imin <- 1
      imax <- findInterval(TS[length(TS)]-delta, TS)
    }
    if (!na.pad) {
      TS <- TS[imin:imax]
      IX <- IX[imin:imax]
    }
  }
  
  if (result.type == "values") {
    Xc <- coredata(X)
    Ina <- integer(0)
    if (na.pad) {
      if (imin > 1) Ina <- 1:(imin-1)
      if (imax < length(TS)) Ina <- (imax+1):length(TS)
    }
    if (is.null(dim(Xc))) {
      Z <- zoo(Xc[IX], order.by=TS)
      Z[Ina] <- NA
    } else {
      Z <- zoo(Xc[IX,,drop=FALSE], order.by=TS)
      Z[Ina,] <- NA
    }
    return (Z)
  } else if (result.type == "indexes") {
    return (IX)
  } else {
    return (time(X)[IX])
  }
}

timeunit_apply <- function (data, width, FUN, ...,
                            align=c("center", "left", "right"),
                            partial=FALSE,
                            FUN.VALUE=NA,
                            by.column=TRUE,
                            fill=NA,
                            include.endpoints=c(TRUE,TRUE)) {
  # Zoo Time Window Apply
  #
  # Applies the function FUN to each window of length at most width in the
  # zoo vector data.  Returns the vector of result values.
  #
  # Parameters:
  # data:     A zoo-based vector with a time-based or numeric index type.
  # width:    An object compatible with difftime specifying the size of
  #           the time window.  Width can be specified as a scalar, a vector
  #           or as a list (see below).
  # FUN:      The function to be applied to each width-length time window of data.
  # align:    Specifies whether the result for a given index should
  #           be computed using elements of lower timestamps ("right")
  #           or higher timestamps ("left"), or timestamps from both
  #           the left and the right ("center").
  # partial:  If TRUE, then partial values are computed for elements
  #           at the left (respectively, right) end of the vector.
  #           A numeric argument to partial can be used to determine
  #           the minimum window size for partial computations.
  # FUN.VALUE: A template for the return value from FUN (see vapply).
  #           If given, and if data is an undimensioned vector, then
  #           uses vapply to evaluate the function FUN.
  # by.column: If data is two-dimensional, then applies FUN to each column
  #           separately within each window, and concatenates the results.
  # fill:     A three-component vector or list (recycled otherwise) providing
  #           filling values at the left/within/to the right of the data range.
  #           See the fill argument of na.fill for details.
  # include.endpoints:  Specifies whether or not the left and/or right endpoints
  #           should be included in the time intervals.  If this parameter is
  #           omitted, then the choice to include the endpoints is dependent
  #           on the value of align.  For align="center" and align="right",
  #           the left endpoint is included and the right is not.  For align="left",
  #           the right endpoint is included and the left is not.
  #
  # Details
  #
  # We imagine the elments of \code{data} laid out on a timeline stretching
  # from left to right.  A window of \code{width} time units is
  # advanced along the timeline.  Within each window, the function
  # \code{FUN} is applied to the elements of \code{data} lying within that
  # window, yielding a result that is associated with that window.
  # Because the width of the window is measured in units of time rather
  # than in size of its contents, the number of elements within each
  # window may vary.
  #
  # As with rollapply, width may be given as a scalar, a vector or a list.
  # If a vector, then a different window size is used for each element of data,
  # as specified by the corresponding element of width.  If a list, then the list
  # should be given as list of pairs (L,R).  Each pair represents the left and right
  # offsets to be used in computing the corresponding window.
  # 
  # If align="right", then the result vector R is right-aligned with the
  # input vector data.  In other words, the value returned in R[i] is obtained
  # by applying FUN to those values data[j] which are to the left of i
  # (e.g. j < i) and which satisfy time(i) - time(j) < width.  Conversely,
  # if align="left", then the result vector R is left-aligned with the
  # input vector.
  #
  # A question arises as to how to treat the first few (respectively, last)
  # windows, which may be substantially less than width in width.  If partial=TRUE,
  # then FUN is applied to these windows as well and the results are included in R.
  # Otherwise, these values are omitted from the result vector.
  #
  # If align="right", then result[i] is a sum of those elements 
  # data[j] such that
  #    0 <= timestamp[i] - timestamp[j] < width,
  # where timestamp[i] is the timestamp (index) associated with the
  # i-th element of data.  If align="left", then result[i] is a 
  # sum of those elements data[j] such that
  #    0 < timestamp[j] - timestamp[i] <= width.
  # And if align="center", then result[i] is a sum of those elements
  # data[j] such that
  #    -width/2 <= timestamp[j] - timestamp[i] < width/2
  #
  # When partial=TRUE and align="right", this function is equivalent to
  #   sapply(seq_along(data), function(i) FUN(data[time(data) <= time(data)[i] &
  #                                          time(data) > time(data)[i] - width], ...)
  # When partial=TRUE and align="left", this function is equivalent to
  #   sapply(seq_along(data), function(i) FUN(data[time(data) >= time(data)[i] &
  #                                          time(data) < time(data)[i] + width], ...)
  # When partial=TRUE and align="center", this function is equivalent to
  #   sapply(seq_along(data), function(i) FUN(data[time(data) > time(data)[i] - width/2 &
  #                                          time(data) <= time(data)[i] + width/2], ...)
  #
  # The principal advantage of this function over the above sapply formulation
  # is execution speed.  Even for small vectors, timeunit_apply may yield a factor
  # of ten speedup, and for larger vectors, the speedup may exceed a factor of 100.
  #
  # Returns a zoo-based vector of function results.

  if (length(data) == 0) {
    return (data)
  }
  
  if (!is(data, "zoo")) {
    stop ("data must be a vector of type zoo")
  }

  if (is.logical(partial) && !is.list(width)) {
    partial <- if (partial) 0 else width
  } else if (partial < 0) {
    partial <- width
  }

  datac <- coredata(data)
  TS <- time(data)
  N <- length(TS)
  align <- match.arg(align)

  # Compute the lower and upper indexes of each width time window.
  # Also computes upper and lower bounds for any partial windows.
  if (missing(include.endpoints)) {
    if (align == "left") {
      include.left <- TRUE;
      include.right <- FALSE;
    } else if (align == "center") {
      include.left <- FALSE;
      include.right <- TRUE;
    } else { # align == "right"
      include.left <- FALSE;
      include.right <- TRUE;
    }
  } else if (length(include.endpoints) > 1) {
    include.left <- include.endpoints[1]
    include.right <- include.endpoints[2]
  } else {
    include.left <- include.endpoints
    include.right <- include.endpoints
  }

  # For each index in 1:N, we compute the left and right indices of the window
  # that correspond to data[i].  After this block is complete, IXL is a vector
  # of left indices, and IXR is a vector of right indices.  Thus, for data[i],
  # FUN will be applied to the elements from IXL[i] through IXR[i].
  if (is.list(width)) {
    left_widths <- sapply (width, function(x) { x[1] })
    right_widths <- sapply (width, function(x) { x[2] })
    IXL <- timeunit_left_endpoint_index(TS, left_widths, include.left)
    IXR <- timeunit_right_endpoint_index(TS, right_widths, include.right)
    valid_window <- !is.na(IXL) & !is.na(IXR)
  } else if (align == "left") {
    IXL <- 1:N
    IXR <- timeunit_right_endpoint_index(TS, width, include.right)
    valid_window <- (IXR < N) | (TS[N] - TS[IXL] >= partial)
  } else if (align == "center") {
    width_half <- width * 0.5
    IXL <- timeunit_left_endpoint_index(TS, -width_half, include.left)
    IXR <- timeunit_right_endpoint_index(TS, width_half, include.right)
    valid_window <- ((1 < IXL) | (TS[IXR] - TS[1] >= partial)) &
      ((IXR < N) | (TS[N] - TS[IXL] >= partial))
  } else { # align == "right"
    IXL <- timeunit_left_endpoint_index(TS, -width, include.left)
    IXR <- 1:N
    valid_window <- (1 < IXL) | (TS[IXR] - TS[1] >= partial)
  }

  if (!any(valid_window)) {
    return (zoo(,))
  }

  # We now evaluate FUN on each valid window
  valid_indexes <- which(valid_window)
  if (is.null(dim(datac))) {
    VFUN <- function(k) FUN(datac[IXL[k]:IXR[k]], ...)
    if (!missing(FUN.VALUE)) {
      S <- vapply(valid_indexes, VFUN, FUN.VALUE)
    } else {
      S <- sapply(valid_indexes, VFUN)
    }
  } else if ((length(dim(datac)) == 2) && by.column) {
    VFUN <- function(k) apply(datac[IXL[k]:IXR[k],,drop=FALSE], 2, FUN, ...)
    S <- t(sapply(valid_indexes, VFUN))
  } else {
    VFUN <- function(k) FUN(datac[IXL[k]:IXR[k],,drop=FALSE], ...)
#    S <- t(sapply(valid_indexes, VFUN))
    S <- sapply(valid_indexes, VFUN)
    if (!is.null(dim(S))) {
      S <- t(S)
    }
  }

  ZS <- zoo(S, TS[valid_indexes])

  if ((partial > 0) && !missing(fill)) {
    ZS <- na.fill(merge.zoo(ZS, zoo(, TS)), fill)
  }

  ZS
}

timeunit_sum <- function(X, delta, align=c("center", "left", "right"), partial=FALSE, ...) {
  # Zoo Time Window Sum
  #
  # Constructs a zoo-based numeric vector of partial sums from X.
  # See the documentation of timeunit_apply for more details.

  align <- match.arg(align)
  timeunit_apply (X, delta, sum, align=align, partial=partial, ...)
}

########################################################################
#
# The following functions are commented out so as to simplify the
# interface provided by this module.  We leave timeunit_sum as an
# internal function because it is helpful in regression testing.
#

# timeunit_prod <- function(X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Product
#   #
#   # Constructs a zoo-based numeric vector of partial products from X.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, prod, align=align, partial=partial, ...)
# }

# timeunit_count <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Count
#   #
#   # Counts the number of non-NA values in each time window.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, function(z) { sum(!is.na(z)) },
#               FUN.VALUE=0, align=align, partial=partial, ...)
# }

# timeunit_min <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Min
#   #
#   # Finds the minimum value within each time window.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, min, align=align, partial=partial, ...)
# }

# timeunit_max <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Max
#   #
#   # Finds the maximum value within each time window.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, max, align=align, partial=partial, ...)
# }

# timeunit_any <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Any
#   #
#   # Finds the maximum value within each time window.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, any, align=align, partial=partial, ...)
# }

# timeunit_all <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window All
#   #
#   # Finds the maximum value within each time window.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, all, align=align, partial=partial, ...)
# }

# timeunit_mean <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Mean
#   #
#   # Computes the mean within each time window of size delta.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, mean, 
#               FUN.VALUE=0, align=align, partial=partial, ...)
# }

# timeunit_var <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Variance
#   #
#   # Computes the variance within each time window of size delta.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   timeunit_apply (X, delta, var, 
#               FUN.VALUE=0, align=align, partial=partial, ...)
# }

# timeunit_sd <- function (X, delta, align=c("center","left", "right"), partial=FALSE, ...) {
#   # Zoo Time Window Standard Deviation
#   #
#   # Computes the standard deviation within each time window of size delta.
#   # See the documentation of timeunit_apply for more details.
# 
#   align <- match.arg(align)
#   z <- timeunit_var(X, delta, align, partial, ...)
#   sqrt(z)
# }

########################################################################

test.timeunit <- function () {
  # Tests the timeunit_* functions in various ways.

  assert <- function (msg, X1, X2) checkEquals(X2, X1, paste(": ", msg, sep=""))

  # The following check is more stringent than the check that is
  # performed by RUnit's checkEquals function.
  
#  assert <- function (msg, X1, X2) {
#    if (!identical(X1, X2)) {
#      timeunit_test_fail(msg);
#    }
#  }

  library(RUnit)
  
  # Tests of timeunit_lag_index
  I1 <- c(1L,2L,3L,5L,8L,11L,12L,13L)
  assert("timeunit_lag_index test 1", timeunit_lag_index(1:5, 0), 1:5)
  assert("timeunit_lag_index test 2", timeunit_lag_index(1:5, -.5), 1:5)
  assert("timeunit_lag_index test 3", timeunit_lag_index(1:5, -.999), 1:5)
  assert("timeunit_lag_index test 4", timeunit_lag_index(1:5, -1), c(1L, 1L, 2L, 3L, 4L))
  assert("timeunit_lag_index test 5", timeunit_lag_index(1:5, -1.01), c(1L, 1L, 2L, 3L, 4L))
  assert("timeunit_lag_index test 6", timeunit_lag_index(1:5, -2), c(1L, 1L, 1L, 2L, 3L))
  assert("timeunit_lag_index test 7", timeunit_lag_index(1:5, -3.99), c(1L, 1L, 1L, 1L, 2L))
  assert("timeunit_lag_index test 8", timeunit_lag_index(1:5, -4), c(1L, 1L, 1L, 1L, 1L))

  assert("timeunit_lag_index test 9", timeunit_lag_index(1:5, .5), 1:5)
  assert("timeunit_lag_index test 10", timeunit_lag_index(1:5, .999), 1:5)
  assert("timeunit_lag_index test 11", timeunit_lag_index(1:5, 1), c(2L, 3L, 4L, 5L, 5L))
  assert("timeunit_lag_index test 12", timeunit_lag_index(1:5, 1.01), c(2L, 3L, 4L, 5L, 5L))
  assert("timeunit_lag_index test 13", timeunit_lag_index(1:5, 2), c(3L, 4L, 5L, 5L, 5L))
  assert("timeunit_lag_index test 14", timeunit_lag_index(1:5, 3.99), c(4L, 5L, 5L, 5L, 5L))
  assert("timeunit_lag_index test 15", timeunit_lag_index(1:5, 4), c(5L, 5L, 5L, 5L, 5L))

  assert("timeunit_lag_index test 16", timeunit_lag_index(NULL, 1), integer(0))
  assert("timeunit_lag_index test 17", timeunit_lag_index(10,1), 1L)
  assert("timeunit_lag_index test 18", timeunit_lag_index(10,-1), 1L)
  assert("timeunit_lag_index test 19", timeunit_lag_index(I1, 2), c(3L,3L,4L,4L,5L,8L,8L,8L))
  assert("timeunit_lag_index test 20", timeunit_lag_index(I1, -2), c(1L, 1L, 1L, 3L, 5L, 6L, 6L, 6L))
  assert("timeunit_lag_index test 21", timeunit_lag_index(I1, 0), 1:length(I1))

  D1 <- as.Date(c("1970-01-01", "1970-01-02", "1970-01-03", "1970-01-05", "1970-01-07", "1970-01-10",
                  "1970-01-12", "1970-01-14", "1970-01-15", "1970-01-16"))

  assert("timeunit_lag_index test 7", timeunit_lag_index(D1, 1), c(2L, 3L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 10L))
  assert("timeunit_lag_index test 8", timeunit_lag_index(D1, -2), c(1L, 1L, 1L, 3L, 4L, 6L, 6L, 7L, 8L, 8L))

  # Tests of timeunit_lag
  D2 <- D1[1:5]
  X2 <- (5:1)*10
  Z2 <- zoo(X2, order.by = D2)
  assert("timeunit_lag test 1", timeunit_lag(Z2, 1, partial=TRUE), zoo(c(40, 30, 30, 20, 10), D2))
  assert("timeunit_lag test 2", timeunit_lag(Z2, -1, result.type="indexes", partial=TRUE), c(1L, 1L, 2L, 4L, 5L))
  assert("timeunit_lag test 3", timeunit_lag(Z2, -1, result.type="times", partial=TRUE),
                            as.Date(c("1970-01-01", "1970-01-01", "1970-01-02",
                                      "1970-01-05", "1970-01-07")))
  assert("timeunit_lag test 4", timeunit_lag(Z2, 1, partial=FALSE), zoo(c(40, 30, 30, 20), D2[1:4]))
  assert("timeunit_lag test 5", timeunit_lag(Z2, -1, partial=FALSE), zoo(c(50, 40, 20, 10), D2[2:5]))
  assert("timeunit_lag test 6", timeunit_lag(Z2, 1, na.pad=TRUE), zoo(c(40, 30, 30, 20, NA), D2))
  assert("timeunit_lag test 7", timeunit_lag(Z2, -1, na.pad=TRUE, partial=TRUE), zoo(c(NA, 50, 40, 20, 10), D2))
  assert("timeunit_lag test 8", timeunit_lag(Z2, -1, na.pad=TRUE, partial=FALSE), zoo(c(NA, 50, 40, 20, 10), D2))
#  assert("timeunit_lag test 8", timeunit_lag(Z2, -1, partial=FALSE, result.type="abc"), zoo(c(50, 40, 20, 10), D2[2:5]));

  # Tests of timeunit_findfirsts
  assert("timeunit_findfirsts test 1", timeunit_findfirsts(c(1,1,2,2,2,3,4,4,5,5)),
         c(1L, 1L, 3L, 3L, 3L, 6L, 7L, 7L, 9L, 9L))

  # Tests of timeunit_left_endpoint_index
  assert("timeunit_left_endpoint_index test 1", timeunit_left_endpoint_index(integer(0), 0), integer(0))
  assert("timeunit_left_endpoint_index test 2", timeunit_left_endpoint_index(99, 0), 1L)
  assert("timeunit_left_endpoint_index test 3", timeunit_left_endpoint_index(99, -1), 1L)
  assert("timeunit_left_endpoint_index test 4", timeunit_left_endpoint_index(99, 1), NA_integer_)
  assert("timeunit_left_endpoint_index test 5", timeunit_left_endpoint_index(99, 0, FALSE), NA_integer_)

  assert("timeunit_left_endpoint_index test 6", timeunit_left_endpoint_index(4:6, 0), 1:3)
  assert("timeunit_left_endpoint_index test 7", timeunit_left_endpoint_index(4:6, 0, FALSE), c(2L,3L,NA))
  assert("timeunit_left_endpoint_index test 8", timeunit_left_endpoint_index(4:6, 0.5), c(2L, 3L, NA))
  assert("timeunit_left_endpoint_index test 9", timeunit_left_endpoint_index(4:6, 0.5, FALSE), c(2L, 3L, NA))
  assert("timeunit_left_endpoint_index test 10", timeunit_left_endpoint_index(4:6, 1), c(2L, 3L, NA))
  assert("timeunit_left_endpoint_index test 11", timeunit_left_endpoint_index(4:6, 1, FALSE), c(3L, NA, NA))
  assert("timeunit_left_endpoint_index test 12", timeunit_left_endpoint_index(4:6, -0.5), 1:3)
  assert("timeunit_left_endpoint_index test 13", timeunit_left_endpoint_index(4:6, -0.5, FALSE), 1:3)
  assert("timeunit_left_endpoint_index test 14", timeunit_left_endpoint_index(4:6, -1), c(1L, 1L, 2L))
  assert("timeunit_left_endpoint_index test 15", timeunit_left_endpoint_index(4:6, -1, FALSE), 1:3)

  assert("timeunit_left_endpoint_index test 16", timeunit_left_endpoint_index(c(2,2,2,4,4,4,6,6,6), -1),
         c(1L, 1L, 1L, 4L, 4L, 4L, 7L, 7L, 7L))
  assert("timeunit_left_endpoint_index test 17", timeunit_left_endpoint_index(c(2,2,2,4,4,4,6,6,6), 1),
         c(4L, 4L, 4L, 7L, 7L, 7L, NA, NA, NA))

  # Tests of timeunit_right_endpoint_index
  assert("timeunit_right_endpoint_index test 1", timeunit_right_endpoint_index(integer(0), 0), integer(0))
  assert("timeunit_right_endpoint_index test 2", timeunit_right_endpoint_index(99, 0), 1L)
  assert("timeunit_right_endpoint_index test 3", timeunit_right_endpoint_index(99, -1), NA_integer_)
  assert("timeunit_right_endpoint_index test 4", timeunit_right_endpoint_index(99, 1), 1L)
  assert("timeunit_right_endpoint_index test 5", timeunit_right_endpoint_index(99, 0, FALSE), NA_integer_)

  assert("timeunit_right_endpoint_index test 6", timeunit_right_endpoint_index(4:6, 0), 1:3)
  assert("timeunit_right_endpoint_index test 7", timeunit_right_endpoint_index(4:6, 0, FALSE), c(NA, 1L, 2L))
  assert("timeunit_right_endpoint_index test 8", timeunit_right_endpoint_index(4:6, 0.5), 1:3)
  assert("timeunit_right_endpoint_index test 9", timeunit_right_endpoint_index(4:6, 0.5, FALSE), 1:3)
  assert("timeunit_right_endpoint_index test 10", timeunit_right_endpoint_index(4:6, 1), c(2L, 3L, 3L))
  assert("timeunit_right_endpoint_index test 11", timeunit_right_endpoint_index(4:6, 1, FALSE), 1:3)
  assert("timeunit_right_endpoint_index test 12", timeunit_right_endpoint_index(4:6, -0.5), c(NA, 1L, 2L))
  assert("timeunit_right_endpoint_index test 13", timeunit_right_endpoint_index(4:6, -0.5, FALSE), c(NA, 1L, 2L))
  assert("timeunit_right_endpoint_index test 14", timeunit_right_endpoint_index(4:6, -1), c(NA, 1L, 2L))
  assert("timeunit_right_endpoint_index test 15", timeunit_right_endpoint_index(4:6, -1, FALSE), c(NA, NA, 1L))

  assert("timeunit_right_endpoint_index test 16", timeunit_right_endpoint_index(c(2,2,2,4,4,4,6,6,6), -1),
         c(NA, NA, NA, 3L, 3L, 3L, 6L, 6L, 6L))
  assert("timeunit_right_endpoint_index test 17", timeunit_right_endpoint_index(c(2,2,2,4,4,4,6,6,6), 1),
         c(3L, 3L, 3L, 6L, 6L, 6L, 9L, 9L, 9L))


  
  # Tests of timeunit_sum
  Z3 <- zoo(1:5, 1:5)
  Z4 <- Z3;
  Z4[3] <- NA;
  assert("timeunit_sum test 1", timeunit_sum(Z3, 1), zoo(c(2L,3L,4L), 2:4))
  assert("timeunit_sum test 2", timeunit_sum(Z3, 1.5), zoo(c(2L,3L,4L), 2:4))
  assert("timeunit_sum test 3", timeunit_sum(Z3, 2), zoo(c(5L, 7L), 2:3))
  assert("timeunit_sum test 4", timeunit_sum(Z3, 2.5), zoo(9L, 3L))
  assert("timeunit_sum test 5", timeunit_sum(Z3, 3), zoo(9L, 3L))
  assert("timeunit_sum test 6", timeunit_sum(Z3, 4), zoo(,))
  assert("timeunit_sum test 7", timeunit_sum(Z3, 5), zoo(,))

  assert("timeunit_sum test 8", timeunit_sum(Z3, 1, partial=TRUE), zoo(1:5, 1:5))
  assert("timeunit_sum test 9", timeunit_sum(Z3, 2, partial=TRUE), zoo(c(3L, 5L, 7L, 9L, 5L), 1:5))
  assert("timeunit_sum test 10", timeunit_sum(Z3, 4, partial=TRUE), zoo(c(6L, 10L, 14L, 12L, 9L), 1:5))
  assert("timeunit_sum test 11", timeunit_sum(Z3, 8, partial=TRUE), zoo(c(15L, 15L, 15L, 15L, 14L), 1:5))
  
  assert("timeunit_sum test 12", timeunit_sum(Z3, 1, partial=-1), zoo(c(2L,3L,4L), 2:4))
  assert("timeunit_sum test 13", timeunit_sum(Z3, 1, partial=0), zoo(1:5, 1:5))
  assert("timeunit_sum test 14", timeunit_sum(Z3, 1, partial=1), zoo(c(2L,3L,4L), 2:4))
  assert("timeunit_sum test 15", timeunit_sum(Z3, 1, partial=3), zoo(c(2L,3L,4L), 2:4))
  assert("timeunit_sum test 16", timeunit_sum(Z3, 1, partial=5), zoo(c(2L,3L,4L), 2:4))
  assert("timeunit_sum test 17", timeunit_sum(Z3, 1, align="left", partial=2), zoo(1:4, 1:4))
  assert("timeunit_sum test 18", timeunit_sum(Z3, 1, align="left", partial=2, include.endpoints=c(TRUE,TRUE)),
         zoo(c(3L,5L,7L), 1:3))
  assert("timeunit_sum test 19", timeunit_sum(Z3, 1, align="right", partial=2), zoo(2:5, 2:5))
  assert("timeunit_sum test 20", timeunit_sum(Z3, 1, align="right", partial=2, include.endpoints=c(TRUE,TRUE)),
         zoo(c(5L,7L,9L), 3:5))

  assert("timeunit_sum test 21", timeunit_sum(Z3, .5, align="right"), zoo(2:5, 2:5))
  assert("timeunit_sum test 22", timeunit_sum(Z3, 1, align="right", include.endpoints=TRUE),
         zoo(c(3L, 5L, 7L, 9L), 2:5))
  assert("timeunit_sum test 23", timeunit_sum(Z3, 2, align="right"), zoo(c(5L, 7L, 9L), 3:5))
  assert("timeunit_sum test 24", timeunit_sum(Z3, 4, align="right"), zoo(14L, 5L))
  assert("timeunit_sum test 25", timeunit_sum(Z3, 5, align="right"), zoo(,))

  assert("timeunit_sum test 26", timeunit_sum(Z3, 1, partial=TRUE, align="right", include.endpoints=TRUE),
         zoo(as.integer(c(1,3,5,7,9)), 1:5))
  assert("timeunit_sum test 27", timeunit_sum(Z3, 1, align="left"), zoo(1:4, 1:4))
  assert("timeunit_sum test 28", timeunit_sum(Z3, 1, align="left", partial=TRUE, include.endpoints=TRUE),
         zoo(as.integer(c(3,5,7,9,5)), 1:5))
  assert("timeunit_sum test 29", timeunit_sum(Z4, 2, align="right"), zoo(c(NA,NA,9L), 3:5))
  assert("timeunit_sum test 30", timeunit_sum(Z4, 2, align="right", na.rm=TRUE), zoo(c(2L,4L,9L), 3:5))

  assert("timeunit_sum test 31", timeunit_sum(Z3, 1, fill=-(1:3)), zoo(c(-1,2,3,4,-3), 1:5))
  
#  # Tests for other summary functions
#  assert ("timeunit_prod test 1", timeunit_prod(Z3, 2), zoo(c(6,24,60), 2:4))
#  assert ("timeunit_count test 1", timeunit_count(Z4, 1), zoo(c(1,0,1), 2:4))
#  assert ("timeunit_mean test 1", timeunit_mean(Z3, 1, align="right"), zoo(c(1.5,2.5,3.5,4.5), 2:5))
#  assert ("timeunit_var test 1", timeunit_var(Z3, 2, align="left"), zoo(c(1,1,1), 1:3))
#  assert ("timeunit_sd test 1", timeunit_sd(Z3, 3, align="left"), zoo(rep(sqrt(5.0/3.0),2), 1:2))

#  Z5 <- zoo(c(3, 2, 1, 0, 1, 2, 3), 1:7)
#  assert ("timeunit_min test 1", timeunit_min(Z5, 2, align="right"), zoo(c(1,0,0,0,1), 3:7))
#  assert ("timeunit_max test 1", timeunit_max(Z5, 2, align="right"), zoo(c(3,2,1,2,3), 3:7))

#  Z6 <- zoo(c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE), 1:6)
#  assert ("timeunit_any test 1", timeunit_any(Z6, 1, align="right"), zoo(c(TRUE, TRUE, TRUE, TRUE, FALSE), 2:6))
#  assert ("timeunit_all test 1", timeunit_all(Z6, 1, align="right"), zoo(c(FALSE, FALSE, TRUE, FALSE, FALSE), 2:6))

  # Tests of timeunit_apply
  Z7 <- zoo(cbind((1:10)*2, (10:1)*2+1), as.Date("1970-01-01")+1:10)
  R7 <- cbind((1:8)*2, (3:10)*2, (8:1)*2+1, (10:3)*2+1)
  F7 <- function(M) { c(min(M[,1]), max(M[,1]), min(M[,2]), max(M[,2]))}
  assert ("timeunit_apply test 1", timeunit_apply(Z7, 2, F7, align="right",
                                                  by.column=FALSE, include.endpoints=TRUE),
          zoo(R7, as.Date("1970-01-01")+(3:10)))
  assert ("timeunit_apply test 2", timeunit_apply(Z7, 2, F7, align="left", by.column=FALSE,
                                                  include.endpoints=TRUE),
          zoo(R7, as.Date("1970-01-01")+(1:8)))

  Z8 <- zoo(c(NA, 2, NA, 3, NA, 4, NA), 1:7)
  F8 <- function(x) x[1]
  assert ("timeunit_apply test 3", timeunit_apply(Z8, 1, F8, fill=-(1:3)),
          zoo(c(-1,2,-2,3,-2,4,-3), 1:7))
  assert ("timeunit_apply test 4", timeunit_apply(Z8, 2, F8, fill=-(1:3), align="left"),
          zoo(c(-1,2,-2,3,-3,-3,-3), 1:7))

  # Tests of varying width parameter to timeunit_apply
  assert ("timeunit_apply width test 1", timeunit_apply(zoo(1:5, 1:5), 5:1, sum, align="right", partial=TRUE),
          zoo(c(1L, 3L, 6L, 7L, 5L), 1:5))
  assert ("timeunit_apply width test 2", timeunit_apply(zoo(1:5, 1:5),
                                                        list(c(0,1), c(-1,0), c(0,1), c(-1,0), c(-2,0)), sum),
          zoo(c(2L, 2L, 4L, 4L, 9L), 1:5))
  assert ("timeunit_apply width test 2", timeunit_apply(zoo(1:5, 1:5),
                                                        list(c(0,1), c(-1,0), c(0,1), c(-1,0), c(-2,0)), sum,
                                                        include.endpoints=TRUE),
          zoo(c(3L, 3L, 7L, 7L, 12L), 1:5))
  
  
  
  "pass";
}

timeunit_compare_times <- function (W=20, N=c(100, 200, 400, 600, 800, 1000, 2000, 4000, 8000, 16000),
                           index.base=as.Date("1970-01-01")) {
  # Prints a table of execution times of various timeunit_* functions, as compared to sapply and rollapply
  sumfuns <- c(sapply = function(z) { zoo(sapply(seq_along(z), function(i) sum(z[time(z) <= time(z)[i] & time(z) >= time(z)[i] - W])), order.by=index(z)) },
            rollapply = function(z) { rollapply(z, W+1, sum, partial=TRUE, align="right") },
            null_func = function(z) { z; },
            almost_null = function(z) { zoo(coredata(z)+.01, order.by=index(z)-W) },
            timeunit_lag = function(z) { timeunit_lag(z, W) },
            timeunit_sum = function(z) { timeunit_sum(z, W, partial=TRUE) }
#            timeunit_count = function(z) { timeunit_count(z, W, partial=TRUE) },
#            timeunit_mean = function(z) { timeunit_mean(z, W, partial=TRUE) },
#            timeunit_var = function(z) { timeunit_var(z, W, partial=TRUE) },
#            timeunit_sd = function(z) { timeunit_sd(z, W, partial=TRUE) },
#            timeunit_min = function(z) { timeunit_min(z, W, partial=TRUE) },
#            timeunit_max = function(z) { timeunit_max(z, W, partial=TRUE) }
            )

  zvec <- function(k) { zoo(runif(k), order.by=index.base + (1:k))}
  ftime <- function (f, N) { sapply(N, function(k) { system.time(f(zvec(k)))["user.self"] }) }
  E <- sapply (sumfuns, function(F) { ftime(F, N)})
  rownames(E) <- N;

  E;
}

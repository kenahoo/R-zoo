# rollmean, rollmax, rollmedian (, rollmad) based on code posted by Jarek Tuszynski at
# https://www.stat.math.ethz.ch/pipermail/r-help/2004-October/057363.html
# ToDo: rollmad, currently rollapply() can be used

rollmean <- function(x, k, na.pad = FALSE, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...)
  UseMethod("rollmean")

rollmean.default <- function(x, k, na.pad = FALSE, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...)
{

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  x <- unclass(x)
  n <- length(x) 
  y <- x[k:n] - x[c(1, seq_len(n-k))] # difference from previous
  y[1] <- sum(x[1:k])		 # find the first
  # apply precomputed differencest sum
  rval <- cumsum(y)/k
  if (rule == "NA") {
    rval <- switch(match.arg(align),
      "left" = { c(rval, rep(NA, k-1)) },
      "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
      "right" = { c(rep(NA, k-1), rval) })
  }
  return(rval)
}

rollmean.zoo <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...)
{ 
  stopifnot(k <= NROW(x))

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  index.x <- index(x)
  if(rule != "NA") {
    n <- length(index.x)
    ix <- switch(match.arg(align),
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })
    index.x <- index.x[ix]
  }
  if(length(dim(x)) == 0) 
    return(zoo(rollmean.default(coredata(x), k, rule = rule, align = align), index.x, attr(x, "frequency")))
  else
    return(zoo(apply(coredata(x), 2, rollmean.default, k=k, rule=rule, align=align),
        index.x, attr(x, "frequency")))
}

rollmean.ts <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...) {

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  as.ts(rollmean(as.zoo(x), k = k, rule = rule, align = align, ...))
}


rollmax <- function(x, k, na.pad,
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...) 
  UseMethod("rollmax")

rollmax.default <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...)
{

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  n <- length(x) 
  rval <- rep(0, n) 
  a <- 0
  for (i in k:n) {
  rval[i] <- if (is.na(a) || is.na(rval[i=1]) || a==rval[i-1]) 
      max(x[(i-k+1):i]) # calculate max of window
  else 
      max(rval[i-1], x[i]); # max of window = rval[i-1] 
  a <- x[i-k+1] # point that will be removed from window
  }
  rval <- rval[-seq(k-1)]
  if (rule == "NA") {
    rval <- switch(match.arg(align),
      "left" = { c(rval, rep(NA, k-1)) },
      "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
      "right" = { c(rep(NA, k-1), rval) })
  }
  return(rval)
} 

rollmax.zoo <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...)
{ 
  stopifnot(k <= NROW(x))

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  index.x <- index(x)
  if(rule != "NA") {
    n <- length(index.x)
    ix <- switch(match.arg(align),
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })
    index.x <- index.x[ix]
  }
  if (length(dim(x)) == 0) 
    return(zoo(rollmax.default(coredata(x), k, rule = rule, align = align), index.x, attr(x, "frequency")))
  else {
    s <- apply(coredata(x), 2, rollmax.default, k=k, rule = rule, align=align)
	if (length(s) > 1 && length(index.x) == 1) {
		s <- matrix(s, 1)
		colnames(s) <- colnames(x)
	}
    return(zoo(s, index.x, attr(x, "frequency")))
  }
}

rollmax.ts <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...) {

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  as.ts(rollmax(as.zoo(x), k = k, rule = rule, align = align, ...))
}


rollmedian <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...)
  UseMethod("rollmedian")

rollmedian.default <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...)
{
  ## interfaces runmed from `stats'
  stopifnot(k <= length(x), k %% 2 == 1)

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  n <- length(x)
  m <- k %/% 2
  rval <- runmed(x, k, ...)
  attr(rval, "k") <- NULL
  rval <- rval[-c(1:m, (n-m+1):n)]
  if (rule == "NA") {
    rval <- switch(match.arg(align),
      "left" = { c(rval, rep(NA, k-1)) },
      "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
      "right" = { c(rep(NA, k-1), rval) })
  }
  return(rval)
}

rollmedian.zoo <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...) { 
  stopifnot(all(!is.na(x)), k <= NROW(x), k %% 2 == 1)

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  # todo:
  # rather than abort we should do a simple loop to get the medians
  # for those columns with NAs.
  index.x <- index(x)
  m <- k %/% 2
  n <- NROW(x)
  align <- match.arg(align)
  
  if(rule != "NA") {
    n <- length(index.x)
    ix <- switch(align,
      "left" = { 1:(n-k+1) },
      "center" = { floor((1+k)/2):ceiling(n-k/2) },
      "right" = { k:n })
    index.x <- index.x[ix]
  }
  
  rollmedian0 <- function(x, k, rule, ...) {
    x <- runmed(x, k, ...)[-c(seq(m),seq(to=n,len=m))]
    if (rule == "NA") {
      x <- switch(align,
        "left" = { c(x, rep(NA, k-1)) },
        "center" = { c(rep(NA, floor((k-1)/2)), x, rep(NA, ceiling((k-1)/2))) },
        "right" = { c(rep(NA, k-1), x) })
    }
    return(x)
  }
  if (length(dim(x)) == 0)
    return(zoo(rollmedian0(coredata(x), k, rule = rule, ...), index.x,
      attr(x, "frequency")))
  else {
    s <- apply(coredata(x), 2, rollmedian0, k = k, rule = rule, ...)
	if (length(s) > 1 && length(index.x) == 1) {
		s <- matrix(s, 1)
		colnames(s) <- colnames(x)
	}
    return(zoo(s, index.x, attr(x, "frequency")))
  }
}

rollmedian.ts <- function(x, k, na.pad, 
	rule = c("drop", "NA", "extend", "partial"), 
	align = c("center", "left", "right"), ...) {

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

  as.ts(rollmedian(as.zoo(x), k = k, rule = rule, align = align, ...))

}



if (FALSE) {
make.unique.incr <- function(x, incr, ...) {
	UseMethod("make.unique.incr")
}

make.unique.incr.Date <- function(x, incr, ...) {
	make.unique.incr.default(x, incr = 1, ...)
}

make.unique.incr.yearmon <- function(x, incr, ...) {
	make.unique.incr.default(x, incr = 1/12, ...)
}

make.unique.incr.yearqtr <- function(x, incr, ...) {
	make.unique.incr.default(x, incr = 1/4, ...)
}

make.unique.incr.default <- function(x, incr, quantile = 1, ...) {
	n <- length(x)
	if (n < 2) return(x)
	o <- ORDER(x)
	xo <- x[o]
	d <- as.numeric(xo[-1] - xo[-n])
	d <- d[d != 0]
	stopifnot(length(d) > 0)
	mind <- min(d)
	if (missing(incr)) incr <- mind / n
	FUN = function(x) {
		n <- length(x)
		# arg of ceiling is a convex combination of 1-n and 0
		x[1] + incr * seq(ceiling((1-n) * quantile), length = n)
	}
	y <- ave(x, x, FUN = FUN)
	if (!identical(ORDER(y), o)) stop("Algorithm failed", call. = TRUE)
	y
}
}

make.unique.approx <- function(x, ...) {
	UseMethod("make.unique.approx")
}

make.unique.approx.default <- function(x, quantile = 1, ...) {
	o <- ORDER(x)
	xo <- x[o]
	d <- as.numeric(xo - xo[1])
	y <- ave(d, d, FUN = function(x) {
			n <- length(x)
			if (n > 1) replace(x, -ceiling(n * quantile + 1 - quantile), NA)
			else x
	})
	y <- na.approx(y)
	x[o] <- xo[1] + y
	x
}


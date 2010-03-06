
na.spline <- function(object, ...) UseMethod("na.spline")

na.spline.zoo <- function(object, x = index(object), na.rm = TRUE, xout = x, order.by = if (missing(xout) || is.null(xout)) x else if (identical(class(xout), class(index(object)))) xout else index(object), along, ...) {

	if (!missing(along)) {
		warning("along to be deprecated - use x instead")
		if (missing(x)) x <- along
	}

	if (missing(xout) || is.null(xout) || identical(xout, index(object))) {
		result <- object
	} else {
		object.x <- object
		index(object.x) <- x
		objectm <- merge(object.x, zoo(, xout))
		if (length(dim(objectm)) == 2) colnames(objectm) <- colnames(object)
		result <- window(objectm, index = xout)
	}
	result[] <- na.spline.default(object, x = x, na.rm = FALSE, 
		xout = xout, ...)
	if (!missing(order.by) && !is.null(order.by)) index(result) <- order.by

    if (na.rm) {
            result <- na.omit(result)
            attr(result, "na.action") <- NULL
    }

	result

}

na.spline.zooreg <- function(object, ...) {
	object. <- structure(object, class = setdiff(class(object), "zooreg"))
	as.zooreg(na.spline(object., ...))
}


na.spline.default <- function(object, x = index(object), na.rm = TRUE, xout = x, maxgap = Inf, along, ...) {

	if (!missing(along)) {
		warning("along to be deprecated - use x instead")
		if (missing(x)) x <- along
	}

	na.spline.vec <- function(x, y, xout = x, ...) {
		na <- is.na(y)
		yf <- splinefun(x[!na], y[!na], ...)(xout)
		if (maxgap < length(y)) {
		    ## construct version of y with only gaps > maxgap
		    ygap <- .fill_short_gaps(y, seq_along(y), maxgap = maxgap)
		    ## construct y values at 'x', keeping NAs from ygap
		    ## (spline() does not allow NAs to be propagated)
		    ix <- spline(x, seq_along(y), ...)(xout)
		    yx <- ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]),
					NA, yf)
		    yx
		} else {
		    yf
		}
	}

	if (!identical(length(x), length(index(object)))) {
		stop("x and index must have the same length")
	}
	x. <- as.numeric(x)
	if (missing(xout) || is.null(xout)) xout <- x.
	xout. <- as.numeric(xout)
	object. <- coredata(object)

	result <- if (length(dim(object.)) < 2) {
		na.spline.vec(x., coredata(object.), xout = xout., ...)
	} else {
		apply(coredata(object.), 2, na.spline.vec, x = x., xout = xout., ...)
	}

    if (na.rm) {
		result <- na.omit(result)
        attr(result, "na.action") <- NULL
	}

	result

}

na.spline.ts <- function(object, ...) {
	as.ts(na.spline(as.zoo(object), ...))
}


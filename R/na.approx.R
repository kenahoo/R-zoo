na.approx <- function(object, ...) UseMethod("na.approx")

na.approx.zoo <- function(object, along = index(object), na.rm = TRUE, ...) {

	if (missing(along)) {
		result <- object
		result[] <- na.approx.default(object, na.rm = FALSE, ...)
	} else {

		objectm <- merge(object, zoo(, along))
		if (length(dim(objectm)) == 2) colnames(objectm) <- colnames(object)

		result <- window(objectm, index = along)
		result[] <- na.approx.default(objectm, along = along, na.rm = FALSE, ...)
	}

    if (na.rm) {
            result <- na.omit(result)
            attr(result, "na.action") <- NULL
    }

	result

}

na.approx.zooreg <- function(object, along = index(object), na.rm = TRUE, ...) {
	object. <- structure(object, class = setdiff(class(object), "zooreg"))
	as.zooreg(na.approx(object., along = along, na.rm = na.rm, ...))
}

na.approx.default <- function(object, along = index(object), na.rm = TRUE, maxgap = Inf, x = time(object), ...) {

	na.approx.vec <- function(x, y, along, ...) {
		na <- is.na(y)
		yf <- approx(x[!na], y[!na], along, ...)$y
		if (maxgap < length(y)) {
		    ## construct version of y with only gaps > maxgap
		    ygap <- .fill_short_gaps(y, seq_along(y), maxgap = maxgap)
		    ## construct y values at 'along', keeping NAs from ygap
		    ## (approx() does not allow NAs to be propagated)
		    ialong <- approx(x, seq_along(y), along, ...)$y
		    yalong <- ifelse(is.na(ygap[floor(ialong)] + ygap[ceiling(ialong)]),
					NA, yf)
		    yalong
		} else {
		    yf
		}
	}

	x. <- as.numeric(x)
	along. <- as.numeric(along)
	object. <- coredata(object)

	result <- if (length(dim(object.)) < 2) {
		na.approx.vec(x., coredata(object.), along = along., ...)
	} else {
		apply(coredata(object.), 2, na.approx.vec, x = x., along = along., ...)
	}

    if (na.rm) {
		result <- na.omit(result)
        attr(result, "na.action") <- NULL
	}

	result

}

na.approx.ts <- function(object, ...) {
	as.ts(na.approx(as.zoo(object), ...))
}

## x = series with gaps
## fill = same series with filled gaps
.fill_short_gaps <- function(x, fill, maxgap)
{
    if (maxgap <= 0)
        return(x)
    if (maxgap >= length(x))
        return(fill) #return(ifelse(is.na(x), fill, x))
    naruns <- rle(is.na(x))
    naruns$values[naruns$lengths > maxgap] <- FALSE
    naok <- inverse.rle(naruns)
    ifelse(naok, fill, x)
}


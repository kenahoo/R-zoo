na.approx <- function(object, ...) UseMethod("na.approx")

na.approx.zoo <- function(object, along = index(object), na.rm = TRUE, ...) {

	objectm <- merge(object, zoo(, along))
	if (length(dim(objectm)) == 2) colnames(objectm) <- colnames(object)
	result <- window(objectm, index = along)
	result[] <- na.approx.default(coredata(objectm), along = as.numeric(along), 
		na.rm = FALSE, ...)

    if (na.rm) {
            out <- na.omit(result)
            attr(out, "na.action") <- NULL
            out
    } else result
}

na.approx.zooreg <- function(object, along = index(object), na.rm = TRUE, ...) {
	object0 <- structure(object, class = setdiff(class(object), "zooreg"))
	as.zooreg(na.approx(object0, along = along, na.rm = na.rm, ...), 
		frequency = frequency(object))
}

na.approx.default <- function(object, along = index(object), na.rm = TRUE, maxgap = Inf, ...) {

	na.approx.vec <- function(x, y, along, ...) {
		na <- is.na(y)
		yf <- approx(x[!na], y[!na], along, ...)$y
		if (maxgap < length(y)) {
		    yalong <- approx(x, y, along, ...)$y
		    .fill_short_gaps(yalong, yf, maxgap = maxgap)
		} else {
		    yf
		}
	}

	along.numeric <- as.numeric(along)
	x <- as.numeric(index(object))
	stopifnot(all(along.numeric %in% x))

	objcore <- coredata(object)

	result <- if (missing(along)) { object
	} else if (length(dim(objcore)) < 2) {
		object[x %in% along.numeric]
	} else object[x %in% along.numeric, ]

	result[] <- if (length(dim(objcore)) < 2) {
		na.approx.vec(x = x, y = objcore, along = along.numeric, ...)
	} else {
		apply(objcore, 2, na.approx.vec, x = x, along = along.numeric, ...)
	}

    if (na.rm) {
            out <- na.omit(result)
            attr(out, "na.action") <- NULL
            out
    } else result
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


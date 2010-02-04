na.spline <- function(object, ...) UseMethod("na.spline")

na.spline <- function(object, ...) UseMethod("na.spline")

na.spline.zoo <- function(object, along = index(object), na.rm = TRUE, ...) {
	result <- na.spline.default(object, along = along, na.rm = FALSE, ...)
	result <- zoo(result, along)
    if (na.rm) {
            out <- na.omit(result)
            attr(out, "na.action") <- NULL
            out
    } else result
}

na.spline.zooreg <- function(object, along = index(object), na.rm = TRUE, ...) {
	object0 <- structure(object, class = setdiff(class(object), "zooreg"))
	as.zooreg(na.spline(object0, along = along, na.rm = na.rm, ...), 
		frequency = frequency(object))
}

# interpolates object along along which defaults to index(object)
# along has to be numeric, is otherwise coerced
na.spline.default <- function(object, along = index(object), na.rm = TRUE, maxGapFilled = Inf, ...)
{
	along.numeric <- as.numeric(along)
	object.index <- as.numeric(time(object))
	na.spline.0 <- function(y) {
		na <- is.na(y)
		if(all(!na)) return(y)
		yf <- splinefun(object.index[!na], y[!na], ...)(along)
		fillShortGaps(y, yf, maxGapFilled = maxGapFilled)
	}

        result <- structure(if (length(dim(object)) == 0) na.spline.0(object)
        	else apply(object, 2, na.spline.0), class = class(object))
        if (na.rm) {
            out <- na.omit(result)
            attr(out, "na.action") <- NULL
            out
        } else result
}



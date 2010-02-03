na.spline <- function(object, ...) UseMethod("na.spline")

# interpolates object along along which defaults to index(object)
# along has to be numeric, is otherwise coerced
na.spline.default <- function(object, along = index(object), na.rm = TRUE, 
                              maxGapFilled = Inf, ...)
{
	object.0 <- if (missing(along)) object else window(object, index = along)
	along <- as.numeric(along)
	along.index <- as.numeric(time(object))
	na.spline.0 <- function(y) {
		na <- is.na(y)
		if(all(!na)) return(y)
		yf <- splinefun(along.index[!na], y[!na], ...)(along)
                fillShortGaps(y, yf, maxGapFilled = maxGapFilled)
	}

        object.0[] <- if (length(dim(object)) == 0) na.spline.0(object)
        	else apply(object, 2, na.spline.0)
        if (na.rm) na.omit(object.0) else object.0
}


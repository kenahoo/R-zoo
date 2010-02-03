na.approx <- function(object, ...) UseMethod("na.approx")

# interpolates object along along which defaults to index(object)
# along has to be numeric, is otherwise coerced
na.approx.default <- function(object, along = index(object), na.rm = TRUE, 
                              maxGapFilled = Inf, ...)
{
	object.0 <- if (missing(along)) object else window(object, index = along)
	along <- as.numeric(along)
	along.index <- as.numeric(time(object))
	na.approx.0 <- function(y) {
		na <- is.na(y)
		if(all(!na)) return(y)
		# y[na] <- approx(along.index[!na], y[!na], along[na], ...)$y
		yf <- approx(along.index[!na], y[!na], along, ...)$y
                fillShortGaps(y, yf, maxGapFilled = maxGapFilled)
	}

        object.0[] <- if (length(dim(object)) == 0) na.approx.0(object)
        	else apply(object, 2, na.approx.0)
        if (na.rm) {
            out <- na.omit(object.0)
            attr(out, "na.action") <- NULL
            out
        } else object.0
}

fillShortGaps <- function(x, fill, maxGapFilled = 1)
{
    if (maxGapFilled <= 0)
        return(x)
    if (maxGapFilled >= length(x))
        return(fill) #return(ifelse(is.na(x), fill, x))
    naruns <- rle(is.na(x))
    naruns$values[naruns$lengths > maxGapFilled] <- FALSE
    naok <- inverse.rle(naruns)
    ifelse(naok, fill, x)
}

na.approx <- function(object, ...) UseMethod("na.approx")

na.approx.zoo <- function(object, along = index(object), na.rm = TRUE, ...) {
	result <- na.approx.default(object, along = along, na.rm = FALSE, ...)
	result <- zoo(result, along)
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

# interpolates object along along which defaults to index(object)
# along has to be numeric, is otherwise coerced
na.approx.default <- function(object, along = index(object), na.rm = TRUE, maxGapFilled = Inf, ...)
{
	along.numeric <- as.numeric(along)
	object.index <- as.numeric(time(object))
	na.approx.0 <- function(y) {
		na <- is.na(y)
		if(all(!na)) return(y)
		# y[na] <- approx(along.index[!na], y[!na], along.numeric[na], ...)$y
		yf <- approx(object.index[!na], y[!na], along.numeric, ...)$y
		fillShortGaps(y, yf, maxGapFilled = maxGapFilled)
	}

        result <- structure(if (length(dim(object)) == 0) na.approx.0(object)
        	else apply(object, 2, na.approx.0), class = class(object))
        if (na.rm) {
            out <- na.omit(result)
            attr(out, "na.action") <- NULL
            out
        } else result
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


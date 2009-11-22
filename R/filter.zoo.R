
filter.zoo <- function (x, ..., na.exclude = FALSE)
{
	if (length(x) == 0) return(x)
	x. <- x
	x.[] <- if (na.exclude) {
		if (length(dim(x)) < 2) {
			x.na <- na.exclude(x)
			y <- filter(coredata(x.na), ...)
			napredict(attr(x.na, "na.action"), y)
		} else apply(coredata(x), 2, filter.zoo, ..., na.exclude = TRUE)
	} else
		if (length(dim(x)) < 2) {
			filter(coredata(x), ...)
		} else apply(coredata(x), 2, filter.zoo, ..., na.exclude = TRUE)
	x.
}
	
if (FALSE) {
na.omit.zoo <- function (object, ...) 
{
	index <- index(object)
    if (!is.atomic(object)) 
        return(object)
    d <- dim(object)
    if (length(d) > 2) 
        return(object)
    omit <- seq_along(object)[is.na(object)]
    if (length(omit) == 0) 
        return(object)
	index. <- index(object)
    object <- if (length(d)) {
        omit <- unique(((omit - 1)%%d[1L]) + 1L)
        nm <- rownames(object)
        object[-omit, , drop = FALSE]
    }
    else {
        nm <- names(object)
        object[-omit]
    }
    if (any(omit > 0L)) {
        attr(object, "na.action") <- structure(index[omit],
			class = c("omit.zoo", class(index(object))))
		class(object) <- c("omit.zoo", class(object))
    }
    object
}

na.exclude.zoo <- function (object, ...) 
{
	index <- index(object)
    if (!is.atomic(object)) 
        return(object)
    d <- dim(object)
    if (length(d) > 2) 
        return(object)
    omit <- seq_along(object)[is.na(object)]
    if (length(omit) == 0) 
        return(object)
    if (length(d)) {
        omit <- unique(((omit - 1)%%d[1L]) + 1L)
        nm <- rownames(object)
        object <- object[-omit, , drop = FALSE]
    }
    else {
        nm <- names(object)
        object <- object[-omit]
    }
    if (any(omit > 0L)) {
        attr(object, "na.action") <- structure(index[omit],
			class = c("exclude.zoo", class(index(object))))
		class(object) <- c("exclude.zoo", class(object))
    }
    object
}
}

napredict.exclude.zoo <- function (omit, x, ...) 
{
	class(omit) <- setdiff(class(omit), "exclude.zoo")
	if (is.null(omit)) x else merge(zoo(, omit), x)
}

cumsum.zoo <- function (x, na.exclude = FALSE)
{
	# replace NA's with 0's, cumsum and put NA's back
	# if x is 2d then do that for each column

	if (length(x) == 0) return(x)
	x. <- x
	if (na.exclude) x.[is.na(x)] <- 0
	x.[] <- if (length(dim(x)) < 2) cumsum(coredata(x.))
		else apply(coredata(x), 2, cumsum.zoo, na.exclude = TRUE)
	if (na.exclude) x.[is.na(x)] <- NA
	x.
}


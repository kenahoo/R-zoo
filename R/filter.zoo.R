
filter.zoo <- function (x, ..., na.action) {

	has.na.action <- !missing(na.action) && !is.null(na.action)
	x. <- if (has.na.action) na.action(x) else x
	fx <- filter(coredata(x.), ...)
	fz <- zoo(coredata(fx), time(x.))
	if (has.na.action) napredict(attr(x., "na.action"), fz) else fz

}

	
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

napredict.exclude.zoo <- function (omit, x, ...) 
{
	class(omit) <- setdiff(class(omit), "exclude.zoo")
	if (is.null(omit)) x else merge(zoo(, omit), x)
}

cumsum.zoo <- function (x, na.action) 
{
	has.na.action <- !missing(na.action) && !is.null(na.action)
	x. <- if (has.na.action) na.action(x) else x
    x.[] <- if (length(dim(x.))) apply(coredata(x.), 2, cumsum)
	else cumsum(coredata(x.))
	if (has.na.action) napredict(attr(x., "na.action"), x.) else x.
}


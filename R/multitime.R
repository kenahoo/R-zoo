multitime <- function(x, ...) as.multitime(x, ...)

as.multitime <- function(x, ...) UseMethod("as.multitime")
as.multitime.default <- function(x, index, ...) {
	class(x) <- c("multitime", setdiff(class(x), "multitime"))
	if (!missing(index)) attr(x, "index") <- index
	x
}
coredata.multitime <- function(x, ...) {
	attr(x, "index") <- NULL
	class(x) <- setdiff(class(x), "multitime")
	x
}
index.multitime <- function(x, ...) attr(x, "index")

as.character.multitime <- function(x, ...) {
	if (is.null(index(x))) coredata(x) else 
	paste(as.character(coredata(x)), "(", as.character(index(x)), ")", sep = "")
}

MATCH.multitime <- function(x, table, nomatch = NA, ...) {
	# TODO: this assumes we have unique character representation for
	# class of coredata and index.  Need to find better way to do this.
	cx <- coredata(x)
	ctable <- coredata(table)
	if (is.null(ctable)) return(match(cx, ctable, nomatch, ...))
	tochar <- function(x) paste(coredata(x), index(x), paste = "\1")
	match(paste(cx, index(x), sep = "\1"), 
		paste(coredata(table), index(table), sep = "\1"), nomatch, ...)
}

ORDER.multitime <- function(x, ...) {
	if (is.null(index(x))) ORDER(coredata(x), ...)
	else order(ORDER(coredata(x)), ORDER(index(x)), ...)
}	

xtfrm.multitime <- function(x) {
	if (is.null(index(x))) xtfrm(coredata(x))
	else { 
		xindex <- xtfrm(index(x))
		(max(xindex) + 1) * xtfrm(coredata(x)) + xindex
	}
}

Ops.multitime <- function (e1, e2) 
{
	e <- NextMethod(.Generic)
    if (is.null(index(e))) as.multitime(e)
	else as.multitime(e, index(e))
}

"[.multitime" <- function(x, i, j, drop = TRUE, ...)
{
  if (is.null(index(x))) {
	as.multitime(coredata(x)[i, ...])
  } else {
	as.multitime(coredata(x)[i, ...], index(x)[i, ...])
  }
} 

c.multitime <- function(...) {
	L <- list(...)
	Lc <- lapply(L, coredata)
	Li <- lapply(L, index)
	lens <- sapply(Li, length)
	if (sum(lens == 0) != length(L)  && sum(lens > 0) != length(L)) 
		stop(paste("incompatible lengths of index values in c.multitime:",
			paste(lens, collapse = " ")))
	as.multitime(do.call("c", Lc), do.call("c", Li))
}
	
as.numeric.multitime <- function(x, ...) {
	cx <- coredata(x)
	if (is.numeric(cx)) cx else as.numeric(cx)
}

as.Date.multitime <- function(x, ...) {
	cx <- coredata(x)
	if (inherits(cx, "Date")) cx 
	else {
		ix <- index(x)
		if (inherits(ix, "Date")) ix else as.Date(cx)
	}
}

as.yearmon.multitime <- function(x, ...) {
	cx <- coredata(x)
	if (inherits(cx, "yearmon")) cx 
	else {
		ix <- index(x)
		if (inherits(ix, "yearmon")) ix else as.yearmon(cx)
	}
}

as.yearqtr.multitime <- function(x, ...) {
	cx <- coredata(x)
	if (inherits(cx, "yearqtr")) cx 
	else {
		ix <- index(x)
		if (inherits(ix, "yearqtr")) ix else as.yearqtr(cx)
	}
}

make.unique.multitime <- function(x, index, sign = -1 , ...) {
	if (missing(index)) {
		index <- if (sign > 0) seq_along(x) - MATCH(x, x)
		else rev(match(rev(x), rev(x)) - seq_along(x))
	}
	as.multitime(x, index, ...)
}



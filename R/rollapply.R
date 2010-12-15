rollapply <- function(data, width, FUN, ..., by = 1, 
  by.column = TRUE, na.pad, align = c("center", "left", "right"), 
  which, partial = FALSE, rule = c("drop", "NA", "extend", "partial"))
    UseMethod("rollapply")

## up to zoo 1.2-0 rollapply was called rapply(), it was deprecated
## up to zoo 1.3-x and removed in zoo 1.4-0.
## 
## rapply <- function(data, width, FUN, ..., by = 1, ascending = TRUE,
##   by.column = TRUE, na.pad = FALSE, align = c("center", "left", "right"))
## {
##     .Deprecated("rollapply")
##     UseMethod("rollapply")
## }

rollapply.zoo <- function(data, width, FUN, ..., by = 1, by.column = TRUE, na.pad,
  align = c("center", "left", "right"), 
  which, partial = FALSE, rule = c("drop", "NA", "extend", "partial")) {

	if (missing(rule) && !missing(na.pad) && na.pad) rule <- "NA"
	if (missing(rule) && !missing(na.pad) && !na.pad) rule <- "drop"
	if (!missing(na.pad) && !missing(rule)) {
		warning("na.pad is deprecated and will be ignored")
	}

	rule <- if (is.numeric(rule)) c("drop", "NA", "extend", "partial")[rule+1]
	else match.arg(rule)
	if (identical(rule, "extend")) {
		warning("rule='extend' not yet implemented - rule ignored")
		rule <- "drop"
	}

    itt <- 0

    embedi <- function(n, which, by = 1) {
    # n = no of time points, k = number of columns
    # by = increment. normally = 1 but if = b calc every b-th point 
    # ascending If TRUE, points passed in ascending order else descending.
    # Note that embed(1:n, k) corresponds to embedi(n, k, by = 1, rev = TRUE)
    # e.g. embedi(10, 3)
			k <- length(which)
    	    s <- seq(1, n, by)
    	    s <- seq_len(n)
    	    lens <- length(s)
			cols <- which
    	    mat <- matrix(s + rep(cols, rep(lens,k)), lens)
			withzero <- replace(mat, TRUE, ifelse(mat < 1 | mat > n, 0, mat))
			if (by > 1) withzero[seq(1, nrow(withzero), by), ] else withzero
    }


    if (missing(which) && by.column && by == 1 && length(list(...)) < 1 &&
		length(sw <- deparse(substitute(FUN))) == 1) {
    if (sw == "mean" && all(!is.na(data))) {
		return(rollmean(data, width, rule = rule, align = align))
	} else switch(sw,
		max = return(rollmax(data, width, rule = rule, align = align)),
		median = return(rollmedian(data, width, rule = rule, align = align)))
	}

    ## evaluate FUN only on coredata(data)
    cdata <- coredata(data)
    nr <- NROW(cdata)
	width <- if (!missing(width) && !is.null(width)) as.integer(width)[1]
    
    ## process alignment
    tt <- index(data)

	if (missing(which) || is.null(which)) {
		if (is.null(width)) stop("width and which cannot both be missing")
		align <- match.arg(align)
		which <- switch(align,
		  "left" = { seq(from = 0, length = width) },
		  "center" = { seq(to = floor(width/2), length = width) },
		  "right" = { seq(to= 0, length = width) })    
	}

    FUN <- match.fun(FUN)
	e <- embedi(nr, which, by)
	idx <- if (rule == "partial") rep(TRUE, nr) else apply(e > 0, 1, all) 
	idx <- idx & apply(e > 0, 1, any)
	idx <- seq_along(idx)[idx]
    res <- if (is.null(dim(cdata))) {
           xx <- sapply(idx, function(i) FUN(cdata[e[i,]], ...))
	   if (! is.null(dim(xx))) xx <- t(xx)
	   zoo(xx, tt[idx], if (by == 1 || rule == "NA") attr(data, "frequency"))
    } else if (by.column) {
	    # e <- embedi(nr, width, by, ascending)
	    s <- sapply( seq_len(ncol(cdata)), 
			function(i) apply( e[idx,], 1, function(st) FUN(cdata[st,i], ...) ) )
		if (length(s) > 1 && length(tt) == 1) s <- matrix(s, 1)
		zoo(s, tt[idx], if (by == 1 || rule == "NA") attr(data, "frequency"))
    } else {
           rval <- apply(e[idx,], 1, function(st) FUN(cdata[st,,drop=FALSE], ...))
	   if(!is.null(dim(rval))) rval <- t(rval)
	   zoo(rval, tt[idx], if (by == 1) attr(data, "frequency"))
    }	   
    res <- if (rule == "NA") merge(res, zoo(,index(data), attr(data, "frequency"))) else res
    if(by.column && !is.null(dim(cdata))) colnames(res) <- colnames(cdata)
    return(res)
} 

rollapply.ts <- function(data, width, FUN, by = 1, by.column = TRUE, 
	na.pad = FALSE, rule = c("drop", "NA", "extend", "partial"), ...) {
  as.ts(rollapply(as.zoo(data), width = width, FUN = FUN, by = by, 
               by.column = by.column, na.pad = na.pad, rule = rule, ...))
}

rollapply.default <- function(data, width, FUN, by = 1, by.column = TRUE, 
	na.pad = FALSE, rule = c("drop", "NA", "extend", "partial"), ...) {
  coredata(rollapply(as.zoo(data), width = width, FUN = FUN, by = by,
               by.column = by.column, na.pad = na.pad, rule = rule, ...))
}


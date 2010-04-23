## new generic functions that do "intuitive"
## leads/lags for time series objects
lagts <- function(x, ...) UseMethod("lagts")
leadts <- function(x, ...) UseMethod("leadts")

lagts.default <- function(x, k = 1, na.pad = TRUE, ...)
{
    ## NOTE:
    ## na.pad = FALSE might not be very useful...especially if length(k) > 1

    ## number of observations
    nr <- NROW(x)

    ## handling of multiple lags
    if(length(k) > 1) {
        ## first align everything along original index (i.e., na.pad = TRUE)
        if(is.null(names(k))) names(k) <- paste("lag", k, sep = "")
        rval <- do.call("cbind", lapply(k, lagts.default, x = x, na.pad = TRUE))

        ## try to figure out what observation have been padded for all series
	if(!na.pad & min(abs(k)) > 0) {
            rval <- if(all(k > 0)) {
	        rval[-seq(1, length = min(k[k > 0])),, drop = FALSE]
            } else if(all(k < 0)) {
	        rval[-seq(to = nr, length = -max(k[k < 0])),, drop = FALSE]
	    } else {
	        if((min(k[k > 0]) >= nr) & (min(-k[k < 0]) >= nr)) rval[0,, drop = FALSE]
	    }
	}

        return(rval)
    }
   
    ## handling of a single lag

    ## consistency checks for k
    if(k != round(k)) {
        k <- round(k)
        warning("k is not an integer")
    }
    if(k < 0) warning("'k' is negative, i.e., leads not lags are computed. It is recommended to use leadts() instead.")
    if(k == 0) return(x)
    if(abs(k) > nr) k <- nr

    ## do subsetting first
    if(k < 0)  {
        rval <- if(length(dim(x)) == 0) {
            x[-seq(1, length = -k)]
        } else {
	    x[-seq(1, length = -k),, drop = FALSE]
	}
    } else {
        rval <- if(length(dim(x)) == 0) {
	    x[-seq(to = nr, length = k)]
	} else {
  	    x[-seq(to = nr, length = k),, drop = FALSE]
	}
    }
   
    ## do padding if selected
    if(na.pad) {
        nas <- if(length(dim(x)) == 0) {
	    rep.int(NA, min(abs(k), length(x)))
	} else {
	    matrix(NA, ncol = ncol(rval), nrow = min(abs(k), length(x)))
	}
        rval <- if(length(dim(x)) == 0) {
	    if(k > 0) c(nas, rval) else c(rval, nas)
	} else {
	    if(k > 0) rbind(nas, rval) else rbind(rval, nas)
	}
    }
    
    rval
}

lagts.ts <- function(x, k = 1, na.pad = TRUE, ...)
{
    ## number of observations
    nr <- NROW(x)
    
    ## handling of multiple lags
    if(length(k) > 1) {
        if(is.null(names(k))) names(k) <- paste("lag", k, sep = "")
        rval <- do.call("ts.union", lapply(k, lagts.ts, x = x, na.pad = na.pad))
        return(rval)
    }

    ## handling of a single lag

    ## consistency checks for k
    if(k != round(k)) {
        k <- round(k)
        warning("'k' is not an integer")
    }
    if(k < 0) warning("'k' is negative, i.e., leads not lags are computed. It is recommended to use leadts() instead.")
    if(k == 0) return(x)
    if(abs(k) > nr) k <- nr
    
    p <- tsp(x)    

    if(!na.pad) {
        tsp(x) <- p + (k/p[3L]) * c(1, 1, 0)
    } else {
        nas <- if(length(dim(x)) == 0) {
	    rep.int(NA, min(abs(k), length(x)))
	} else {
	    matrix(NA, ncol = ncol(x), nrow = min(abs(k), length(x)))
	}
	
        x <- if(length(dim(x)) == 0) {
	    if(k > 0) c(nas, x) else c(x, nas)
	} else {
	    if(k > 0) rbind(nas, x) else rbind(x, nas)
	}
	
	x <- if(k > 0) {
	    ts(x, start = p[1], frequency = p[3])
	} else {
	    ts(x, end = p[2], frequency = p[3])
	}
    }
    x
}

lagts.zoo <- function(x, k = 1, na.pad = TRUE, ...)
{
  if(length(k) > 1) {
    if(is.null(names(k)))
      names(k) <- paste("lagts",k,sep="")
    return(do.call("merge.zoo", lapply(k, lagts.zoo, x=x, na.pad=na.pad)))
  }
  if(abs(k) > NROW(x))
    return(zoo(,))
  .Call("zoo_lagts", x, as.integer(k), as.logical(na.pad), PACKAGE = "zoo")
}

fortify.zoo <- function(model, data, melt = FALSE, ...)
{
  ## dimensions
  n <- NROW(model)
  k <- NCOL(model)

  ## series labels
  lab <- deparse(substitute(model))  
  lab <- if(is.null(nam <- colnames(model))) {
    if(k == 1L) lab else paste(lab, 1:k, sep = ".")
  } else {
    nam
  }
  
  ## either long format (melt = TRUE) or wide format (melt = FALSE)
  if(melt) {
    df <- if(k == 1L) {
      data.frame(index(model), factor(rep.int(1, n), labels = lab), coredata(model))
    } else {
      data.frame(index(model)[rep.int(1:n, k)],
        factor(rep(1:k, each = n), levels = 1:k, labels = lab),
	value <- as.vector(coredata(model)))
    }
    names(df) <- c("time", "series", "value")
  } else {
    df <- cbind(data.frame(index(model)), coredata(model))
    names(df) <- c("time", lab)  
  }
  
  return(df)
}


autoplot.zoo <- function(object, ..., geom = "line")
{
  ## need ggplot2 package
  stopifnot(require("ggplot2"))

  ## convert to data.frame (and assure correct label
  ## processing by fortify.zoo)
  lab <- deparse(substitute(object))
  if(NCOL(object) == 1L) {
    dim(object) <- c(NROW(object), 1L)
    colnames(object) <- lab
  }
  if(is.null(colnames(object))) colnames(object) <- paste(lab, 1:NCOL(object), sep = ".")
  df <- na.omit(fortify.zoo(object, melt = TRUE))

  ## call qplot
  gg <- if(nlevels(df$series) == 1L) {
    qplot(time, value, data = df, geom = geom, ...) + ylab(levels(df$series)) + xlab("Index")
  } else {
    qplot(time, value, data = df, group = series, colour = series, linetype = series,
      shape = series, geom = geom, ...) + ylab("") + xlab("Index")
  }
  return(gg)
}

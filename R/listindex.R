## really awkward and wrong way to set up a class
## only used for regtesting zoo in conversion to C code
listindex <- function(object, ...) structure(list(index = object), class = "listindex")

## necessary methods for zoo interface
## (emulate a dispatch on $index)
c.listindex <- function(...) listindex(do.call("c", lapply(list(...), function(x) x$index)))
"[.listindex" <- function(x, i, ...) listindex(x$index[i, ...])
length.listindex <- function(x) length(x$index)
ORDER.listindex <- function(x, ...) ORDER(x$index, ...)
MATCH.listindex <- function(x, table, nomatch = NA, ...) MATCH(x$index, table$index, nomatch = nomatch, ...)

## de facto these are required as well
as.numeric.listindex <- function(x, ...) as.numeric(x$index, ...)
as.vector.listindex <- function(x, ...) as.vector(x$index, ...)
as.character.listindex <- function(x, ...) as.character(x$index, ...)

## these help also
print.listindex <- function(x, ...) print(x$index, ...)
xtfrm.listindex <- function(x, ...) xtfrm(x$index, ...)

## some code for illustration
##
## x <- listindex(as.Date(0) + 0:2)
## y <- listindex(as.Date(0) + 3:5)
## z1 <- zoo(rnorm(4), c(x, y[1]))
## z2 <- zoo(rnorm(4), c(x[3], y))
##
## z1 + z2
## lag(z1, -1)
## merge(z1, z2)
## c(z1[1:2], z2[4])

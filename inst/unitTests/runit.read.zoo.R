

# needed as all.equal in chron seems not to work
all.equal.chron <-
all.equal.times <-
all.equal.dates <- function(target, current, ...) {
	num <- all.equal(as.numeric(target), as.numeric(current), ...)
	if (isTRUE(num)) {
		a <- attr.all.equal(target, current, ...)
		if (is.null(a)) TRUE else a
	} else num
}

test.read.zoo.simple <- function() {
	# simple test
	Lines <- "
	time latitude longitude altitude  distance heartrate
	1277648884 0.304048 -0.793819      260  0.000000        94
	1277648885 0.304056 -0.793772      262  4.307615        95
	1277648894 0.304075 -0.793544      263 25.237911       103
	1277648902 0.304064 -0.793387      256 40.042988       115
	"
	z <- read.zoo(textConnection(Lines), header = TRUE)
	z
	target <- structure(c(0.304048, 0.304056, 0.304075, 0.304064, -0.793819, 
	-0.793772, -0.793544, -0.793387, 260, 262, 263, 256, 0, 4.307615, 
	25.237911, 40.042988, 94, 95, 103, 115), .Dim = 4:5, .Dimnames = list(
		NULL, c("latitude", "longitude", "altitude", "distance", 
		"heartrate")), index = c(1277648884L, 1277648885L, 1277648894L, 
	1277648902L), class = "zoo")
	checkIdentical(z, target)
}

# if (FALSE)
test.read.zoo.FUN <- function() {
    require(chron)
	DF <- structure(list(
	  Time = structure(1:5, .Label = c("7:10:03 AM", "7:10:36 AM",
		"7:11:07 AM", "7:11:48 AM", "7:12:25 AM"), class = "factor"),
	  Bid = c(6118.5, 6118.5, 6119.5, 6119, 6119),
	  Offer = c(6119.5, 6119.5, 6119.5, 6120, 6119.5)),
	  .Names = c("Time", "Bid", "Offer"), row.names = c(NA, -5L),
	  class = "data.frame")
	DF
	z <- read.zoo(DF, FUN = function(x) 
	  times(as.chron(paste("1970-01-01", x), format = "%Y-%m-%d %H:%M:%S %p")))
	target <- structure(c(6118.5, 6118.5, 6119.5, 6119, 6119, 6119.5, 6119.5, 
6119.5, 6120, 6119.5), .Dim = c(5L, 2L), .Dimnames = list(NULL, 
    c("Bid", "Offer")), index = structure(c(0.298645833333333, 
0.299027777777778, 0.299386574074074, 0.299861111111111, 0.300289351851852
), format = "h:m:s", origin = structure(c(1, 1, 1970), .Names = c("month", 
"day", "year")), class = "times"), class = "zoo")
	checkEquals(z, target)
}

test.read.zoo.split <- function() {
	Lines <- "
    Date;Time;Close
    01/09/2009;10:00;56567
    01/09/2009;10:05;56463
    01/09/2009;10:10;56370
    01/09/2009;16:45;55771
    01/09/2009;16:50;55823
    01/09/2009;16:55;55814
    02/09/2009;10:00;55626
    02/09/2009;10:05;55723
    02/09/2009;10:10;55659
    02/09/2009;16:45;55742
    02/09/2009;16:50;55717
    02/09/2009;16:55;55385"
	f <- function(x) times(paste(x, 0, sep = ":"))
	z <- read.zoo(textConnection(Lines), header = TRUE, sep = ";", 
	  split = 1, index = 2, FUN = f)
	colnames(z) <- sub(".*(\\d{2}).(\\d{2}).(\\d{4}).*", "\\3-\\2-\\1", 
		colnames(z))
	target <-
	structure(c(56567L, 56463L, 56370L, 55771L, 55823L, 55814L, 55626L, 
	55723L, 55659L, 55742L, 55717L, 55385L), .Dim = c(6L, 2L), .Dimnames = list(
		NULL, c("2009-09-01", "2009-09-02")), index = structure(c(0.416666666666667, 
	0.420138888888889, 0.423611111111111, 0.697916666666667, 0.701388888888889, 
	0.704861111111111), format = "h:m:s", class = "times"), class = "zoo")
	checkEquals(z, target)

	# same but refer to split column using its column name
	z2 <- read.zoo(textConnection(Lines), header = TRUE, sep = ";", 
	  split = "Date", index = 2, FUN = f)
	colnames(z2) <- sub(".*(\\d{2}).(\\d{2}).(\\d{4}).*", "\\3-\\2-\\1", 
		colnames(z2))
	checkEquals(z2, target)
}

# if (FALSE)
test.read.zoo.fun.custom <- function() {
	Lines <- "
	Date Time O H L C
	1/2/2005 17:05 1.3546 1.3553 1.3546 1.35495
	1/2/2005 17:10 1.3553 1.3556 1.3549 1.35525
	1/2/2005 17:15 1.3556 1.35565 1.35515 1.3553
	1/2/2005 17:25 1.355 1.3556 1.355 1.3555
	1/2/2005 17:30 1.3556 1.3564 1.35535 1.3563
	"
	f <- function(d, t) as.chron(paste(as.Date(chron(d)), t))
	z <- read.zoo(textConnection(Lines), header = TRUE,
	  index = 1:2, FUN = f)
	target <-
	structure(c(1.3546, 1.3553, 1.3556, 1.355, 1.3556, 1.3553, 1.3556, 
	1.35565, 1.3556, 1.3564, 1.3546, 1.3549, 1.35515, 1.355, 1.35535, 
	1.35495, 1.35525, 1.3553, 1.3555, 1.3563), .Dim = c(5L, 4L), .Dimnames = list(
		NULL, c("O", "H", "L", "C")), index = structure(c(12785.7118055556, 
	12785.7152777778, 12785.71875, 12785.7256944444, 12785.7291666667
	), format = structure(c("m/d/y", "h:m:s"), .Names = c("dates", 
	"times")), origin = structure(c(1, 1, 1970), .Names = c("month", 
	"day", "year")), class = c("chron", "dates", "times")), class = "zoo")
	checkEquals(z, target)
}

test.read.zoo.aggregate <- function() {
	Lines <-
	"  views  number  timestamp day            time
	1  views  910401 1246192687 Sun 6/28/2009 12:38
	2  views  921537 1246278917 Mon 6/29/2009 12:35
	3  views  934280 1246365403 Tue 6/30/2009 12:36
	4  views  986463 1246888699 Mon  7/6/2009 13:58
	5  views  995002 1246970243 Tue  7/7/2009 12:37
	6  views 1005211 1247079398 Wed  7/8/2009 18:56
	7  views 1011144 1247135553 Thu  7/9/2009 10:32
	8  views 1026765 1247308591 Sat 7/11/2009 10:36
	9  views 1036856 1247436951 Sun 7/12/2009 22:15
	10 views 1040909 1247481564 Mon 7/13/2009 10:39
	11 views 1057337 1247568387 Tue 7/14/2009 10:46
	12 views 1066999 1247665787 Wed 7/15/2009 13:49
	13 views 1077726 1247778752 Thu 7/16/2009 21:12
	14 views 1083059 1247845413 Fri 7/17/2009 15:43
	15 views 1083059 1247845824 Fri 7/17/2009 18:45
	16 views 1089529 1247914194 Sat 7/18/2009 10:49
	"
	cl <- c("NULL", "numeric", "character")[c(1, 1, 2, 2, 1, 3, 1)]
	cn <- c(NA, NA, "views", "number", NA, NA, NA)
	z <- read.zoo(textConnection(Lines),
	  skip = 1, col.names = cn, colClasses = cl,
	  index = 3, format = "%m/%d/%Y",
	  aggregate = function(x) tail(x, 1))
	target <-
	structure(c(910401, 921537, 934280, 986463, 995002, 1005211, 
	1011144, 1026765, 1036856, 1040909, 1057337, 1066999, 1077726, 
	1083059, 1089529, 1246192687, 1246278917, 1246365403, 1246888699, 
	1246970243, 1247079398, 1247135553, 1247308591, 1247436951, 1247481564, 
	1247568387, 1247665787, 1247778752, 1247845824, 1247914194), .Dim = c(15L, 
	2L), .Dimnames = list(c("2009-06-28", "2009-06-29", "2009-06-30", 
	"2009-07-06", "2009-07-07", "2009-07-08", "2009-07-09", "2009-07-11", 
	"2009-07-12", "2009-07-13", "2009-07-14", "2009-07-15", "2009-07-16", 
	"2009-07-17", "2009-07-18"), c("views", "number")), index = structure(c(14423, 
	14424, 14425, 14431, 14432, 14433, 14434, 14436, 14437, 14438, 
	14439, 14440, 14441, 14442, 14443), class = "Date"), class = "zoo")
	checkIdentical(z, target)
}

# if (FALSE)
test.read.zoo.fun.custom.2 <- function() {
	Lines <- "
    Date,Time,Open,High,Low,Close,Up,Down
    05.02.2001,00:30,421.20,421.20,421.20,421.20,11,0
    05.02.2001,01:30,421.20,421.40,421.20,421.40,7,0
    05.02.2001,02:00,421.30,421.30,421.30,421.30,0,5"
	f <- function(d, t) chron(d, paste(t, "00", sep = ":"),
	  format = c("m.d.y", "h:m:s"))
	z <- read.zoo(textConnection(Lines), sep = ",", header = TRUE,
	  index = 1:2, FUN  = f)
	target <-
	structure(c(421.2, 421.2, 421.3, 421.2, 421.4, 421.3, 421.2, 
	421.2, 421.3, 421.2, 421.4, 421.3, 11, 7, 0, 0, 0, 5), .Dim = c(3L, 
	6L), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", 
	"Up", "Down")), index = structure(c(11444.0208333333, 11444.0625, 
	11444.0833333333), format = c("m.d.y", "h:m:s"), origin = structure(c(1, 
	1, 1970), .Names = c("month", "day", "year")), class = c("chron", 
	"dates", "times")), class = "zoo")
	checkEquals(z, target)

	f2 <- function(d, t) as.chron(paste(d, t), format = "%d.%m.%Y %H:%M")
	z2 <- read.zoo(textConnection(Lines), sep = ",", header = TRUE, 
	  index = 1:2, FUN  = f2)
	target2 <-
	structure(c(421.2, 421.2, 421.3, 421.2, 421.4, 421.3, 421.2, 
	421.2, 421.3, 421.2, 421.4, 421.3, 11, 7, 0, 0, 0, 5), .Dim = c(3L, 
	6L), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", 
	"Up", "Down")), index = structure(c(11358.0208333333, 11358.0625, 
	11358.0833333333), format = structure(c("m/d/y", "h:m:s"), .Names = c("dates", 
	"times")), origin = structure(c(1, 1, 1970), .Names = c("month", 
	"day", "year")), class = c("chron", "dates", "times")), class = "zoo")
	checkEquals(z2, target2)

	z3 <- read.zoo(textConnection(Lines), sep = ",", header = TRUE, 
	index = 1:2, tz = "", format = "%d.%m.%Y %H:%M")
	target3 <- structure(c(421.2, 421.2, 421.3, 421.2, 421.4, 421.3, 421.2, 
	421.2, 421.3, 421.2, 421.4, 421.3, 11, 7, 0, 0, 0, 5), .Dim = c(3L, 
	6L), .Dimnames = list(NULL, c("Open", "High", "Low", "Close", 
	"Up", "Down")), index = structure(c(981351000, 981354600, 981356400
	), class = c("POSIXct", "POSIXt"), tzone = ""), class = "zoo")
	checkEquals(z3, target3)
}

test.read.zoo.POSIXct <- function() {
	Lines <- "Date Time V2   V3   V4   V5
	2010-10-15 13:43:54 73.8 73.8 73.8 73.8
	2010-10-15 13:44:15 73.8 73.8 73.8 73.8
	2010-10-15 13:45:51 73.8 73.8 73.8 73.8
	2010-10-15 13:46:21 73.8 73.8 73.8 73.8
	2010-10-15 13:47:27 73.8 73.8 73.8 73.8
	2010-10-15 13:47:54 73.8 73.8 73.8 73.8
	2010-10-15 13:49:51 73.7 73.7 73.7 73.7
	"
	z <- read.zoo(textConnection(Lines), header = TRUE,
	  index = 1:2, tz = "")
	target <- structure(c(73.8, 73.8, 73.8, 73.8, 73.8, 73.8, 73.7, 73.8, 73.8, 
	73.8, 73.8, 73.8, 73.8, 73.7, 73.8, 73.8, 73.8, 73.8, 73.8, 73.8, 
	73.7, 73.8, 73.8, 73.8, 73.8, 73.8, 73.8, 73.7), .Dim = c(7L, 
	4L), .Dimnames = list(NULL, c("V2", "V3", "V4", "V5")), index = structure(c(1287164634, 
	1287164655, 1287164751, 1287164781, 1287164847, 1287164874, 1287164991
	), class = c("POSIXct", "POSIXt"), tzone = ""), class = "zoo")
	checkIdentical(z, target)
}

test.read.zoo.na <- function() {
	Lines <- "
	13/10/2010      A       23
	13/10/2010      B       12
	13/10/2010      C       124
	14/10/2010      A       43
	14/10/2010      B       54
	14/10/2010      C       65
	15/10/2010      A       43
	15/10/2010      B       N.A.
	15/10/2010      C       65
	"
	z <- read.zoo(textConnection(Lines), na.strings = "N.A.",
	  format = "%d/%m/%Y", split = 2)
	target <- 
	structure(c(23L, 43L, 43L, 12L, 54L, NA, 124L, 65L, 65L), .Dim = c(3L, 
	3L), .Dimnames = list(NULL, c("A", "B", "C")), 
	index = structure(14895:14897, class = "Date"), class = "zoo")
	checkIdentical(z, target)
}

# if (FALSE)
test.read.zoo.colClasses <- function() {
	Lines <- '
	"","Fish_ID","Date","R2sqrt"
	"1",1646,2006-08-18 08:48:59,0
	"2",1646,2006-08-18 09:53:20,100
	'
	z <- read.zoo(textConnection(Lines), header = TRUE, sep = ",",
	  colClasses = c("NULL", "NULL", "character", "numeric"), FUN = as.chron)
	target <-
	structure(c(0, 100), index = structure(c(13378.367349537, 13378.412037037
	), format = structure(c("m/d/y", "h:m:s"), .Names = c("dates", 
	"times")), origin = structure(c(1, 1, 1970), .Names = c("month", 
	"day", "year")), class = c("chron", "dates", "times")), class = "zoo")
	checkEquals(z, target)
	z2 <- read.zoo(textConnection(Lines), header = TRUE, sep = ",",
	  colClasses = c("NULL", "NULL", "character", "numeric"), tz = "")
	target2 <-
	structure(c(0, 100), index = structure(c(1155905339, 1155909200
	), class = c("POSIXct", "POSIXt"), tzone = ""), class = "zoo")
	checkEquals(z2, target2)
}

# if (FALSE)
test.read.zoo.multiple.time.columns <- function() {
	Lines <-
	" iteration         Datetime    VIC1    NSW1     SA1    QLD1
	1         1 2011-01-01 00:30 5482.09 7670.81 2316.22 5465.13
	2         1 2011-01-01 01:00 5178.33 7474.04 2130.30 5218.61
	3         1 2011-01-01 01:30 4975.51 7163.73 2042.39 5058.19
	4         1 2011-01-01 02:00 5295.36 6850.14 1940.19 4897.96
	5         1 2011-01-01 02:30 5042.64 6587.94 1836.19 4749.05
	6         1 2011-01-01 03:00 4799.89 6388.51 1786.32 4672.92
	"
	z <- read.zoo(textConnection(Lines), skip = 1, index = 3:4,
	  FUN = paste, FUN2 = as.chron)
	target <- structure(c(1, 2, 3, 4, 5, 6, 1, 1, 1, 1, 1, 1, 5482.09, 5178.33, 
	4975.51, 5295.36, 5042.64, 4799.89, 7670.81, 7474.04, 7163.73, 
	6850.14, 6587.94, 6388.51, 2316.22, 2130.3, 2042.39, 1940.19, 
	1836.19, 1786.32, 5465.13, 5218.61, 5058.19, 4897.96, 4749.05, 
	4672.92), .Dim = c(6L, 6L), .Dimnames = list(NULL, c("V1", "V2", 
	"V5", "V6", "V7", "V8")), index = structure(c(14975.0208333333, 
	14975.0416666667, 14975.0625, 14975.0833333333, 14975.1041666667, 
	14975.125), format = structure(c("m/d/y", "h:m:s"), .Names = c("dates", 
	"times")), origin = structure(c(1, 1, 1970), .Names = c("month", 
	"day", "year")), class = c("chron", "dates", "times")), class = "zoo")
	checkEquals(z, target)
	z2 <- read.zoo(textConnection(Lines), skip = 1, index = 3:4,
	  tz = "")
	target2 <-
	structure(c(1, 2, 3, 4, 5, 6, 1, 1, 1, 1, 1, 1, 5482.09, 5178.33, 
	4975.51, 5295.36, 5042.64, 4799.89, 7670.81, 7474.04, 7163.73, 
	6850.14, 6587.94, 6388.51, 2316.22, 2130.3, 2042.39, 1940.19, 
	1836.19, 1786.32, 5465.13, 5218.61, 5058.19, 4897.96, 4749.05, 
	4672.92), .Dim = c(6L, 6L), .Dimnames = list(NULL, c("V1", "V2", 
	"V5", "V6", "V7", "V8")), index = structure(c(1293859800, 1293861600, 
	1293863400, 1293865200, 1293867000, 1293868800), class = c("POSIXct", 
	"POSIXt"), tzone = ""), class = "zoo")
	checkEquals(z2, target2)
}

test.read.zoo.last.in.month <- function() {
	DF <- structure(list(
	  Date = structure(c(14609, 14638, 14640, 14666, 14668, 14699,
		14729, 14757, 14759, 14760), class = "Date"),
	  A = c(4.9, 5.1, 5, 4.8, 4.7, 5.3, 5.2, 5.4, NA, 4.6),
	  B = c(18.4, 17.7, NA, NA, 18.3, 19.4, 19.7, NA, NA, 18.1),
	  C = c(32.6, NA, 32.8, NA, 33.7, 32.4, 33.6, NA, 34.5, NA),
	  D = c(77, NA, 78.7, NA, 79, 77.8, 79, 81.7, NA, NA)),
	  .Names = c("Date", "A", "B", "C", "D"), row.names = c(NA, -10L),
	  class = "data.frame")
	DF
	z <- read.zoo(DF)
	last. <- na.locf(z)[!duplicated(as.yearmon(time(z)), fromLast = TRUE)]
	target <-
	structure(c(4.9, 5, 4.7, 5.3, 5.2, 4.6, 18.4, 17.7, 18.3, 19.4, 
	19.7, 18.1, 32.6, 32.8, 33.7, 32.4, 33.6, 34.5, 77, 78.7, 79, 
	77.8, 79, 81.7), .Dim = c(6L, 4L), .Dimnames = list(NULL, c("A", 
	"B", "C", "D")), index = structure(c(14609, 14640, 14668, 14699, 
	14729, 14760), class = "Date"), class = "zoo")
	checkIdentical(last., target)
}

test.read.zoo.aggregate.2 <- function() {
	Lines <- "
	2009-10-07      0.009378
	2009-10-19      0.014790
	2009-10-23      -0.005946
	2009-10-23      0.009096
	2009-11-08      0.004189
	2009-11-10      -0.004592
	2009-11-17      0.009397
	2009-11-24      0.003411
	2009-12-02      0.003300
	2010-01-15      0.010873
	2010-01-20      0.010712
	2010-01-20      0.022237
	"
	z <- read.zoo(textConnection(Lines),
	  aggregate = function(x) tail(x, 1))
	target <- structure(c(0.009378, 0.01479, 0.009096, 0.004189, -0.004592, 
	0.009397, 0.003411, 0.0033, 0.010873, 0.022237), index = structure(c(14524, 
	14536, 14540, 14556, 14558, 14565, 14572, 14580, 14624, 14629
	), class = "Date"), class = "zoo")
	checkIdentical(z, target)
}


test.read.zoo.two.fields <- function() {
	Lines <- "
    timestamp,time-step-index,value
    2009-11-23 15:58:21,23301,800
    2009-11-23 15:58:29,23309,950"
	z <- read.zoo(textConnection(Lines), header = TRUE, sep = ",",
	  tz = "")
	target <- 
	structure(c(23301L, 23309L, 800L, 950L), .Dim = c(2L, 2L), .Dimnames = list(
    NULL, c("time.step.index", "value")), index = structure(c(1259009901, 
	1259009909), class = c("POSIXct", "POSIXt"), tzone = ""), class = "zoo")
	checkEquals(z, target)
	z2 <- read.zoo(textConnection(Lines), header = TRUE, sep = ",",
	FUN = as.chron)
	target2 <- 
	structure(c(23301L, 23309L, 800L, 950L), .Dim = c(2L, 2L), .Dimnames = list(
	NULL, c("time.step.index", "value")), index = structure(c(14571.6655208333, 
	14571.6656134259), format = structure(c("m/d/y", "h:m:s"), .Names = c("dates", 
	"times")), origin = structure(c(1, 1, 1970), .Names = c("month", 
	"day", "year")), class = c("chron", "dates", "times")), class = "zoo")
	checkEquals(z2, target2)
}

# if (FALSE)
test.read.zoo.chron <- function() {
	require(chron)
	Lines <- "
	Date Time Value
	01/23/2000 10:12:15 12.12
	01/24/2000 11:10:00 15.00
	"
	z <- read.zoo(textConnection(Lines), header = TRUE,
	  index = 1:2, FUN = chron)
	target <- structure(c(12.12, 15), index = structure(c(10979.4251736111, 
	10980.4652777778), format = structure(c("m/d/y", "h:m:s"), .Names = c("dates", 
	"times")), origin = structure(c(1, 1, 1970), .Names = c("month", 
	"day", "year")), class = c("chron", "dates", "times")), class = "zoo")
	checkEquals(z, target)
}

test.read.zoo.quarterly <- function() {
	Lines <- "
	Year   Qtr1  Qtr2  Qtr3  Qtr4   
	1992    566   443   329   341   
	1993    344   212   133   112   
	1994    252   252   199   207
	"
	za <- read.zoo(textConnection(Lines), header = TRUE)
	zq <- zooreg(as.vector(t(za)), start = yearqtr(start(za)), freq = 4)
	target <- structure(c(566L, 443L, 329L, 341L, 344L, 212L, 133L, 112L, 252L, 
	252L, 199L, 207L), index = structure(c(1992, 1992.25, 1992.5, 
	1992.75, 1993, 1993.25, 1993.5, 1993.75, 1994, 1994.25, 1994.5, 
	1994.75), class = "yearqtr"), frequency = 4, class = c("zooreg", 
	"zoo"))
	checkIdentical(zq, target)
}




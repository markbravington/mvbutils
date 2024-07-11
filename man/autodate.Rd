\name{autodate}
\alias{autodate}
\title{Universal date converter}
\description{At your own risk: this aims for the most-sensible interpretation of a character vector of dates in whatever godawful format they may be, to avoid the delights of \code{\link{strptime}}. "Most sensible" is according to \emph{me}; but \emph{you} (or the originator of the dataset) might have different ideas, and if so it's \emph{your} problem. See \bold{Details} for, you guessed it, DETAILS.
}
\usage{autodate(datestr, ct=TRUE)
}
\arguments{
\item{ datestr}{character vector}
\item{ ct}{whether to return \code{POSIXct} (the default) or \code{POSIXlt} object}
}
\value{\code{POSIXct} or \code{\link{POSIXlt}} object, always with timezone \code{GMT}. Attributes (\code{dim} etc) should be preserved.
}
\details{
All dates in the vector must have the \emph{same} format as each other. Each must have a Day, Month, and Year, in any order except that Year cannot be in the middle, separated by either "/" or "-". Spaces are ignored. Month can be numbers, 3-letter abbreviation, or full month name. Year can be either 2- or 4-digits, but (unlike \code{strptime} itself) all digits are checked; note that \code{strptime} will uncomplainingly accept \code{1/1/2099} as coming from AD20 if you tell it \code{\%Y}, even tho IMO you should have to write eg \code{1/1/0020} if you want stuff pre-AD1000, and \code{autodate} will enforce that. Consequently, leading zeros on Day and Month are ignored, but are honoured on Year.

In case of ambiguous results (which are common, especially with Day and 2-digit Year), the version with the smallest range is chosen; if several versions have equal range, the most recent (or furthest-future) is chosen.
}
\examples{
## Should add more...
## Unambigous:
autodate( '1-Mar-2017')
# [1] "2017-03-01 GMT"
## Stupid
autodate( '1/1/1')
# Warning in autodate("1/1/1") :
#   Ambiguous date format: gonna pick futurest...
# [1] "2001-01-01 GMT"
## Ancient: NB 4 digits.
autodate( '1/13/0001')
# [1] "0001-01-13 GMT"
## Lazy, 2-digit year: assume modern
autodate( '1/13/01')
#  Warning in autodate("1/13/01") :
#    Ambiguous date format: gonna pick futurest...
#  [1] "2001-01-13 GMT"
## Corner case...
autodate( character(), ct=FALSE)
# POSIXlt of length 0
autodate( character())
# character(0) # actually CORRECT-- it really is 'POSIXct'-- but just prints as if wrong
}
\keyword{misc}

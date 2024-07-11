\name{do.on}
\alias{do.on}
\alias{FOR}
\title{Easier sapply/lapply avoiding explicit function}
\description{
Simpler to demonstrate:

\preformatted{
  do.on( find.funs(), environment( get( .)))
  # same as:
  lapply( find.funs(), function( x) environment( get( x)))
}

\code{do.on} evaluates \code{expr} for all elements of \code{x}. The expression should involve the symbol \code{.}, and will be cast into a function which has an argument \code{.} and knows about any dotdotdot arguments passed to \code{do.on} (and objects in the function that calls \code{do.on}). If \code{x} is atomic (e.g. character or numeric, but not list) and lacks names, it will be given names via \code{\link{named}}. With \code{do.on}, you are calling \code{sapply}, so the result is simplified if possible, unless \code{simplify=FALSE} (or \code{simplify="array"}, for which see \code{\link{sapply}}). With \code{FOR}, you are calling \code{lapply}, so no simplication is tried; this is often more useful for programming.
}
\usage{
do.on(x, expr, ..., simplify = TRUE)
FOR(x, expr, ...)
}
\arguments{
\item{ x}{thing to be iterated over. Names are copied to the result, and are pre-allocated if required as per \bold{Description}}
\item{ expr}{expression, presumably involving the symbol \code{.} which will successively become the individual elements of \code{x}}
\item{ ...}{other "arguments" for \code{expr}}
\item{ simplify}{as per \code{sapply}, and defaulting to TRUE.}
}
\value{
\item{ do.on}{as per \code{sapply}, a vector or array of the same "length" as \code{x}.}
\item{ FOR}{a list of the same length as \code{x}}
}
\examples{
do.on( 1:7, sum(1:.))
#  1  2  3  4  5  6  7
# 1  3  6 10 15 21 28
# note the numeric "names" in the first row
FOR( 1:3, sum(1:.))
}
\keyword{misc}

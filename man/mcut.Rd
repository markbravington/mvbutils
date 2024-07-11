\name{mcut}
\alias{mcut}
\alias{mintcut}
\title{Put reals and integers into specified bins, returning factors.}
\description{Put reals and integers into specified bins, returning ordered factors. Like \code{\link{cut}} but for human use.
}
\usage{
mcut( x, breaks, pre.lab='', mid.lab='', post.lab='', digits=getOption( 'digits'))
mintcut( x, breaks=NULL, prefix='', all.levels=, by.breaks=1)
}
\arguments{
\item{ x}{(numeric vector) What to bin-- will be coerced to integer for \code{mintcut}}
\item{ breaks}{(numeric vector) LH end of each bin-- should be increasing. Values of \code{x} exactly on the LH end of a bin will go into that bin, not the previous one. For \code{mintcut}, defaults to equal-size bins across the range of \code{x}, where bin size is set from \code{by.breaks} which itself defaults to 1. For \code{mcut}, should start with -Inf if necessary, but should not finish with Inf unless you want a bin for Infs only.}
\item{ prefix, pre.lab}{(string) What to prepend to the factor labels-- e.g. "Amps" if your original data is about Amps.}
\item{ mid.lab}{"units" to append to numeric vals \emph{inside} factor labels. Tends to make the labels harder to read; try using \code{post.lab} instead.}
\item{ post.lab}{(string) What to append to the factor labels.}
\item{ digits}{(integer) How many digits to put into the factor labels.}
\item{ all.levels}{if FALSE, omit factor levels that don't occur in \code{x}. To override "automatically", just set the "all.levels" attribute of \code{breaks} to anything non-NULL; useful e.g. if you are repeatedly calling \code{mintcut} with the same \code{breaks} and you always want \code{all.levels=TRUE}.}
\item{ by.breaks}{for \code{mintcut} when default \code{breaks} is used, to set the bin size.}
}
\details{Values of \code{x} below \code{breaks[1]} will end up as NAs. For \code{mintcut}, factor labels (well, the bit after the \code{prefix}) will be of the form "2-7" or "3" (if the bin range is 1) or "8+" (for last in range). For \code{mcut}, labels will look like this (apart from the \code{pre.lab} and \code{post.lab} bits): "[<0.25]" or "[0.25,0.50]" or "[>=0.75]".
}
\examples{
set.seed( 1)
mcut( runif( 5), c( 0.25, 0.5, 0.75))
# [1] [0.25,0.50] [0.25,0.50] [0.50,0.75] [>=0.75]     [<0.25]
# Levels: [<0.25] [0.25,0.50] [0.50,0.75] [>=0.75]
 mcut( runif( 5), c( 0.25, 0.5, 0.75), pre.lab='A', post.lab='B', digits=1)
# [1] A[>=0.8]B    A[>=0.8]B    A[0.5,0.8]B A[0.5,0.8]B A[<0.2]B
# Levels: A[<0.2]B A[0.2,0.5]B A[0.5,0.8]B A[>=0.8]B
mintcut( 1:8, c( 2, 4, 7))
# [1] <NA> 2-3  2-3  4-6  4-6  4-6  7+   7+
# Levels: 2-3 4-6 7+
mintcut( c( 1, 2, 4)) # auto bins, size defaulting to 1
# [1] 1  2  4+
# Levels: 1 < 2 < 3 < 4+
mintcut( c( 1, 2, 6), by=2) # auto bins of size 2
# [1] 1-2 1-2 5+
# Levels: 1-2 < 3-4 < 5+
}
\keyword{misc}

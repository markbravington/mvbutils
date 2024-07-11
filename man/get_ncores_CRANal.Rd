\name{get_ncores_CRANal}
\alias{get_ncores_CRANal}
\title{Detect number of CPU cores in CRAN-robust way}
\description{
This is only relevant for parallel code inside package examples and vignettes. In real applications, you would call \code{parallel::detectCores} (qv) and then decide how many of those to use. But CRAN enforces a limit of (currently) 2 cores when checking examples (and presumably vignettes etc)--- and doesn't give you any way to find out what the limit is from code; it just gives an error. Since the \emph{entire} point of parallel processing is to use lots of cores if available, CRAN makes it impossible to demonstrate anything realistic in examples, if you want to get them past CRAN. You ccan of course limit the number of cores to 2 purely for CRAN's benefit, but then you are castrating your code for real tests.

To avoid this lunacy, you can call this function inside your example/vignette. It counts roughly how many cores are \emph{allowed} (ie won't cause an error), up to the limit requested by its argument (which you would get from \code{detectCores} etc). Actually it only goes in multiples of 2, so it won't necessarily give you the max.

In real code as opposed to examples, you probably don't want this; rather call \code{parallel::detectCores} and then decide for yourself, as I mention in two other places in this helpfile!

The "algorithm" is to start with 2 cores and keep doubling until there's an error (trapped with \code{try}), or until the target is reached. This will at least be "quick" on CRAN. But it always means setting up and destroying a cluster at least twice, which is \bold{inefficient} if you can just decide for yourself! And if you are really happy just having your example use 2 cores, then just use 2 cores in the example--- don't bother with this!

At present, this code is only in \code{mvbutils} so I can make the \code{\link{numvbderiv_parallel}} (qv) example run nicely; but I guess I might use it in other packages eventually. Parallel stuff in R{} is messy; be warned.
}
\usage{get_ncores_CRANal( target)
}
\arguments{\item{ target}{How many cores you would like. Presumably, requires a previous call to \code{parallel::detectCores} and also a sensible decision on your part.}
}
\value{Integer
}
\examples{
\dontrun{
"See numvbderiv example"
}
}
\keyword{parallel
}

\name{setup.mcache}
\alias{setup.mcache}
\title{Cacheing objects for lazy-load access}
\description{Manually setup existing reference objects-- rarely used explicitly.
}
\usage{setup.mcache( envir, fpath, refs)
}
\arguments{
\item{ envir}{environment or position on the search path.}
\item{ fpath}{directory where "obj*.rda" files live.}
\item{ refs}{which objects to handle-- all names in the \code{mcache} attribute of \code{envir}, by default}
}
\details{Creates an active binding in \code{envir} for each element in \code{refs}. The active binding for an object \code{myobj} will be a function which keeps the real data in its own environment, reading and writing it as required. Writing a new value will give \code{attr( envir, "mcache")[ "myobj"]} a negative sign. This signals that the "obj*.rda" file needs updating, and the next \code{\link{Save}} (or \code{\link{move}} or \code{\link{cd}}) command will do so. [The "*" is the absolute value of \code{attr( envir, "mcache")[ "myobj"]}.] One wrinkle is that the "real data" is initially a \code{promise} created by \code{delayedAssign}, which will fetch the data from disk the first time it is needed.
}
\seealso{\code{\link{mlazy}}, \code{makeActiveBinding}, \code{delayedAssign}
}
\author{Mark Bravington}
\keyword{programming}
\keyword{data
}

\name{RENEWS}
\alias{RENEWS}
\title{Markdownize & reverse NEWS object}
\description{Probably only for me. Each of my maintained packages has a \code{<mypack>.NEWS} object (character vector) in a pretty arbitrary format which I might as well markdownize, so that \code{utils::news} can process it. Reverse order (previously, most recent came last).) Remove dates.
}
\usage{RENEWS( pkg, character.only = FALSE)
}
\arguments{
\item{ pkg}{eg \code{..debug} or \code{debug} or \code{"debug"} (the latter a string)}
\item{ character.only}{for programmatic use, enforce string format as \code{pkg}}
}
\value{Modified \code{<pkg>.NEWS} of class \code{cat}, so you can check it before manually assiging to eg \code{..<mypack>$<mypack>.NEWS}. Likely to need cleanup with \code{\link{fixr}} afterwards!
}
\examples{
\dontrun{
RENEWS( mvbutils)
}
}
\keyword{internal
}

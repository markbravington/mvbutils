\name{sleuth}
\alias{sleuth}
\title{Generalized version of find}
\description{Looks for objects that regex-match \code{pattern}, in all attached workspaces (as per \code{search()}) and any maintained packages (see \code{\link{maintain.packages}}).
}
\usage{sleuth(pattern, ...)
}
\arguments{
\item{ pattern}{regex}
\item{ ...}{other args to \code{grep}, e.g. \code{perl=TRUE} or \code{ignore.case=TRUE}}
}
\value{A list of environments containing one or more matching objects, with the object names returned as a character vector within each list element.
}
\seealso{\code{\link{search.for.regexpr}}
}
\examples{
sleuth( '^rm')
# On my setup, that currently gives:
#$ROOT
#[1] "rmsrc"
#
#$`package:stats`
#[1] "rmultinom"
#
#$`package:base`
#[1] "rm"
#
#$mvbutils
#[1] "rm.pkg"
#
#$handy2
#[1] "rmultinom"
#
}
\keyword{misc}

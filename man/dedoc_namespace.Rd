\name{dedoc_namespace}
\alias{dedoc_namespace}
\title{Remove doc attributes when package loads}
\description{Suppose you want to keep plain-text "doc" attributes attached to your function code even in the package source (as opposed to in a private version of the package). You probably don't want them around after the package loads for real, though. In that case, you can stick a call to \code{dedoc_namespace} at the end of your \code{.onLoad} and everything should be copacetic.
}
\usage{dedoc_namespace(ns)
}
\arguments{\item{ ns}{Name of the package, or its namespace environment.}
}
\seealso{write_sourceable_function, pre.install
}
\examples{
\dontrun{
# Put this into your package:
.onLoad <- function( libname, pkgname)\{
  # stuff for .onLoad(), or no stuff
  dedoc_package( pkgname)
\}
}
}
\keyword{misc}

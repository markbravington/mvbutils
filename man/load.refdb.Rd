\name{load.refdb}
\alias{load.refdb}
\title{Cacheing objects for lazy-load access}
\description{\code{load.refdb} is like \code{load}, but automatically calls \code{\link{setup.mcache}} to create access arrangements for cached objects. You probably don't need to call it directly.
}
\usage{load.refdb( file, envir, fpath=attr( envir, "path"))
}
\arguments{
\item{ file}{a filename relative to \code{fpath}}
\item{ envir}{an environment or (more usually) a position on the search path (numeric or character)}
\item{ fpath}{a directory. Usually the default will do.}
}
\seealso{\code{\link{mlazy}}, \code{\link{setup.mcache}}
}
\author{Mark Bravington}
\keyword{internal
}

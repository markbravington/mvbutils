\name{full.path}
\alias{full.path}
\title{Expand relative file path}
\description{\code{path} is expanded relative to \code{start}, with any \code{.} being eliminated and any \code{..} being treated as "go back one step". If \code{path} doesn't start with a \code{.} or \code{..}, \code{start} is ignored. Might be Windows-specific but probably fairly safe in general. NB that all separators in \code{path} and \code{start} must be "/".
}
\usage{full.path(path, start)
}
\arguments{
\item{ path}{character(1)}
\item{ start}{character(1), defaulting to \code{.}}
}
\keyword{internal
}

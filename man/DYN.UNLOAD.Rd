\name{DYN.UNLOAD}
\alias{DYN.UNLOAD}
\title{Unload DLL easily}
\description{
R{}'s \code{\link{dyn.unload}} is ridiculously hard to use in practice, because it requires complete paths. These \emph{can} be extracted from \code{\link{getLoadedDLLs}}, but only with ridiculous amounts of effort and tricks that I always forget. Use \code{DYN.UNLOAD} instead with just the basename of the DLL(s) you actually want to unload.

Note that there can be multiple versions of a DLL loaded at the same time, with the same "name" (according to \code{getLoadedDLLs}) but different paths. This will unload the first one (only), so you may need to call it repeatedly.
}
\usage{DYN.UNLOAD( dllnames, warn_if_not_loaded=TRUE)
}
\arguments{
\item{ dllnames}{Usually one string, eg "my_dodgy_C_code", but you can do several at once (in a character vector, obvs).}
\item{ warn_if_not_loaded}{Pretty self-explanatory.}
}
\value{The satisfaction of actually having cleared the bloody thing out of memory, eg so that you can delete the file.
}
\examples{
\dontrun{
DYN.UNLOAD( "offending_C_code")
}
}
\keyword{misc}

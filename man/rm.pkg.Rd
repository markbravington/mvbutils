\name{rm.pkg}
\alias{rm.pkg}
\alias{remove.from.package}
\title{Remove object(s) from maintained package}
\description{Remove object(s) from maintained package. If the package is loaded, then objects are also removed from the search path version if any, the namespace if any, any importing namespaces, and any S3 method table. \code{remove.from.package} is a synonym. You will be prompted about whether to auto-save the maintained package.
}
\usage{
rm.pkg( pkg, ..., list = NULL, save.=NA)
# remove.from.package( pkg, ..., list=NULL)
remove.from.package( ...) # really has same args as 'rm.pkg'
}
\arguments{
\item{ pkg}{(string, or environment) package name or environment, e.g. \code{..mypack}}
\item{ ...}{unquoted object names to remove}
\item{ list}{character vector alternative to ..., which is ignored if \code{list} is set}
\item{ save.}{For internal use--- leave this alone!}
}
\details{For now, methods are only removed from the \bold{base} S3 methods table; if new S3 generics have been defined in loaded packages, and you are trying to remove a method for such a generic, then it won't be removed. I could implement this feature if anyone really wants it.
}
\seealso{\code{\link{maintain.packages}}
}
\examples{
\dontrun{
rm.pkg( "mypackage", foo, bar)
rm.pkg( "mypackage", list=cq( foo, bar))
rm.pkg( ..mypackage, list=cq( foo, bar))
}
}
\keyword{misc
}

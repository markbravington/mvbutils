\name{dont.lock.me}
\alias{dont.lock.me}
\title{Prevent sealing of a namespace, to facilitate package maintenance.}
\description{Call \code{dont.lock.me()} during a \code{.onLoad} to stop the namespace from being sealed. This will allow you to add/remove objects to/from the namespace later in the R{} session (in a sealed namespace, you can only change objects, and you can't unseal a namespace retrospectively). There could be all sorts of unpleasant side-effects. Best to leave it to \code{\link{maintain.packages}} to look after this for you...
}
\usage{
# default of env works if called directly in .onLoad
dont.lock.me( env=environment( sys.function( -1)))
}
\arguments{\item{ env}{the environment to not lock.}
}
\details{\code{dont.lock.me} hacks the standard \code{lockEnvironment} function so that locking won't happen if the environment has a non-NULL \code{dont.lock.me} attribute. Then it sets this attribute for the namespace environment.
}
\examples{
\dontrun{
# This unseals the namespace of MYPACK only if the option "maintaining.MYPACK" is non-NULL:
.onLoad <- function( libname, pkgname) \{
  if( !is.null( getOption( 'maintaining.' \%&\% pkgname)))
    mvbutils:::dont.lock.me()
\}
}
}
\keyword{programming
}

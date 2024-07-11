\name{check.patch.versions}
\alias{check.patch.versions}
\title{Check consistency of maintained package versions}
\description{
Utility to compare version numbers of the different "instances" of one of your maintained packages. Only the most up-to-date folders relevant to the running R{} version are checked; see \code{\link{mvbutils.packaging.tools}}.

The "instances" checked are:

\itemize{
\item the task package itself (in eg \code{..mypack$mypack.VERSION})
\item the source package created by \code{\link{pre.install}}
\item the installed package, maintained by \code{\link{patch.install}}
\item the tarball package, created by \code{\link{build.pkg}}
\item the binary package, created by \code{\link{build.pkg.binary}}
}

The \code{care} argument controls what's shown. Mismatches when \code{care="installed"} should be addressed by \code{\link{patch.install}}, because something has gotten out-of-synch (probably when maintaining the same version of a package for different R{} versions). Mismatches with the built ("tarball" and "binary") packages are not necessarily a problem, just an indication of work-in-progress.
}
\usage{check.patch.versions(care = NULL)
}
\arguments{\item{ care}{if non-NULL, a character vector with elements in the set "installed", "source", "tarball", and "binary". Only packages where there's a version mismatch between these fields and the task package version will be shown.}
}
\value{A character matrix with maintained packages as rows, and the different instances as columns. "NA" indicates that a version couldn't be found.
}
\keyword{misc}

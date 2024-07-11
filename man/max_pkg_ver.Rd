\name{max_pkg_ver}
\alias{max_pkg_ver}
\title{Max package version}
\description{Finds the highest version number of an installed package in (possibly) \emph{several} libraries. Mainly for internal use in \code{mvbutils}, but might come in handy if your version numbers have gotten out-of-synch eg with different R{} versions. On my setup, all my "non-base" libraries are folders inside "d:/rpackages", with folder names such as "R2.13"; my \code{.First} sets \code{.libPaths()} to all of these that are below the running version of R{} (but that are still legal for that R{} version; so for R{} > 3.0, folders named "R2.xxxx" would be excluded). Hence I can call \code{max_pkg_ver( mypack, "d:/rpackages")} to find the highest installed version in all these subfolders.
}
\usage{
max_pkg_ver(pkg, libroot, pattern = "^[rR][ -]?[0-9]+")
    # NB named with underscores to avoid interpretation as S3 method
}
\arguments{
\item{ pkg}{character, the name of the package}
\item{ libroot}{folder(s) to be searched recursively for package \pkg{pkg}}
\item{ pattern}{what regexp to use when looking for potential libraries to recurse into}
}
\value{A \code{numeric_version} object for the highest-numbered installation, with value \code{numeric_version("0")} if no such package is found. If \code{libroot} is a single library containing the package, the result will equal \code{packageVersion( pkg, limbroot)}.
}
\examples{
max_pkg_ver( "mvbutils", .libPaths())
}
\keyword{misc}

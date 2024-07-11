\name{bugfix_Rd2roxygen}
\alias{bugfix_Rd2roxygen}
\title{Like Rd2roxygen, but fixing some bugs}
\description{
Like package \pkg{Rd2roxgyen} (qv), for modding the R{} source in an existing source package to add Roxgyen comments (i.e., documentation and export instructions). Package \pkg{Rd2roxgyen} does most of the work, but it has a couple of bugs and I don't think they are likely to get fixed soon (one of them is "a feature").

This is called internally by \code{\link{pre.install}}, if "RoxygenNote" is found in the DESCRIPTION file, but can also be called manually.

Personally I don't like Roxygen--- to me it seems a bad implementation of a reasonable idea (keep documentation tightly linked with code, and avoid markup complexity) for which there are better and simpler ways--- but Others do. So this might help, especially if Others are collaborating with non-Roxygenites..
}
\usage{bugfix_Rd2roxygen( sourcedir, pkg = basename(sourcedir), nsinfo = NULL)
}
\arguments{
\item{ sourcedir}{folder containing the source package (so it should contain a "DESCRIPTION" file, a folder called "R", and so on)}
\item{ pkg}{name of the package, deduced from \code{sourcedir} if not supplied}
\item{ nsinfo}{info slurped from the NAMESPACE file (actually just about S3 methods, which \code{Rd2roxygen} inexplicably ignores). Used internally by \code{pre.install} for efficiency, but if you are calling this manually, you can leave it and the NAMESPACE file itself will be used}
}
\value{Alters the file "<sourcedir>/R/<pkg>.R". Also, if there's a file "<sourcedir>/R/<pkg>-package.R", then a "Collate" field is added (or modified) to the DESCRIPTION file, to make sure that the package-source is collated \emph{last}. This is a good idea, for reasons that I can no longer remember.
}
\keyword{misc}

\name{lsize}
\alias{lsize}
\title{Report objects and their memory sizes}
\description{
\code{lsize} is like \code{ls}, except it returns a numeric vector whose names are the object names, and whose elements are the object sizes. The vector is sorted in order of increasing size. \code{lsize} avoids loading objects cached using \code{\link{mlazy}}; instead of their true size, it uses the size of the file that stores each cached object, which is shown as a \emph{negative} number. The file size is typically smaller than the size of the loaded object, because \code{\link{mlazy}} saves a compressed version. NB that \code{lsize} will scan \emph{all} objects in the environment, including ones with funny names, whereas \code{ls} does so only if its \code{all.names} argument is set to TRUE.

Missing objects should return 0 (which may or may not be exactly correct!). You won't normally get that, but see \bold{Examples} for a perverse case.
\subsection{Environments}{
If there are environment-objects (which are really symbols that \emph{point} to frames--- see R) within the very environment you are \code{lsize}ing, what should their size be? There is no perfect answer. R{} will tell you it's "56 bytes" and that's what you'll get with the default \code{recursive=0}. However, each environment could be holding arbitrarily large objects--- so you might want to know how much memory they are "really" taking. You can do so by setting \code{recursive} to a positive number, which also controls the \emph{depth} of recursion (because environments can themselves contain other environments).

However-however, those environments might be innocuous things that just refer to shared system-y ones (eg namespaces of packages, copies of \code{.GlobalEnv}, etc), in which case they are not costing any memory. And if two symbols refer to the same actual environment, they are duplicates and second one is not taking any extra real memory. So \code{lsize} tries to keep track of such cases (whenever \code{recursive>0}), and \emph{not} to incorporate their memory-use; whether it does so optimally, is another Q. NB that for duplicates, only the alphabetically-first will be recursed.

There are lots of places that environments can lurk \emph{within} other objects: notably, environments-of-functions, and formulae/results of calls to \code{lm} etc. These can take huge amounts of memory, sometimes manifest only when saving/loading. \code{lsize} does not currently attempt to measure those; but see \code{\link{find.lurking.envs}}.
}
}
\usage{lsize( envir=.GlobalEnv, recursive=0)
}
\arguments{
\item{ envir}{where to look for the objects. Will be coerced to environment, so that e.g. \code{lsize( 2)} and \code{lsize( "package:mvbutils")} work. \code{envir} can be a \code{sys.frame}-- useful during debugging.}
\item{ recursive}{depth of recursion to allow, for objects that are themselves environments. See .ENVIRONMENTS.}
}
\value{Named numeric vector.
}
\seealso{\code{ls}, \code{\link{mlazy}}, \code{\link{find.lurking.envs}}
}
\author{Mark Bravington}
\examples{
# Current workspace
lsize()
# Contrived example to show objects in a function's environment
nonsense <- function(..., a, b, c) lsize( environment())
try( # this might be fragile with missings; OK in R4.3
  nonsense()
)
# a, b, c are all missing; this example might break in future R versions
# ...   a   b   c
#   0   0   0   0
}
\keyword{programming}
\keyword{misc
}

\name{dont.lockBindings}
\alias{dont.lockBindings}
\title{Helper for live-editing of packages}
\description{Normally, objects in a NAMESPACEd package are locked and can't be changed. Sometimes this isn't what you want; you can prevent it by calling \code{dont.lockBindings} in the \code{.onLoad} for the package. For user-visible objects (i.e. things that end up in the "package:blah" environment on the search path), you can achieve the same effect by calling \code{dont.lockBindings} in the package's \code{.onAttach} function, with \code{namespace=FALSE}.
}
\usage{ dont.lockBindings( what, pkgname, namespace.=TRUE)
}
\arguments{
\item{  what}{(character) the names of the objects to not lock.}
\item{  pkgname}{(string) the name of the package. As you will only use this inside \code{.onLoad}, you can just set this to \code{pkgname} which is an argument of \code{.onLoad}.}
\item{  namespace.}{TRUE to antilock in the namespace during \code{.onLoad}; FALSE to antilock in the visible manifestation of the package.}
}
\details{Locking occurs after \code{.onLoad} / \code{.onAttach} are called so, to circumvent it, \code{dont.lockBindings} creates a hook function to be called after the locking step.
}
\examples{
\dontrun{
library( debug)
debug:::.onLoad # d.lB is called to make 'tracees' editable inside 'debug's namespace.
debug:::.onAttach # d.lB is called to make 'tracees' editable in the search path
# NB also that an active binding is used to ensure that the 'tracees' object in the search...
#... path is a "shadow of" or "pointer to" the one in 'debug's namespace; the two cannot get...
#... out-of-synch
}
}
\seealso{\code{lockBinding}, \code{setHook}
}
\keyword{misc}

\name{do.in.envir}
\alias{do.in.envir}
\title{Modify a function's scope}
\description{\code{do.in.envir} lets you write a function whose scope (enclosing environment) is defined at runtime, rather than by the environment in which it was defined.
}
\usage{
# Use only as wrapper of function body, like this:
# my.fun <- function(...) do.in.envir( fbody, envir=)
# ... should be the arg list of "my.fun"
# fbody should be the code of "my.fun"
do.in.envir( fbody, envir=parent.frame(2)) # Don't use it like this!
}
\arguments{
\item{ fbody}{the code of the function, usually a braced expression}
\item{ envir}{the environment to become the function's enclosure}
}
\details{
By default, a \code{do.in.envir} function will have, as its enclosing environment, the environment in which it was \emph{called}, rather than \emph{defined}. It can therefore read variables in its caller's frame directly (i.e. without using \code{get}), and can assign to them via \code{<<-}. It's also possible to use \code{do.in.envir} to set a completely different enclosing environment; this is exemplified by some of the functions in \code{debug}, such as \code{go}.

Note the difference between \code{do.in.envir} and \code{\link{mlocal}}; \code{\link{mlocal}} functions evaluate in the frame of their caller (by default), whereas \code{do.in.envir} functions evaluate in their own frame, but have a non-standard enclosing environment defined by the \code{envir} argument.

Calls to e.g. \code{sys.nframe} won't work as expected inside \code{do.in.envir} functions. You need to offset the frame argument by (at time of writing this documentation...) 5, so that \code{sys.parent()} should be replaced by \code{sys.parent( 5)} and \code{sys.call} by \code{sys.call(-5)}. In future, 5 may not be the right magic number.

\code{do.in.envir} functions are awkward inside namespaced packages, because the code in \code{fbody} will have "forgotten" its original environment when it is eventually executed. This means that objects in the namespace will not be found.

The \pkg{debug} package tries to \code{mtrace} inside \code{do.in.envir} functions; this used to work, but hasn't been recently tested in R4.1 where a few internal R{} deepshit mysteries seem to have changed.
}
\value{Whatever \code{fbody} returns.
}
\examples{
fff <- function( abcdef) ffdie( 3)
ffdie <- function( x) do.in.envir( \{ x+abcdef\} )
fff( 9) # 12; ffdie wouldn't know about abcdef without the do.in.envir call
# Show sys.call issues
# Note that the "envir" argument in this case makes the
# "do.in.envir" call completely superfluous!
ffe <- function(...) do.in.envir( envir=sys.frame( sys.nframe()), sys.call( -5))
ffe( 27, b=4) # ffe( 27, b=4)
}
\seealso{\code{\link{mlocal}}
}
\author{Mark Bravington}
\keyword{programming}
\keyword{utilities
}

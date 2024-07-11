\name{mlocal}
\alias{mlocal}
\title{Macro-like functions}
\description{\code{mlocal} lets you write a function whose statements are executed in its caller's frame, rather than in its own frame.
}
\usage{
# Use only as wrapper of function body, like this:
# my.fun <- function(..., nlocal=sys.parent()) mlocal( expr)
# ... should be replaced by the arguments of "my.fun"
# expr should be replaced by the code of "my.fun"
# nlocal should always be included as shown
mlocal( expr) # Don't use it like this!
}
\arguments{\item{ expr}{the function code, normally a braced expression}
}
\details{
Sometimes it's useful to write a "child" function that can create and modify variables in its parent directly, without using \code{assign} or \code{<<-} (note that \code{<<-} will only work on variables that exist already). This can make for clearer, more modular programming; for example, tedious initializations of many variables can be hidden inside an \code{initialize()} statement. The definition of an \code{mlocal} function does not have to occur within its caller; the \code{mlocal} function can exist as a completely separate R{} object.

\code{mlocal} functions can have arguments just like normal functions. These arguments will temporarily hide any objects of the same name in the \code{nlocal} frame (i.e. the calling frame). When the \code{mlocal} function exits, its arguments will be deleted from the calling frame and the hidden objects (if any) will be restored. Sometimes it's desirable to avoid cluttering the calling frame with variables that only matter to the \code{mlocal} function. A useful convention is to "declare" such temporary variables in your function definition, as defaultless arguments after the \code{nlocal} argument.

The \code{nlocal} argument of an \code{mlocal} function-- which must ALWAYS be included in the definition, with the default specified as \code{sys.parent()}-- can normally be omitted when invoking your \code{mlocal} function. However, you will need to set it explicitly when your function is to be called by another, e.g. \code{lapply}; see the third example. A more daring usage is to call e.g. \code{fun.mlocal(nlocal=another.frame.number)} so that the statements in \code{fun.mlocal} get executed in a completely different frame. A convoluted example can be found in the (internal) function \code{find.debug.HQ} in the \pkg{debug} package, which creates a frame and then defines a large number of variables in it by calling \code{setup.debug.admin(nlocal=new.frame.number)}. As of 2016, you can also set \code{nlocal} to be an environment.

\code{mlocal} functions can be nested, though this gets confusing. By default, all evaluation will happen in the same frame, that of the original caller.

Note that (at least at present) all arguments are evaluated as soon as your \code{mlocal} function is invoked, rather than by the usual lazy evaluation mechanism. Missing arguments are still OK, though.

If you call \code{return} in an \code{mlocal} function, you must call \code{\link{local.return}} too.

\code{on.exit} doesn't work properly. If you want to have exit code in the \code{mlocal} function itself, use \code{\link{local.on.exit}}. I can't find any way to set the exit code in the calling function from within an \code{mlocal} function. (Not checked for some years)

Frame-dependent functions (sys.parent()) etc. will not do what you expect inside an \code{mlocal} function. For R{} versions between at least 1.8 and 2.15, calling the \code{mvb...} versions will return information about the \bold{caller} of the current \code{mlocal()} function caller (or the original caller, if there is a chain of \code{mlocal}s). For example, \code{mvb.sys.function()} returns the definition of the caller, and \code{mvb.sys.parent()} the frame of the caller's parent. Note that \code{sys.frame( mvb.sys.nframe())} gives the current environment (i.e. where all the variables live), because this is shared between the caller and the \code{mlocal} function. Other behaviour seems to depend on the version of R{}, and in R{} 2.15 I don't know how to access the definition of the \code{mlocal} function itself. This means, for example, that you can't reliably access attributes of the \code{mlocal} function itself, though you can access those of its caller via e.g. \code{attr( mvb.sys.function(), "thing")}.
}
\value{As per your function; also see \code{\link{local.return}}.
}
\examples{
# Tidiness and variable creation
init <- function( nlocal=sys.parent()) mlocal( sqr.a <- a*a)
ffout <- function( a) \{ init(); sqr.a \}
ffout( 5) # 25
# Parameters and temporary variables
ffin <- function( n, nlocal=sys.parent(), a, i) mlocal(\{
    # this "n" and "a" will temporarily replace caller's "n" and "a"
    print( n)
    a <- 1
    for( i in 1:n)
      a <- a*x
    a
  \})
x.to.the.n.plus.1 <- function( x, n) \{
    print( ffin( n+1))
    print( n)
    print( ls())
  \}
x.to.the.n.plus.1( 3, 2) # prints as follows:
# [1] 3 (in "ffin")
# [1] 27 (result of "ffin")
# [1] 2 (original n)
# [1] "n" "x" (vars in "x.to.the..."-- NB no a or i)
# Use of "nlocal"
ffin <- function( i, nlocal=sys.parent()) mlocal( a <- a+i )
ffout <- function( ivec) \{ a <- 0; sapply( ivec, ffin, nlocal=sys.nframe()) \}
ffout( 1:3) # 1 3 6
}
\seealso{\code{\link{local.return}}, \code{\link{local.on.exit}}, \code{\link{do.in.envir}}, \code{\link{localfuncs}}, and R-news 1/3 2001 for a related approach to "macros"
}
\author{Mark Bravington}
\keyword{programming
}

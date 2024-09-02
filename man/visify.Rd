\name{visify}
\alias{visify}
\title{Make a function autoprint all its doings}
\description{
Occasionally you want a function to do a whole bunch of things, and print the results; you might be thinking about executing its code directly in a "script", but you don't want to be cluttering up the workspace. Then you can wrap the body of your function in a call to \code{visify}, and R{} will act as if you \code{source} the corresponding script.

The output isn't particularly beautiful, but it's jolly handy for eg routine diagnostics when fitting a series of models. \code{visify} is experimental as of package \pkg{mvbutils} v2.9.18, and I might add features over time, eg to make output look prettier and give better options for hiding things.
}
\usage{
# Never use it like this...
visify(exprs, local = parent.frame(), ...)
# ... always like this:
# my_autoprinting_function <- function(<args>) visify( \{<body>\})
}
\arguments{
\item{ exprs}{The body of your function}
\item{ local}{Normally leave this alone; it's the environment to run \code{exprs} in}
\item{ ...}{other args to \code{\link{withAutoprint}}}
}
\details{
Compound statements, such as \code{if} and \code{for}, are not printed "internally", only the final outcome (which is NULL for \code{for}). Their code is still printed, though; you could try adding the argument \code{max.deparse.length=100} or similar to prune it; see \bold{Examples}.

If you want certain statements in your function to execute without autoprinting their output (eg because it is an enormous and cluttery intermediate calculation), wrap it or them in \code{{ <dontshowthisbit>; NULL}}. Again, their code will still be printed regardless.

The trick behind \code{visify} is to use \code{\link{withAutoprint}}, but it's not obvious exactly how to. I was encouraged by:

\code{https://stackoverflow.com/questions/58285497}

though I did not use exactly the solutions there (because I wanted a slightly different "flow").
}
\seealso{\code{withAutoprint}
}
\examples{
# Note the use of max.deparse.length param
testvis <- function( xx, MDL=Inf) visify( max.deparse.length=MDL, \{
  yy <- xx + 1
  # Comments show up, too...
  for( i in 1:5) yy <- yy + 1
  # Dont' want to show gory details of next "block"
  \{
    xx <- xx+1
    xx <- xx + 2
    NULL # that's all you'll see
  \}
  xx*yy
\})
testvis( 3)
testvis( 3, MDL=100)
testvis( 3, MDL=50) # too terse
}
\keyword{misc}

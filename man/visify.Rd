\name{visify}
\alias{visify}
\title{Make a function autoprint all its doings}
\description{
Occasionally you want a function to do a whole bunch of things, and print the results; you might be thinking about executing its code directly in a "script", but you don't want to be cluttering up the workspace. Then you can wrap the body of your function in a call to \code{visify}, and R{} will act as if you \code{source} the corresponding script.

You can also use \code{visify} inside a function, to just display certain bits. (Well, no; as of 2.9.18, the first part before \code{visify} \emph{always} seems to be shown. But at least you can hide the return value.) For example, it's sometimes useful to not visify the entire return-value of a function, even though you want to "show the rest of your working". In that case, you can just not return the value within the \code{visify} block, but separately afterwards; see \bold{Examples}.

The output isn't particularly beautiful, but it's jolly handy for eg routine diagnostics when fitting a series of models. \code{visify} is experimental as of package \pkg{mvbutils} v2.9.18, and I might add features over time, eg to make output look prettier and give better options for hiding things.
}
\usage{
# Never use it like this...
visify(exprs, local = parent.frame(), prompt.echo='', ...)
# ... always like this, for an entire function:
# my_autoprinting_function <- function(<args>) visify( \{<body>\})
# ... or just as part of one:
# my_part_apf <- function( <args>)\{ <hide>; visify(\{<shown>\}); <returns>\}
}
\arguments{
\item{ exprs}{The body of your function}
\item{ local}{Normally leave this alone; it's the environment to run \code{exprs} in}
\item{ prompt.echo}{what to print at the start of each line. Default is nothing.}
\item{ ...}{other args to \code{\link{withAutoprint}}, such as \code{max.deparse.length} or \code{width.cutoff} .}
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
# Basic: show it all
tv1 <- function( xx) visify(\{
  yy <- xx + 1
  # Comments show up, too...
  for( i in 1:5) yy <- yy + 1
  # ... but loops only show end result; ditto ifs
  # and other compound statements
  xx <- xx+1
  xx <- xx + 2
  xx*yy
\})
# Note the use of max.deparse.length param. Also try width.cutoff
tv2 <- function( xx, MDL=Inf) visify( max.deparse.length=MDL, \{
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
tv2( 3)
tv2( 3, MDL=100)
tv2( 3, MDL=50) # too terse
# Hide boring stuff
tv2 <- function( xx)\{
  # I don't understand why this bit _before_ visify() is shown
  yy <- xx + 1
  for( i in 1:5) yy <- yy + 1
  visify(\{
    xx <- xx+1
    xx <- xx + 2
    NULL # that's all you'll see
  \})
  # At least the return-value isn't!
invisible( rep( xx*yy, 9999)) # don't wanna show this!
\}
tv2( 3)
}
\keyword{misc}

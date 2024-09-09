\name{visify}
\alias{visify}
\title{Make a function autoprint all its doings}
\description{
Occasionally you want a function to do a whole bunch of things, and print the results as it goes along; you might be thinking about executing its code directly in a "script", but you don't want to be cluttering up the workspace. If so, you can wrap the body of your function in a call to \code{visify}, and R{} will act as if you \code{source} the corresponding script. The output isn't particularly beautiful, but it's jolly handy for eg routine diagnostics when fitting a series of models.

You can also use \code{visify} inside a function, to just display certain bits. (Well, not entirely; as of 2.9.18, any code before \code{visify} \emph{always} seems to be shown. But at least you can hide the return value.) For example, it's sometimes useful to not visify the entire return-value of a function, even though you want to "show the rest of your working". In that case, you can just not return the value within the \code{visify} block, but separately afterwards; see \bold{Examples}.

\code{visify} is experimental as of package \pkg{mvbutils} v2.9.228, and I might add features over time, eg to make output look prettier and give better options for hiding things. At present, it deliberately strips all continuation lines of input, so you only see the first line of each statement (a blessing IME so far). More could be done; but I don't yet understand how all the options to \code{source} and \code{withAutoprint} work, and this is really a convenience function rather than something intended for producing report-quality output.
\subsection{To do}{
As of package \pkg{mvbutils} v 2.9.23 and package \pkg{debug} v 1.4.18, the latter does not currently handle \code{visify} nicely (in contrast to eg \code{evalq}, which is operationally very similar except WRTO output). What you have to do for now, is manually replace the call to \code{visify} with a call to \code{evalq} (and remove any extraneous arguments).

This kinda thing comes up often. I really need a general mechanism in the \pkg{debug} package, for any other package (or the user) to supply an \code{mtrace}-able version of their pet function.
}
}
\usage{
# Never use it like this...
visify(exprs, local = parent.frame(), prompt.echo='', ...)
# ... always like this, for an entire function:
# my_autoprinting_function <- function(<args>) visify( \{<body>\})
# ... or just as part of one:
# my_part_apf <- function( <args>)\{ visify(\{<shown>\}); <posthoc-and-returns>\}
}
\arguments{
\item{ exprs}{The body of your function}
\item{ local}{Normally leave this alone; it's the environment to run \code{exprs} in}
\item{ prompt.echo}{what to print at the start of each line. Default is nothing.}
\item{ ...}{other args to \code{\link{withAutoprint}}, such as \code{max.deparse.length} or \code{width.cutoff}.}
}
\details{
Compound statements, such as \code{if} and \code{for}, are not printed "internally", only the final outcome (which is NULL for \code{for}). The first line of the compound code is still printed, though.

If you want certain statements in your function to execute without autoprinting their output (eg because it is an enormous and cluttery intermediate calculation), wrap it or them in curlies, a la \code{{ <dont; show; these; outputss>; NULL}}. Again, the first line of code will be printed regardless--- so you could just make into a "Hide me!" comment, as per \bold{Examples}.

The trick behind \code{visify} is to use \code{\link{withAutoprint}}, but it's not obvious exactly how to do so. I was encouraged by:

\code{https://stackoverflow.com/questions/58285497}

However, I did not use exactly the solutions there, because I wanted a slightly different "flow".
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
tv1( 3)
# Note the use of max.deparse.length param. Also try width.cutoff
tv2 <- function( xx, MDL=Inf) visify( max.deparse.length=MDL, \{
  yy <- xx + 1
  # Comments show up, too...
  for( i in 1:5) yy <- yy + 1
  # Dont' want to show gory details of next "block"
  \{ # HIDE ME!
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

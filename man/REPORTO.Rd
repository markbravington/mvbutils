\name{REPORTO}
\alias{REPORTO}
\title{Stash variables in caller's environment}
\description{
\code{REPORTO} is a convenience function for use during model-fitting, when you have a hand-written "objective function" to optimize. Suppose your function \code{obfun} computes lots of jolly interesting intermediate quantities, which you would like to preserve somewhere, before the function exits and they vanish. Then, just insert a call eg \code{REPORTO( key_result, fascinating, important)} somewhere. You can make multiple calls to \code{REPORTO} (with different variables...) and they will all be stashed.

You should probably give \code{obfun} an environment before you do this, otherwise the interesting stuff will end up in \code{.GlobalEnv} (if you are lucky), resulting in clutter. You can also use \code{environment(obfun)} to stash data in, so that \code{obfun} will be able to just refer directly to it, again without cluttering up \code{.GlobalEnv}. That level of self-discipline is worth cultivating. See \bold{Examples}, and eg \code{?closure} for some kind of intro to R{}'s lexical-scoping rules, on which this all depends. There must be a more reader-friendly help link somewhere, though...

Of course, you can do all this with base-R commands anyway (see below). But a \bold{key reason} for using \code{REPORTO}--- at least if you are using the package \pkg{offarray}--- is that the package \pkg{offartmb} will automatically translate \code{REPORTO} calls into \code{RTMB::REPORT} calls, so your code can then run under package \pkg{RTMB} without further modification; see \code{offarray::reclasso}. Plus, even in normal R{} use, the \code{REPORTO( var1, var2)} syntax is clearer and easier.
\subsection{Pedants corner}{
HaRd-nuts will note that normal-R-use \code{REPORTO} is "just" syntactic sugar for the totally-self-explanatory idiom:

\preformatted{
  list2env( mget( c( "key_result", "fascinating", "important")),
    envir=environment( sys.function()))
}

And, yes, of course, in normal-R use you can also achieve the effect via \code{<<-} and \code{assign}. But the former requires you to pre-create the interesting things in \code{environment( obfun)}, the latter has pig-ugly syntax, and both require self-discipline, which is hateful to me anyway. However, if you really want to do all that, feel free! (And remember to write your own code to handle the \code{RTMB} case.)
}
}
\usage{REPORTO(..., names = NULL)
}
\arguments{
\item{ ...}{variables you want to stash--- unquoted. Can be empty.}
\item{ names}{A character vector with the names of \emph{additional} variables to stash.}
Thus, \code{REPORTO(myvar)} or \code{REPORTO(char="myvar")} have identical effects. The \code{names} argument is handy if you want to stash, say, all variables whose names begin with "ncomps_"--- then \code{REPORTO(names=ls(pattern="^ncomps_")}.
}
\value{\code{REPORTO} itself returns NULL; it is called for its side-effects.
}
\examples{
rego <- function( beta)\{
  v1 <- X \%*\% beta
  v2 <- y-v1
  REPORTO( v1, v2)
  ssq <- sum( v2*v2)
return( ssq)
\}
e <- new.env( parent=environment( rego))
e$X <- matrix( 1:6, 3, 2)
e$y <- 7:9
environment( rego) <- e
# Now rego will "know about" X & y...
rego( c( 1.6, 2.4))
# ... and it can stash its results there
e$v1
e$v2
}
\keyword{misc}

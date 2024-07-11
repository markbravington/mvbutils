\name{local.on.exit}
\alias{local.on.exit}
\title{Macro-like functions}
\description{\code{local.on.exit} is the analogue of \code{on.exit} for "nested" or "macro" functions written with \code{\link{mlocal}}.
}
\usage{
# Inside an 'mlocal' function of the form
# function( <<args>>, nlocal=sys.parent(), <<temp.params>>) mlocal(\{ <<code>> \})
local.on.exit( expr, add=FALSE)
}
\arguments{
\item{ expr}{the expression to evaluate when the function ends}
\item{ add}{if TRUE, the expression will be appended to the existing \code{local.on.exit} expression. If FALSE, the latter is overwritten.}
}
\details{
\code{on.exit} doesn't work properly inside an \code{\link{mlocal}} function, because the scoping is wrong (though sometimes you get away with it). Use \code{local.on.exit} instead, in exactly the same way. I can't find any way to set the exit code in the \bold{calling} function from within an \code{\link{mlocal}} function.

Exit code will be executed before any temporary variables are removed (see \code{\link{mlocal}}).
}
\examples{
ffin <- function( nlocal=sys.parent(), x1234, yyy) mlocal(\{
  x1234 <- yyy <- 1 # x1234 & yyy are temporary variables
  # on.exit( cat( yyy)) # would crash after not finding yyy
  local.on.exit( cat( yyy))
  \})
ffout <- function() \{
  x1234 <- 99
  ffin()
  x1234 # still 99 because x1234 was temporary
\}
ffout()
}
\seealso{\code{\link{mlocal}}, \code{\link{local.return}}, \code{local.on.exit}, \code{\link{do.in.envir}}, and R-news 1/3
}
\author{Mark Bravington}
\keyword{programming
}

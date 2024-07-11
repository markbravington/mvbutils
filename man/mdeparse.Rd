\name{mdeparse}
\alias{mdeparse}
\title{Deparsing nicelier}
\description{R{}'s built-in \code{\link{deparse}} rather messes up \code{a:=b}, \code{a?b}, and \code{?a}, destroying their elegance. This doesn't--- though for \code{?a} it does wrap the result in superfluous parentheses. There might be a few superfluous parens in other cases too, but \code{base::deparse} does that too (to safeguard reparsability).
\subsection{Detail}{
This works (quickly) by first using \code{substitute} to replace calls to \code{:=} and \code{?} by fake user-defined operators (\code{\%<something>\%}), then calling \code{deparse}, then \code{gsub} to re-replace the user-def-ops by \code{:=} and \code{?}. The \code{<something>} is meant to be a string that could never occur accidentally in \code{deparse} output (ie no character constant or name could ever deparse to it)--- I hope I got it right!

I am not \emph{entirely} sure about precedence. Because user-defined ops have higher precedence than \code{:=} or \code{?}, what I \emph{think} happens is that \code{deparse} puts in extra parentheses to safeguard precedence. I don't remove them, so I \emph{think} the end result is correct in the sense that \code{parse(text=mdeparse(expr))} will keep precedence correct, though maybe with extra parens.

Apparently \code{rlang::expr_deparse} also handles \code{:=} and \code{?} sensibly (and doesn't put the parens on \code{?a}), but it is slow because it does everything itself, and \emph{crikey} it is a big dependency. \code{mdeparse} is fast because it's a beautiful hack.

While I was thinking about this, I came upon the \pkg{doubt} package, which is a \emph{really} clever thing--- kudos to the author!
}
}
\usage{mdeparse(expr, ...)
}
\arguments{
\item{ expr}{what to deparse}
\item{ ...}{other args for \code{base::deparse}}
}
\value{Character vector.
}
\examples{
deparse( quote( a:=b))     # [1] "`:=`(a, b)"
mdeparse( quote( a:=b))    # [1] "a := b"
mdeparse( quote( a?b))     # [1] "a ? b"
deparse( quote( a?b))      # [1] "`?`(a, b)"
mdeparse( quote( ?b))      # [1] "(?b)" best I could do--- sorry!
deparse( quote( ?b))       # [1] "`?`(b)"
}
\keyword{misc}

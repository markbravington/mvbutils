\name{A2D}
\alias{A2D}
\title{Array into dataframe}
\description{
From an \code{array} (or \code{matrix} or \code{vector}) input, \code{A2D} produces a dataframe with one column per dimension of the input, plus a column for the contents, which will be called "response" unless you set the \code{name.of.response} argument. Its (almost) inverse is \code{\link{D2A}}.

The other columns will have names "D1", "D2", etc, unless either (i) the input has a \emph{named} \code{dimnames} attribute, in which its names will be used, or (ii) the argument "add.names" is set to a character vector naming the dimensions. They will be \code{numeric} if If the input has any \code{dimnames}, then the latter's non-NULL elements will be used in place of 1,2,3,... etc for the entries in the corresponding columns.
\subsection{Offarray}{If you know you are dealing with an \code{offarray} object rather than a regular array, you can just call \code{as.data.frame(<myoffar>,...)} instead, for clarity. But if you do call \code{A2D}, all will be well (try it). OTOH, if you call \code{base::array2DF(<myoffar>)} then you generally don't get what you want.
}
\subsection{Note}{
\code{\link{D2A}} and (something similar to) \code{A2D} used to be in my semi-secret \code{handy2} package under slightly different names, but they are useful enough that I've moved them to \code{mvbutils} in 2025. The \code{handy2} version (\code{array.to.data.frame} AKA \code{a2d}) made \code{factor} columns rather than \code{character}, and contained a lot of code. \code{A2D} is largely a wrapper for \code{base::array2DF} (qv), which didn't use to exist; however, \code{A2D} makes \code{numeric} columns where possible and uses names of dimnames to set column names if possible. This all makes it work better with \code{\link{tapply}}.

\code{A2D} and \code{\link{D2A}} are not strict inverses, because (i) if you start with a \code{data.frame} that lacks rows for some index combinations, those rows will still appear in the result, (ii) \code{factor} columns turn into \code{character} columns, and (iii) columns might get re-ordered.
}
}
\usage{A2D( a, name.of.response = "response")
}
\arguments{
\item{ a}{array, matrix, or, vector, including \code{offarray} objects from the eponymous package.}
\item{ name.of.response}{what to call the output column that holds the array \emph{contents} (as opposed to its dimensions).}
}
\value{A \code{data.frame}, with one more column than there are dimensions to \code{a}.
}
\seealso{\code{\link{D2A}}
}
\examples{
grubbb <- expand.grid( xx=1:4, yy=2:3) # data.frame
grubbb$z <- with( grubbb, xx+10*yy)
D2A( grubbb, data.col='z')
A2D( D2A( grubbb, data.col='z'), name.of.response='zzzzzzz')
# ... how very interesting...
}
\keyword{misc}

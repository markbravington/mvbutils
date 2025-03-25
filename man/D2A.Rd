\name{D2A}
\alias{D2A}
\title{data.frame.to.array    package:mvbutils}
\section{D2a}{Dataframe to array
}
\description{
\code{D2A} makes an array out of one column in a dataframe, with (by default) the remaining columns forming the array dimensions, in order. Its (almost) inverse is \code{\link{A2D}}.

You can choose which columns to use for the dimensions, and in which order, via the \code{dim.cols} argument. However, it often easier to subset the array by columns in the call, eg \code{D2A( x[ cq( Year, Len, Count), data.col="Count")}. Each unique value in an index column gets a "row" in the array. Combinations of indices that don't appear as rows in the input will become \code{missing.value} in the output. If a row has missing values in any index column, it is ignored.

Duplicated index rows in the \code{data.frame} are not advisable, and trigger a \code{warning}; I think the last value will be used, but I'm not sure.
\subsection{Note}{
\code{D2A} and (something similar to) \code{\link{A2D}} used to be in my semi-secret \code{handy2} package under slightly different names, but they are useful enough that I've moved them to \code{mvbutils} in 2025.

You can of course do vaguely similar things with base-R and perhaps with countless other packages too, but why not just use this? I do!
}
}
\usage{
D2A(
  df,
  data.col,
  dim.cols = names(df) \%except\% data.col,
  missing.value = NA)
}
\arguments{
\item{ df}{data.frame}
\item{ dim.cols}{character vector saying which columns (in order) to use for array dimensions. Default is everything except \code{data.col}}
\item{ data.col}{string saying which column should form the contents of the output}
\item{ missing.value}{what to put into the output for index-combinations that don't occur in the input.}
}
\value{Array with \code{length( dim.cols)} dimensions, and appropriate \code{dimnames}.
}
\seealso{\code{\link{A2D}}
}
\examples{
grubbb <- expand.grid( xx=1:4, yy=2:3) # data.frame
grubbb$z <- with( grubbb, xx+10*yy)
D2A( grubbb, 'z')
# Let's remove some values, and change the order of array dims...
minigrubbb <- grubbb[ c( 1, 3, 4, 7),]
D2A( minigrubbb, 'z', dim.cols=cq( yy, xx))
# Don't have to use all columns
D2A( minigrubbb, 'z', dim.cols='xx')
}
\keyword{misc}

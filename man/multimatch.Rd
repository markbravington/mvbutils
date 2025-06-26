\name{multimatch}
\alias{multimatch}
\title{Match rows of one dataframe to another using multiple columns}
\description{
Like \code{match}, but for more than one variable at a time--- and geared specifically to dataframes (or matrices). NA values match only to NAs.

So useful that I've finally moved it from secret package \pkg{handy2} into package \pkg{mvbutils}, and added documentation.

Any \code{factor} fields (which I hardly ever use; characters are just Better) will be matched based on the strings they display as, so that (eg) arbitrary re-orderings \code{levels} won't matter.
}
\usage{multimatch(df1, df2, nomatch = NA, char.force = FALSE, force.same.cols = TRUE)
}
\arguments{
\item{ df1,  df2}{two dataframes. Unless you set \code{force.same.cols=FALSE}, column-order is assumed to be the same in both, and a mismatched number of columns will trigger an error.}
\item{ nomatch}{like in \code{\link{match}}}
\item{ char.force}{?convert all columns to \code{character} before checking? Usually doesn't matter; if it does, \code{TRUE} is \emph{probably} the safer, but historically the default is \code{FALSE}}
\item{ force.same.cols}{Perhaps a misleading name... set to \code{FALSE} if you want \code{multimatch} to use only columns whose name exists in both dataframes, and to re-order columns if necessary so that the names match. Usually the non-default \code{FALSE} is better!}
}
\value{A numeric vector, one element per row in \code{df1}, showing which row in \code{df2} it matches to, or \code{nomatch} if none do.
}
\details{\code{multimatch} works by constructing a single numeric composite for each row in \code{df1} and \code{df2}, based on multiplying numbers of distinct values across columns. This could \emph{potentially} overflow, or give inaccurate results, if the number of columns and distinct values is very large. So, don't use \code{multimatch} in that situation...
}
\seealso{\code{match}
}
\examples{
xx <- data.frame(
    animal= cq( cat, dog, cat), colour= cq( blue, blue, pink), size= 1:3,
    royalty= cq( high, low, high))
yy <- data.frame(
    animal= cq( dog, dog, cat), colour= cq( red, blue, pink), size= 1:3,
    loyalty=cq( high, high, low)) # note the spelling!
multimatch( xx, yy)
# NA NA NA
multimatch( xx[,1:3], yy[,1:3])  # ignore 4th col
# NA 2 3
multimatch( xx, yy, force=FALSE) # auto-drop loyalty & royalty (different names)
# NA 2 3
try( multimatch( xx[,1:2], yy[,1:3]))
# <error>: num of cols
try( multimatch( xx[,1:2], yy[,1:3], force=FALSE))
# all good
multimatch( as.matrix( xx[,1:3]), as.matrix( yy[,1:3])) # matrices OK too
# NA 2 3
}
\keyword{misc}

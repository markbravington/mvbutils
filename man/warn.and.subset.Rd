\name{warn.and.subset}
\alias{warn.and.subset}
\title{Extract subset and warn about omitted cases}
\description{Extract row-subset of a \code{data.frame} according to a condition. If any cases (rows) are omitted, they are listed with a warning. Rows where the condition gives NA are omitted.
}
\usage{
# This is the obligatory format, and is not very useful; look at EXAMPLES instead
warn.and.subset(x, cond,
    mess.head=deparse( substitute( x), width.cutoff=20, control=NULL, nlines=1),
    mess.cond=deparse( substitute( cond), width.cutoff=40, control=NULL, nlines=1),
    row.info=rownames( x), sub=TRUE)
}
\arguments{
\item{ x}{data.frame}
\item{ cond}{expression to evaluate in the context of \code{data.frame}. If \code{sub=TRUE} (the default), this will be substituted. If \code{sub=FALSE}, you can use a pre-assigned expression; in that case, you had better set \code{mess.cond} manually.}
\item{ mess.head}{description of data.frame (e.g. its name) for use in a warning.}
\item{ mess.cond}{description of the desired condition for use in a warning.}
\item{ row.info}{character vector that will describe rows; omitted elements appear in the warning}
\item{ sub}{should \code{cond} be treated as a literal expression to be evaluated, or as a pre-computed logical index?}
# ...: just there to keep RCMD CHECK happy-- for heaven's sake...
}
\value{The subsetted data.frame.
}
\seealso{\code{\%where.warn\%} which is a less-flexible way of doing the same thing
}
\examples{
df <- data.frame( a=1:3, b=letters[1:3])
df1 <- warn.and.subset( df, a \%\% 2 == 1, 'Boring example data.frame', 'even-valued "a"')
condo <- quote( a \%\% 2 == 1)
df2 <- warn.and.subset( df, condo, 'Same boring data.frame', deparse( condo), sub=FALSE)
}
\keyword{misc}

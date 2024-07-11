\name{make_dull}
\alias{make_dull}
\alias{make.dull}
\alias{undull}
\title{Hide dull columns in data frames}
\description{
\code{make_dull} AKA \code{make.dull} adds a "dull" S3 class to designated columns in a \code{data.frame}. When the \code{data.frame} is printed, entries in those columns will show up just as "...". Useful for hiding long boring stuff like matrices with loads of columns, nucleotide sequences, MD5 sums, and filenames. Columns will still print clearly and behave properly if manually extracted. You can remove dullness via \code{undull}; see \bold{Examples}.

The \code{dull} class has methods for \code{format} (used when printing a \code{data.frame}) and \code{[}, so that dullness is perpetuated.
}
\usage{make_dull(df, cols)
}
\arguments{
\item{ df}{a data.frame}
\item{ cols}{columns to designate}
}
\value{A modified data.frame
}
\details{
Ask yourself: do you \emph{really} want details of a function called \code{make_dull}? Life may be sweet but it is also short.

Actually, I've had to add something "for the record", but you probably don't want to read this. Just prepending "dull" to the "class" attribute of a column (see \code{\link{oldClass}}) can mask normal S3 dispatch--- matrices being a case in point. Therefore, \code{make_dull} now also adds (part of) the \emph{implicit} class of the column after "dull". In quasi-English, what that means is that a matrix will acquire \emph{explicit} class attribute \code{c( "dull", "matrix", "array")}, whereas a normal non-dull matrix would have a \code{NULL} "class" attribute but an \emph{implicit} class of c("matrix","array"). This means you can still do e.g. \code{isSymmetric( x$dullmat)} and \code{inherits( x$dullmat, "matrix")}. Other semi-exotic objects (including \code{array}, which AFAICR only semi-works inside dataframes, \code{list} which I \emph{think} works OK, and user-defined classes) get similar treatment.

This Sort Of Thing is why it's marginally worth having an explicit \code{undull} function, as opposed to just \code{unclass} or eg \code{oldClass(excol) <- oldClass( excol) \%except\% "dull"}. The former would destroy a user-defined class; the latter would leave superfluous \code{matrix} and \code{array} elements in an unnecessary "class" attribute (although I'm not sure that causes any practical problems).

Sigh... we are deep in the S3wamplands here; "the perfect is the enemy of the good" where S3 is concerned, so we just gotta put up with some of the murky stuff. That's OK. Be careful what you wish for: ie S4!
\subsection{More details}{\code{make_dull} is both autologous and idempotent.
}
}
\examples{
# Becos more logical syntax:
rsample <- function (n = length(pop), pop, replace = FALSE, prob = NULL)\{
  pop[sample(seq_along(pop) - 1, size = n, replace = replace, prob = prob) + 1]
\}
df <- data.frame( x=1:3,
    y=apply( matrix( rsample( 150, as.raw( 33:127), rep=TRUE), 50, 3), 2, rawToChar),
    stringsAsFactors=FALSE) # s.A.F. value shouldn't matter
df # zzzzzzzzzzzzzzz
df <- make_dull( df, 'y')
df # wow, exciting!
df$y # zzzzzzzzzzzzzz
undull( df$y) # no class attrib now
df$ZZZ <- matrix( 1:99, nrow=3, ncol=33)
df # boooring
class( df$ZZZ)
oldClass( df$ZZZ)
df <- make_dull( df, 'ZZZ')
df # whew
class( df$ZZZ)
oldClass( df$ZZZ)
ZZZ <- df$ZZZ
# Suddenly it is interesting! So...
ZZZ <- undull( ZZZ)
class( ZZZ)
oldClass( ZZZ)
}
\keyword{misc}

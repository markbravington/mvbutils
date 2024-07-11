\name{multirep}
\alias{multirep}
\alias{multinsert}
\alias{massrep}
\title{Replacement and insertion functions with more/less than 1 replacement per spot}
\description{\code{multirep} is like \code{replace}, but the replacements are a list of the same length as the number of elements to replace. Each element of the list can have 0, 1, or more elements-- the original vector will be expanded/contracted accordingly. (If all elements of the list have length 1, the result will be the same length as the original.) \code{multinsert} is similar, but doesn't overwrite the elements in \code{orig} (so the result of \code{multinsert} is longer). \code{massrep} is like \code{multirep}, but takes lists as arguments so that a group-of-line-numbers in the first list is replaced by a group-of-lines in the second list.
}
\usage{
multirep( orig, at, repl, sorted.at=TRUE)
multinsert( orig, at, ins, sorted.at=TRUE)
massrep( orig, atlist, replist, sorted.at=TRUE)
}
\arguments{
\item{ orig}{vector}
\item{ at}{numeric vector, saying which elements of the original will be replaced or appended-to. Can't exceed \code{length(orig)}. 0 is legal in \code{multinsert} but not \code{multirep}. Assumed sorted unless \code{sorted.at} is set to FALSE.}
\item{ atlist}{list where each element is a group of line numbers to be replaced by the corresponding element of \code{replist} (and that element can have a different length). Normally each group of line numbers would be consecutive, but this is not mandatory.}
\item{ repl, ins, replist}{a list of replacements. \code{repl[[i]]} will replace line \code{at[i]} in \code{orig}, possibly removing it (if \code{repl[[i]]} has length 0) or inserting extra elements (if \code{repl[[i]]} has length > 1). In \code{multinsert}, \code{repl} can be a non-list, whereupon it will be cast to \code{list(repl)} [if \code{at} is length 1] or \code{as.list(repl)} [if \code{at} is length>1]. If \code{length(repl}) < \code{length(at)}, \code{repl} will be replicated to the appropriate size. If \code{repl} is atomic, it will be typecast into a list-- in this case, all replacements/insertions will be of length 1.}
\item{ sorted.at}{if TRUE, then \code{at} had better be sorted beforehand; if FALSE, \code{at} will be sorted for you inside \code{multirep}, and \code{repl} is reordered accordingly.}
}
\examples{
multirep( cq( the, cat, sat, on, the, mat), c( 2, 6),
    list( cq( big, bug), cq( elephant, howdah, cushion)))
# [1] "the" "big" "bug" "sat" "on" "the" "elephant" "howdah" "cushion"
multirep( cq( the, cat, sat, on, the, mat), c( 2, 6),
    list( cq( big, bug), character(0)))
# [1] "the" "big" "bug" "sat" "on" "the"
# NB the 0 in next example:
multinsert( cq( cat, sat, on, mat), c( 0, 4),
    list( cq( fat), cq( cleaning, equipment)))
# [1] "fat" "cat" "sat" "on" "mat" "cleaning" "equipment"
}
\keyword{misc
}

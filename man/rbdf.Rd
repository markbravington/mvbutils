\name{rbdf}
\alias{rbdf}
\alias{rbind}
\alias{rbind.data.frame}
\title{Data frames: better behaviour with zero-length cases}
\description{
\code{rbind} concatenates its arguments by row; see \code{\link{cbind}} for basic documentation. There is an \code{rbind} method for data frames which \code{mvbutils} overrides, and \code{rbdf} calls the override directly. The \code{mvbutils} version should behave exactly as the base-R version, with two exceptions:

\itemize{
\item zero-row arguments are \bold{not} ignored, e.g. so that factor levels which never appear are not dropped.
\item dimensioned (array or matrix) elements do not lose any extra attributes (such as \code{class}).
}

 I find the zero-row behaviour more logical, and useful because e.g. it lets me create an \code{\link{empty.data.frame}} with the correct type/class/levels for all columns, then subsequently add rows to it. The behaviour for matrix (array) elements allows e.g. the \code{rbind}ing of data frames that contain matrices of \code{POSIXct} elements without losing the \code{POSIXct} class (as in my package \pkg{nicetime}).

When \code{rbind}ing data frames, best practice is to make sure all the arguments really are data frames. Lists and matrices also work OK (they are first coerced to data frames), but scalars are dangerous (even though base-R will process them without complaint). \code{rbind} is quirky around data frames; unless \emph{all} the arguments are data frames, sometimes \code{rbind.data.frame} will not be called even when you'd expect it to be, and the coercion of scalars is frankly potty; see \bold{Details} and EXAMPLES. \code{mvbutils:::rbind.data.frame} tries to mimic the base-R scalar coercion, but I'm not sure it's 100\% compatible. Again, the safest way to ensure a predictable outcome, is to make sure all arguments really are data frames, and/or to call \code{rbdf} directly.

Note that ("thanks" to \code{stringsAsFactors}) the order in which data frames are rbound can affect the result--- see \bold{Examples}.
\subsection{Obsolete}{
Versions of \code{mvbutils} prior to 2.8.207 installed replacements for \code{$<-.data.frame} and \code{[[<-.data.frame} that circumvented weird behaviour with the base-R versions when the \code{data.frame} had zero rows. That weird behaviour seems to be fixed in base-R as of version 3.4.4 (perhaps earlier). I've therefore removed those replacements (after warnings from newer versions RCMD CHECK). Hopefully, everything works... but just for the record, here's the old text, which I \emph{think} no longer applies.

[I \emph{think} this paragraph is obsolete.] Normally, you can replace elements in, or add a column to, data frames via e.g. \code{x$y <- z} or \code{x[["y"]] <- z}. However, in base-R this fails for no good reason if \code{x} is a zero-row data frame; the sensible behaviour when \code{y} doesn't exist yet, would be to create a zero-length column of the appropriate class. \code{mvbutils} overrides the base (mis)behaviour so it works sensibly. Should work for matrix/array "replacements" too.
}
}
\usage{
rbind(..., deparse.level = 1) # generic
\method{rbind}{data.frame}(..., deparse.level = 1) # S3 method for data.frame
rbdf(..., deparse.level = 1) # explicitly call S3 method...
# ... for data frames (circumvent rbind dispatch)
## OBSOLETE x[[i,j]] <- value # S3 method for data.frame; only ...
## OBS ... the version x[[i]] <- value is relevant here, tho' arguably j==0 might be
## OBS x$name <- value # S3 method for data.frame
}
\arguments{
\item{ ...}{Data frames, or things that will coerced to data frames. NULLs are ignored.}
\item{ deparse.level}{not used by \code{rbind.data.frame}, it's for the default and generic only}
}
\details{
 old arguments

\describe{
\item{i,j}{column and row subscripts}
\item{name}{column name}
\item{x, value}{that's up to you; I just have to include them here to stop RCMD CHECK from moaning... :/}
}

See \code{\link{cbind}} documentation in base-R.

R{}'s dispatch mechanism for \code{rbind} is as follows [my paraphrasing of base-R documentation]. Mostly, if any argument is a data frame then \code{rbind.data.frame} will be used. However, if one argument is a data frame but another argument is a scalar/matrix of a class that has an \code{rbind} method, then "default rbind" will be called instead. Although the latter still returns a data frame, it stuffs up e.g. class attributes, so that \code{POSIXct} objects will be turned into huge numbers. Again, if you really want a data frame result, make sure all the arguments are data frames.

In \code{mvbutils:::rbind.data.frame} (and AFAIK in the base-R version), arguments that are not data frames are coerced to data frames, by calling \code{data.frame()} on them. AFAICS this works predictably for list and matrix arguments; note that lists need names, and matrices need column names, that match the names of the real data frame arguments, because column alignment is done by name not position. Behaviour for scalars is IMO weird; see \bold{Examples}. The idea seems to be to turn each scalar into a single-row data frame, coercing its names and truncating/replicating it to match the columns of the first real data frame argument; any \code{names} of the scalar itself are disregarded, and alignment is by position not name. Although \code{mvbutils:::rbind.data.frame} tries to mimic this coercion, it seems to me unnecessary (the user should just turn the scalar into something less ambiguous), confusing, and dangerous, so \code{mvbutils} issues a warning. Whether I have duplicated every quirk, I'm not sure.

Note also that R{}'s accursed \code{drop=TRUE} default means that things you might reasonably think \emph{should} be data frames, might not be. Under some circumstances, this might result in \code{rbind.data.frame} being bypassed. See \bold{Examples}.

Short of rewriting \code{data.frame} and \code{rbind}, there's nothing \code{mvbutils} can do to fix these quirks. Whether base-R should consider any changes is another story, but back-compatibility probably suggests not.
}
\value{
[Taken from the base-R documentation, modified to fit the \code{mvbutils} version]
The \code{rbind} data frame method first drops any NULL arguments, then coerces all others to data frames (see \bold{Details} for how it does this with scalars). Then it drops all zero-column arguments. (If that leaves none, it returns a zero-column zero-row data frame.) It then takes the classes of the columns from the first argument, and matches columns by name (rather than by position). Factors have their levels expanded as necessary (in the order of the levels of the levelsets of the factors encountered) and the result is an ordered factor if and only if all the components were ordered factors. (The last point differs from S-PLUS.) Old-style categories (integer vectors with levels) are promoted to factors. Zero-row arguments are kept, so that in particular their column classes and factor levels are taken account of.
Because the class of each column is set by the \emph{first} data frame, rather than "by consensus", numeric/character/factor conversions can be a bit surprising especially where NAs are involved. See the final bit of EXAMPLES.
}
\seealso{\code{cbind} and \code{data.frame} in base-R; \code{\link{empty.data.frame}}
}
\examples{
# mvbutils versions are used, unless base:: or baseenv() gets mentioned
# Why base-R dropping of zero rows is odd
rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0))$x # mvbutils
#[1] no
#Levels: yes no # two levels
base::rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0))$x # base-R
#[1] no
#Levels: no # lost level
rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0, stringsAsFactors=FALSE))$x
#[1] no
#Levels: yes no
base::rbind( data.frame( x='yes', y=1)[-1,], data.frame( x='no', y=0, stringsAsFactors=FALSE))$x
#[1] "no" # x has turned into a character
# Quirks of scalar coercion
evalq( rbind( data.frame( x=1), x=2, x=3), baseenv()) # OK I guess
#   x
#1  1
#x  2
#x1 3
evalq( rbind( data.frame( x=1), x=2:3), baseenv()) # NB lost element
#  x
#1 1
#x 2
evalq( rbind( data.frame( x=1, y=2, z=3), c( x=4, y=5)), baseenv())
# NB gained element! Try predicting z[2]...
#  x y z
#1 1 2 3
#2 4 5 4
evalq( rbind( data.frame( x='cat', y='dog'), cbind( x='flea', y='goat')), baseenv()) # OK
#     x    y
#1  cat  dog
#2 flea goat
evalq( rbind( data.frame( x='cat', y='dog'), c( x='flea', y='goat')), baseenv()) # Huh?
#Warning in `[<-.factor`(`*tmp*`, ri, value = "flea") :
#  invalid factor level, NAs generated
#Warning in `[<-.factor`(`*tmp*`, ri, value = "goat") :
#  invalid factor level, NAs generated
#     x    y
#1  cat  dog
#2 <NA> <NA>
evalq( rbind( data.frame( x='cat', y='dog'), c( x='flea')), baseenv()) # Hmmm...
#Warning in `[<-.factor`(`*tmp*`, ri, value = "flea") :
#  invalid factor level, NAs generated
#Warning in `[<-.factor`(`*tmp*`, ri, value = "flea") :
#  invalid factor level, NAs generated
#     x    y
#1  cat  dog
#2 <NA> <NA>
try( evalq( rbind( data.frame( x='cat', y='dog'), cbind( x='flea')), baseenv())) # ...mmmm...
#Error in rbind(deparse.level, ...) :
#  numbers of columns of arguments do not match
# Data frames that aren't:
data.frame( x=1,y=2)[-1,] # a zero-row DF-- OK
# [1] x y
# <0 rows> (or 0-length row.names)
data.frame( x=1)[-1,] # not a DF!?
# numeric(0)
data.frame( x=1)[-1,,drop=FALSE] # OK, but exceeeeeedingly cumbersome
# <0 rows> (or 0-length row.names)
# Implications for rbind:
rbind( data.frame( x='yes')[-1,], x='no')
#  [,1]
# x "no" # rbind.data.frame not called!
rbind( data.frame( x='yes')[-1,,drop=FALSE], x='no')
#Warning in rbind(deparse.level, ...) :
#  risky to supply scalar argument(s) to 'rbind.data.frame'
#   x
#x no
# Quirks of ordering and character/factor conversion:
rbind( data.frame( x=NA), data.frame( x='yes'))$x
#[1] NA    "yes" # character
rbind( data.frame( x=NA_character_), data.frame( x='yes'))$x
#[1] <NA> yes
#Levels: yes # factor!
rbind( data.frame( x='yes'), data.frame( x=NA))$x[2:1]
#[1] <NA>  yes
#Levels: yes # factor again
x1 <- data.frame( x='yes', stringsAsFactors=TRUE)
x2 <- data.frame( x='no', stringsAsFactors=FALSE)
rbind( x1, x2)$x
# [1] yes no
# Levels: yes no
rbind( x2, x1)$x
# [1] "no"  "yes"
# sigh...
}
\keyword{misc}

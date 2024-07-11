\name{mvbutils.operators}
\alias{mvbutils.operators}
\alias{\%&\%}
\alias{\%!in\%}
\alias{\%**\%}
\alias{\%except\%}
\alias{\%grepling\%}
\alias{\%has.name\%}
\alias{\%is.not.a\%}
\alias{\%is.a\%}
\alias{\%is.not.an\%}
\alias{\%is.an\%}
\alias{\%matching\%}
\alias{\%not.in\%}
\alias{\%not.in.range\%}
\alias{\%perling\%}
\alias{\%that.end.with\%}
\alias{\%that.start.with\%}
\alias{\%that.match\%}
\alias{\%that.dont.match\%}
\alias{\%THAT.MATCH\%}
\alias{\%THAT.DONT.MATCH\%}
\alias{\%that.are.in\%}
\alias{\%without.name\%}
\alias{\%in.range\%}
\alias{\%such.that\%}
\alias{\%SUCH.THAT\%}
\alias{\%upto\%}
\alias{\%downto\%}
\alias{\%where\%}
\alias{\%where.warn\%}
\alias{\%<-\%}
\title{Utility operators}
\description{Succinct or convenience operators
}
\usage{
a \%&\% b
x \%**\% y
a \%!in\% b
vector \%except\% condition    # does NOT strip attributes--- see *Value*
x \%grepling\% patt
x \%has.name\% name
x \%is.not.a\% what
x \%is.a\% what
x \%is.not.an\% what
x \%is.an\% what
x \%matching\% patt
a \%not.in\% b
a \%not.in.range\% b
x \%perling\% patt
x \%that.end.with\% suffix
x \%that.start.with\% prefix
x \%that.match\% patt
x \%that.dont.match\% patt
x \%THAT.MATCH\% patt
x \%THAT.DONT.MATCH\% patt
a \%that.are.in\% b       # does NOT strip attributes--- see *Value*
x \%without.name\% what   # does NOT strip attributes--- see *Value*
a \%in.range\% b
a \%such.that\% b         # does NOT strip attributes--- see *Value*
a \%SUCH.THAT\% b         # does NOT strip attributes--- see *Value*
from \%upto\% to
from \%downto\% to
x \%where\% cond
x \%where.warn\% cond
a \%<-\% value # really e.g. \{x;y\} \%<-\% list( 'yes', sqrt(pi)) to create x & y
}
\arguments{\item{ a, b, vector, condition, x, y, name, what, patt, from, to, cond, value, prefix, suffix}{see \bold{Arguments by function}.}
}
\value{
\item{ \%&\%}{character vector. If either is zero-length, so is the result (unlike \code{paste}).}
\item{ \%**\%}{numeric, possibly a matrix}
\item{ \%upto\%, \%downto\%}{numeric}
\item{ \%is.a\%, \%in\%, etc}{logical}
\item{ \%<-\%}{technically NULL return, but it overwrites / creates objects; see below...}
\item{ \%has.name\%}{logical vector}
\item{ All others}{same type as first argument.}
\item{ Note that attributes are _not_ stripped by the subsetting of \%without.name\%, \%except\%, \%such.that\%, \%SUCH.THAT\%, or \%that.are.in\% (as of v2.8.369)--- whereas base R does, which I view as a bug. However, attributes may still get stripped by the other \%that... and \%matching\%, because (some of) those use unique. Possibly I should tweak those too, but I think they are inconsistently designed (e.g. \%that.match\% returns unique values, but \%that.dont.match\% uses subsetting) and I dont want to risk breaking more things...}{}
}
\section{Arguments by function}{
\code{\%&\%} a, b: character vectors to be \code{paste}d with no separator. If either is zero-length, so is the result (unlike \code{paste}).

\code{\%**\%} x, y: matrices or vectors to be multiplied using \code{\%*\%} but with less fuss about dimensions

\code{\%!in\%}, \code{\%that.are.in\%} a, b: vectors (character, numeric, complex, or logical).

\code{\%except\%} vector, condition: character or numeric vectors

\code{\%has.name\%} x, name: whether \code{name} (perhaps several) is in \code{names(x)}. Sugar for \code{base::hasName}.

\code{\%in.range\%}, \code{\%not.in.range\%} a, b: numeric vectors.

\code{\%is.a\%}, etc. x: object whose class is to be checked

\code{\%is.a\%}, etc. what: class name

\code{\%matching\%}, \code{\%that.match\%}, \code{\%that.dont.match\%}, \code{\%THAT.MATCH\%}, \code{\%THAT.DONT.MATCH\%}, \code{\%grepling\%}, \code{\%perling\%} x: character vector

\code{\%matching\%}, \code{\%that.match\%}, \code{\%that.dont.match\%}, \code{\%THAT.MATCH\%}, \code{\%THAT.DONT.MATCH\%}, \code{\%grepling\%}, \code{\%perling\%} patt: character vector of regexps, with perl syntax for \code{\%perling\%}. Use the upper-case versions for case-insensitive matching.

\code{\%that.start.with\%}, \code{\%that.end.with\%} : prefix & suffix: \emph{fixed} (non-regex) strings that must match the start or end of \code{x} , as per \code{\link{startsWith}} and \code{\link{endsWith}}.

\code{\%such.that\%}, \code{\%SUCH.THAT\%} a: vector

\code{\%such.that\%}, \code{\%SUCH.THAT\%} b: expression containing a \code{.}, to subscript \code{a} with

\code{\%upto\%}, \code{\%downto\%} from, to: numeric(1)

\code{\%where\%}, \code{\%where.warn\%} x: data.frame

\code{\%where\%}, \code{\%where.warn\%} cond: unquoted expression to be \code{eval}ed in context of \code{x}, then in the calling frame of \code{\%where\%} (or \code{.GlobalEnv}). Should evaluate to logical (or maybe numeric or character); NA is treated as FALSE. Wrap \code{cond} in parentheses to avoid trouble with operator precedence.

\code{\%without.name\%} x: object with \code{names} attribute

\code{\%without.name\%} what: character vector of names to drop

\code{\%<-\%} a, value: \code{value} should be a list, and \code{a} should be e.g. \code{{x;y;z}} with as many elements as \code{value} has. The elements of \code{value} are assigned, in order, to the objects named in \code{a}, which are created / overwritten in the calling environment.
}
\author{Mark Bravington}
\seealso{bquote
}
\examples{
"a" \%&\% "b" # "ab"
matrix( 1:4, 2, 2) \%**\% matrix( 1:2, 2, 1) # c( 7, 10); '\%*\%' gives matrix result
matrix( 1:2, 2, 1) \%**\% matrix( 1:4, 2, 2) # c( 5, 11); '\%*\%' gives error
1:2 \%**\% matrix( 1:4, 2, 2) # '\%*\%' gives matrix result
1:5 \%!in\% 3:4 # c( TRUE, TRUE, FALSE, FALSE, TRUE)
1:5 \%not.in\% 3:4 # c( TRUE, TRUE, FALSE, FALSE, TRUE)
1:5 \%that.are.in\% 3:4 # c( 3, 4)
trf <- try( 1+"nonsense")
if( trf \%is.not.a\% "try-error") cat( "OK\\n") else cat( "not OK\\n")
1:5 \%except\% c(2,4,6) # c(1,3,5)
c( alpha=1, beta=2) \%without.name\% "alpha" # c( beta=2)
xx <- list( y=0, z='pterodactyl')
xx \%has.name\% 'y' # yep
xx \%has.name\% 'pringle' # nope
xx \%has.name\% cq( y, z) # yep and yep again
1:5 \%in.range\% c( 2, 4) # c(F,T,T,T,F)
1:5 \%not.in.range\% c( 2, 4) # c(T,F,F,F,T)
c( "cat", "hat", "dog", "brick") \%matching\% c( "at", "ic") # cat hat brick
c( "cat", "hat", "dog", "brick") \%that.match\% c( "at", "ic") # cat hat brick; ...
# ... synonym for '\%matching\%'
c( "cat", "hat", "dog", "brick") \%THAT.MATCH\% c( "AT", "ic") # cat hat brick; case-insensitive
# ... synonym for '\%matching\%'
c( "cat", "hat", "dog", "brick") \%that.dont.match\% c( "at", "ic") # dog; ...
# ... like '\%except\%' but for regexps
c( "cat", "hat", "dog", "brick") \%that.end.with\% 'at' # cat hat
c( "cat", "hat", "dog", "brick") \%that.start.with\% 'br' # brick
1 \%upto\% 2 # 1:2
1 \%upto\% 0 # numeric( 0); use \%upto\% rather than : in for-loops to avoid unintended errors
1 \%downto\% 0 # 1:0
1 \%downto\% 2 # numeric( 0)
ff <- function( which.row) \{
    x <- data.frame( a=1:3, b=4:6)
    x \%where\% (a==which.row)
  \}
ff( 2) # data.frame( a=2, b=5)
x <- data.frame( start=1:3, end=c( 4, 5, 0))
x \%where.warn\% (start < end) # gives warning about row 3
(1:5) \%such.that\% (.>2) # 3,4,5
listio <- list( a=1,  b=2)
chars <- cq( a, b)
chars \%SUCH.THAT\% (listio[[.]]==2) # 'b'; \%such.that\% won't work because [[]] can't handle xtuples
\{x;y\} \%<-\% list( 'yes', sqrt(pi))
# x: [1] "yes"
# y: [1] 1.772
}
\keyword{misc}

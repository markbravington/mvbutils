\name{mvbutils.utils}
\alias{mvbutils.utils}
\alias{as.cat}
\alias{atts}
\alias{clamp}
\alias{clip}
\alias{compacto}
\alias{cq}
\alias{deparse.names.parsably}
\alias{disatt}
\alias{eclone}
\alias{empty.data.frame}
\alias{env.name.string}
\alias{expanded.call}
\alias{everyth}
\alias{find.funs}
\alias{find.lurking.envs}
\alias{index}
\alias{inv.logit}
\alias{integ}
\alias{is.dir}
\alias{isF}
\alias{isT}
\alias{legal.filename}
\alias{logit}
\alias{lsall}
\alias{masked}
\alias{masking}
\alias{mkdir}
\alias{most.recent}
\alias{mwhere}
\alias{my.all.equal}
\alias{named}
\alias{nscat}
\alias{nscatn}
\alias{option.or.default}
\alias{pos}
\alias{put.in.session}
\alias{rename.els}
\alias{returnList}
\alias{safe.rbind}
\alias{scatn}
\alias{sourceable}
\alias{sqr}
\alias{to.regexpr}
\alias{undent}
\alias{xgsub}
\alias{xsub}
\alias{yes.no}
\title{Miscellaneous utilities}
\description{Miscellaneous utilities.
}
\usage{
as.cat( x)
atts( x, exclude=cq( levels, class, dim, dimnames, names, row.names, tsp))
clamp( x, min, max)
clip( x, n=1)
compacto( x, gap, width, extra)
cq( ...)
deparse.names.parsably( x)
disatt( x, keep_=cq( levels, dim, dimnames, names, row.names, tsp), keep)
eclone( env)
empty.data.frame( ...)
env.name.string( env)
expanded.call( nlocal=sys.parent())
everyth( x, by=1, from=1)
find.funs(pos=1, ..., exclude.mcache = TRUE, mode="function")
find.lurking.envs(obj, delve=FALSE, trace=FALSE)
index( lvector)
integ(expr, lo, hi, what = "x", ..., args.to.integrate = list())
inv.logit( q)
is.dir( dir)
isF( x)
isT( x)
legal.filename( name)
logit( x)
lsall( ...)
masked( pos)
masking( pos=1)
mkdir( dirlist)
most.recent( lvec)
mwhere( x, cond)
my.all.equal( x, y, ...)
named( x)
nscat( fmt, ..., sep='\\n', file='')
nscatn( fmt, ..., sep='\\n', file='')
option.or.default( opt.name, default=NULL)
pos( substrs, mainstrs, any.case = FALSE, names.for.output)
put.in.session( ...)
rename.els( ..., ignore.missing=FALSE)
returnList( ...)
safe.rbind( df1, df2) # Deprecated in 2013
scatn( fmt, ..., sep='\\n', file='', append=FALSE)
sourceable( f, fname=deparse1( substitute( f)))
sqr( x)
to.regexpr( x)
undent( s)
xgsub( x, pattern, replacement, perl=!fixed, fixed=FALSE, ...)
xsub( x, pattern, replacement, perl=!fixed, fixed=FALSE, ...)
yes.no( prompt, default)
}
\arguments{\item{ x, y, n, gap, width, extra, ..., by, keep, keep_, env, from, exclude, exclude.mcache, nlocal, lvector, dir, name, pos, frame, mode, dirlist, lvec, cond, opt.name, default, substrs, mainstrs, any.case, names.for.output, ignore.missing, df1, df2, prompt,  obj, delve, trace, fmt, sep, append, file, expr, lo, hi, what, args.to.integrate, q, s, min, max, pattern, replacement, perl, fixed, f, fname}{see "Arguments by function"}
}
\details{
\code{as.cat} makes a character vector print as if it was \code{cat}ted rather than \code{\link{print}}ed (one element per line, no extra quotes or backslashes, no [1] etc prefixes).

\code{atts} returns the names of the attributes of \code{x}, excluding any that are in \code{exclude}.

\code{clamp} clamps its 1st argument to the limits specified by the 2nd and 3rd. You can also just supply a range of values in the 2nd arg, and leave the 3rd missing. It's meant for use with pipes; see \bold{Examples}.

\code{clip} removes the last \code{n} elements of \code{x}. OBSOLETE--- use \code{head( x, -n)} instead.

\code{compacto} gives a matrix an extra S3 class "compacto", which means it will print out with column names/label \emph{vertical} and optionally no gaps between the columns. \code{gap} and \code{width} control the latter in fairly obvious ways. \code{extra} controls what gets printed to help the eye follow vertical alignment. See \bold{Examples}; there is a method \code{\link{print.compacto}} which surely needs little further description.

\code{cq} is handy for typing \code{cq( alpha, beta, gamma)} instead of \code{cq( "alpha", "beta", "gamma")}. Certain strings DO still require quotes around them, e.g. \code{cq( "NULL", "1-2")}).

\code{deparse.names.parsably} is like \code{deparse} except that \code{name} objects get wrapped in a call to \code{as.name}, so that they won't be evaluated accidentally.

\code{disatt} gets rid of most attributes on \code{x}. If you want to preserve some, use \code{keep}. The usually-default argument \code{keep_}, which is merged with \code{keep}, ensures that the "basic" attributes are retained; if you want to drop some of those too, you will have to modify \code{keep_}. Note that S3 class is dropped by default, because some S3 objects may not make sense without certain attributes.

\code{eclone} clones an \code{environment} into a new one with the same parent, ie making deep copies of all the (non-environment) members, so that changing their values in the new env won't affect the original values (unlike if you just assign the old env to the new one). Functions whose environment \emph{was} the original environment, will have their environment reset to the new one. If you don't understand that, then either don't worry be happy, or do more homework on R{}'s \code{environment} objects. See also \bold{Examples}.

\code{empty.data.frame} creates a template data frame with 0 rows but with all columns of the appropriate type. Useful for \code{\link{rbind}}ing to later.

\code{env.name.string} returns a string naming an environment; its \code{name} attribute if there is one, or the name of its \code{path} attribute if applicable, concatenated with the first line of what would be shown if you \code{\link{print}}ed the argument. Unlike \code{environmentName}, this will always return a non-empty string.

\code{expanded.call} returns the full argument list available to its caller, including defaults where arguments were not set explicitly. The arguments may not be those originally passed, if they were modified before the invocation of \code{expanded.call}. Default arguments which depend on calculations after the invocation of \code{expanded.call} will lead to an error.

\code{everyth} extracts every \code{by}-th element of \code{x}, starting at position \code{from}.

\code{find.funs} finds "function" objects (or objects of other modes, via the "mode" arg) in one or more environments, optionally matching a pattern.

\code{find.lurking.envs( myobj)} will search through \code{myobj} and all its attributes, returning the size of each sub-object. The size of environments is returned as Inf. The search is completely recursive, except for environments and by default the inner workings of functions; attributes of the entire function are always recursed. Changing the \code{delve} parameter to TRUE ensures full recursion of function arguments and function bodies, which will show e.g. the \code{srcref} structure; try it to see why the default is FALSE. \code{find.lurking.envs} can be very useful for working out e.g. why the result of a model-fitting function is taking up 1000000MB of disk space; sometimes this is due to unnecessary environments in well-concealed places.

\code{index} returns the position(s) of TRUE elements. Unlike \code{which}: attributes are lost; NA elements map to NAs; \code{index(<<length 0 object>>)} is \code{numeric(0)}; \code{index( <<non-logical>>)} is NA.

\code{integ} is a handy wrapper for \code{integrate}, that takes an expression rather than a function--- so \code{integ( sin(x), 0, 1)} "just works".

\code{is.dir} tests for directoriness.

\code{isF} and \code{isT} test a logical \emph{scalar} in the obvious way, with NA (and non-logicals) failing the test, to avoid teeeedious repetition of \code{is( !is.na( my.complicated.expression) & my.complicated.expression) ...}. They are deliberately not vectorized (contrary to some versions of \code{mvbutils} documentation); arguments with non-1 length trigger a warning.

\code{legal.filename} coerces its character argument into a similar-looking string that is a legal filename on any (?) system.

\code{logit} and \code{inv.logit} apply those transformations (for those of us who can never remember what the \pkg{stats} package versions are called).

\code{lsall} is like \code{ls} but coerces \code{all.names=TRUE}.

\code{masked} checks which objects in \code{search()[pos]} are masked by identically-named objects higher in the search path. \code{masking} checks for objects mask identically-named objects lower in the search path. Namespaces may make the results irrelevant.

\code{mkdir} makes directories; unlike \code{dir.create}, it can do several levels at once.

\code{most.recent} returns the highest-so-far position of TRUE within a logical vector, or 0 if TRUE has not occurred yet; \code{most.recent( c(F,T,F,T))} returns c(0,2,2,4).

\code{mwhere} subsets a \code{data.frame} by row, just like \code{\%where\%} (qv); it's for use in pipes, as per \bold{Examples}.

\code{my.all.equal} is like \code{all.equal}, except that it returns FALSE in cases where \code{all.equal} returns a non-logical-mode result.

\code{named(x)} is just \code{names(x) <- as.character( x); x}; useful for \code{lapply} etc.

\code{nscat}, \code{nscatn}: see \code{\link{scatn}}

\code{option.or.default} obsolete--- use equivalent \code{getOption()} instead.

\code{pos} is probably to be eschewed in new code, in favour of \code{gregexpr} with \code{fixed=TRUE}, which is likely faster. (And I should rewrite it to use \code{gregexpr}.) It's one of a few legacy functions in \code{mvbutils} that pre-date improvements in base R{}. \code{pos} will either search for several literal patterns in a single target, or vice versa-- but not both. It returns a matrix showing the positions of the matching substrings, with as many columns as the maximum number of matches. 0 signifies "no match"; there is always at least one column even if there are no matches at all.

\code{rename.els} replaces specified names of a vector with new ones.

\code{returnList} returns a list corresponding to old-style (pre-R 1.8) \code{return} syntax. Briefly: a single argument is returned as itself. Multiple arguments are returned in a list. The names of that list are the argument names if provided; or, for any unnamed argument that is just a symbolic name, that symbolic name; or no name at all, for other unnamed arguments. You can duplicate pre-1.8 behaviour of \code{return(...)} via \code{return(returnList(...))}.

\code{safe.rbind} ( \emph{Deprecated in 2013} ) mimics \code{\link{rbind}}, but works round an R{} bug (I reckon) where a column appears to be a numeric in one \code{data.frame} but a factor in the other. But I now think you should just sort your column classes/types properly in advance, rather than mixing types and relying on somewhat arbitrary conversion rules.

\code{scatn} is just \code{cat( sprintf( fmt, ...), "", file=file, sep=sep)}. \code{scatn} prints a newline afterwards, but not before; \code{nscat} does the opposite; \code{nscatn} does both. If you're just displaying a "title" before calling \code{\link{print}}, use \code{nscat}.

\code{sqr} squares its argument (i.e. multiplies the argument by itself), without the risk that \code{x^2} might incur exponentiation.

\code{to.regexpr} converts literal strings to their equivalent regexps, e.g. by doubling backslashes. Useful if you want "fixed=TRUE" to apply only to a portion of your regexp.

\code{undent} is handy when you want a slab of multi-line text inside some function you are writing. Raw-string syntax helps a lot (see the final examples of \code{?Quotes}), but indentation is horrible and the first line is out-of-step with the rest. You ideally want your text to appear indented at whatever looks nice inside your \emph{code}, but for the actual string not to be indented. So, start your raw string with a newline, and wrap the string in \code{undent}, and all will be well.

\code{sourceable} takes a function and returns a character vector which, when printed with \code{\link{print}} or \code{writeLines}, will \emph{probably} be amenable to \code{source}. Unlike \code{deparse}, it keeps the original source text, including comments. It will \emph{strip} the "<environment:gsd907897gsd>" and "<bytecode:097a0sdg>" verbiage which otherwise often prevents \code{source} from working, and which frequently annoy me. But it will try to \emph{keep} other attributes, such as useful "constants" accessed from within the function's code via eg \code{environment(sys.function())$<usefulconst>}. \code{sourceable} may be more generally useful than the similar \code{\link{write.sourceable.function}} because the latter is geared up to dealing directly with the \code{mvbutils} function-documentation system, and also actually writes to a file--- whereas \code{sourceable} returns a character vector (of class \code{cat}) which it's up to you to write or whatever.

\code{xsub} and \code{xgsub} are for pipes. They are just like \code{sub} and \code{gsub}, except that the \code{x} argument comes first, and that there is a default of \code{perl=TRUE} (unless you set \code{fixed=TRUE}). So you can write eg \code{str |> xsub( "old", "new")} rather than \code{str |> sub( "old", "new", x=_)} or \code{sub( "old", "new", str)}. It's just better.

\code{yes.no} \code{cat}s its "prompt" argument and waits for user input. if the user input \code{pmatch}es "yes" or "YES", then \code{yes.no} returns \code{TRUE}; if the input \code{pmatch}es \code{no} or \code{NO} then \code{yes.no} returns \code{FALSE}; if the input is '' and \code{default} is set, then \code{yes.no} returns \code{default}; otherwise it repeats the question. You probably want to put a space at the end of \code{prompt}.
}
\value{
\item{  as.cat}{character vector of class \code{cat}}
\item{  clip}{vector of the same mode as \code{x}}
\item{  cq}{character vector}
\item{  empty.data.frame}{\code{data.frame}}
\item{  env.name.string}{a string}
\item{  expanded.call}{a \code{call} object}
\item{  everyth}{same type as \code{x}}
\item{  find.funs}{a character vector of function names}
\item{  find.lurking.envs}{a \code{data.frame} with columns "what" and "size"}
\item{  integ}{scalar}
\item{  inv.logit}{numeric vector}
\item{  is.dir}{logical vector}
\item{  is.nonzero}{TRUE or FALSE}
\item{  isF, isT}{TRUE or FALSE}
\item{  legal.filename}{character( 1)}
\item{  logit}{numeric vector}
\item{  masked}{character vector}
\item{  masking}{character vector}
\item{  mclip}{possibly-modified version of \code{x}}
\item{  mkdir}{logical vector of success/failure}
\item{  nscat}{NULL}
\item{  nscatn}{NULL}
\item{  most.recent}{integer vector the same length as \code{lvec}, with values in the range (0,length(lvec)).}
\item{  named}{vector of the same mode as \code{x}}
\item{  option.or.default}{option's value}
\item{  pos}{numeric matrix, one column per match found plus one; at least one column guaranteed}
\item{  rename.els}{whatever the first argument was, with new names}
\item{  returnList}{list or single object}
\item{  safe.rbind}{\code{data.frame}}
\item{  scatn}{NULL}
\item{  sourceable}{character (class \code{cat} to print nicely)}
\item{  sqr}{as per input}
\item{  to.regexpr}{character}
\item{  undent}{string}
\item{  xgsub}{character}
\item{  xsub}{character}
\item{  yes.no}{TRUE or FALSE}
}
\section{Arguments by function}{
\describe{
\item{as.cat}{x: character vector that you want to be displayed via \code{cat( x, sep="\\n")}}
\item{atts}{x: any object; exclude: a character vector whatever quotidian attributes that you are not interested in knowing about}
\item{clip}{x: a vector or list}
\item{clip}{n: integer saying how many elements to clip from the end of \code{x}}
\item{cq}{...: quoted or unquoted character strings, to be \code{substitute}d and then concatenated}
\item{deparse.names.parsably}{x: any object for \code{deparse}- \code{name} objects treated specially}
\item{eclone}{env: an environment}
\item{empty.data.frame}{...: named length-1 vectors of appropriate mode, e.g. "first.col=''"}
\item{env.name.string}{env: environment}
\item{expanded.call}{nlocal: frame to retrieve arguments from. Normally, use the default; see \code{\link{mlocal}}.}
\item{everyth}{x: subsettable thing. by: step between values to extract. from: first position.}
\item{find.funs}{...: extra arguments for \code{objects}. Usually just "pattern" for regexp searches.}
\item{find.funs}{exclude.mcache: if TRUE (default), don't look at \code{\link{mlazy}} objects}
\item{find.funs}{mode: "function" to look for functions, "environment" to look for environments, etc}
\item{find.lurking.envs}{delve: whether to recurse into function arguments and function bodies}
\item{find.lurking.envs}{trace: just a debugging aid-- leave as FALSE}
\item{index}{lvector: vector of TRUE/FALSE/NA}
\item{integ}{expr: an expression; what: a string, the argument of \code{expr} to be integrated over; lo, hi: limits; ...: other variables to be set in the expression; args.to.integrate: a list of other things to pass to \code{integrate}}
\item{is.dir}{dir: character vector of files to check existence and directoriness of.}
\item{isF, isT}{x: anything, but meant to be a logical scalar}
\item{legal.filename}{name: character string to be modified}
\item{find.funs}{pos: list of environments, or vector of char or numeric positions in search path.}
\item{lsall}{...: as for \code{ls}, except that \code{all.names} will be coerced to TRUE}
\item{masking, masked}{pos: position in search path}
\item{mclip}{x: thing to be clipped (usually numeric, but character should work)--- dimensions and other attributes are preserved; min, max: clipping range}
\item{mkdir}{dirlist: character vector of directories to create}
\item{most.recent}{logical vector}
\item{my.all.equal}{x, y: anything; ...: passed to \code{all.equal}}
\item{named}{x: character vector which will become its own \code{names} attribute}
\item{nscat, nscatn}{see \code{\link{scatn}}}
\item{option.or.default}{opt.name: character(1)}
\item{option.or.default}{default: value to be returned if there is no \code{option} called \code{"opt.name"}}
\item{pos}{substrs: character vector of patterns (literal not regexpr)}
\item{pos}{mainstrs: character vector to search for \code{substrs} in.}
\item{pos}{any.case: logical- ignore case?}
\item{pos}{names.for.output: character vector to label rows of output matrix; optional}
\item{put.in.session}{...: a named set of objects, to be \code{assign}ed into the \code{mvb.session.info} search environment}
\item{rename.els}{...: the first argument is the thing to rename. Subsequent args like \code{X=<some expr giving a string result>} mean that whichever element of the first arg was called "X", will now be called the result of that expression. \code{ignore.missing=TRUE} means that requests to rename non-existent elements will be ignored; otherwise, they will throw an error.}
\item{returnList}{...: named or un-named arguments, just as for \code{return} before R{} 1.8.}
\item{safe.rbind}{df1, df2: \code{data.frame} or \code{list}}
\item{scatn, nscat}{fmt, ...: as per \code{sprintf}; file, sep, append: as per \code{cat}}
\item{sourceable}{f: an actual function object}
\item{sourceable}{fname: what name should the function be assigned to, if the result is fed to \code{source}? Default is the name of \code{f} itself, which is usually fine.}
\item{sqr}{x: anything for which \code{*} is a valid op.}
\item{to.regexpr}{x: character vector}
\item{undent}{s: string, presumably a "raw string".}
\item{xgsub}{x, pattern, replacement, perl=!fixed, fixed= FALSE, ...: as per \code{gsub}}
\item{xsub}{as per \code{xgsub}}
\item{yes.no}{prompt: string to put before asking for input}
\item{yes.no}{default: value to return if user just presses <ENTER>}
}
}
\author{Mark Bravington}
\examples{
# as.cat
ugly.bugly <- c( 'A rose by any other name', 'would annoy taxonomists')
ugly.bugly
#[1] "A rose by any other name"                 "would annoy taxonomists"
as.cat( ugly.bugly) # calls print.cat--- no clutter
#A rose by any other name
#would annoy taxonomists
x <- structure( matrix( 1:4, 2, 2), baggage='purple suitcase')
atts( x) # will not print "dim" since that is in default 'exclude' list
#[1] "baggage"
1:7 |> clamp( 2, 4)
#[1] 2 2 3 4 4 4 4
1:7 |> clamp( 2:4)
#[1] 2 2 3 4 4 4 4
clip( 1:5, 2) # 1:3
cq( alpha, beta) # c( "alpha", "beta")
x <- matrix( 1:4, 2, 2)
compacto( x)
compacto( x, extra='|', width=3) # similar to gap... yet different
colnames( x) <- c( 'Gogol', 'Turgenev')
compacto( x)
x <- 6
attr( x, 'massive') <- 1:1e5
x
disatt( x)
old_env <- new.env()
evalq( envir=old_env, \{
  x <- 3
  fun <- function() x
\})
new_env <- eclone( old_env)
new_env$x <- 5
new_env$fun() # 5
lazy_env <- old_env
lazy_env$x <- 4
old_env$x # 4 ! Take care with environments...
old_env$fun() # 4 of course
new_env$x # 5 phew
empty.data.frame( a=1, b="yes")
# data.frame with 0 rows of columns "a" (numeric) and "b" (character)
empty.data.frame( a=1, b=factor( c( "yes", "no")))$b
# factor with levels c( "no", "yes")
everyth( 1:10, 3, 5) # c( 5, 8)
f <- function( a=9, b) expanded.call(); f( 3, 4) # list( a=3, b=4)
find.funs( "package:base", patt="an") # "transform" etc.
find.lurking.envs( cd)
#                                     what  size
#1                     attr(obj, "source")  5368
#2                                     obj 49556
#3 environment(obj) <: namespace:mvbutils>   Inf
\dontrun{
eapply( .GlobalEnv, find.lurking.envs)
}
integ( sin(x), 0, 1) # [1] 0.4597
integ( sin(x+a), a=5, 0, 1) # [1] -0.6765; 'a' is "passed" to 'expr'
integ( sin(y+a), what='y', 0, 1, a=0) # [1] 0.4597; arg is 'y' not 'x'
is.dir( getwd()) # TRUE
isF( FALSE) # TRUE
isF( NA) # FALSE
isF( c( FALSE, FALSE)) # FALSE, with a warning
sapply( c( FALSE, NA, TRUE), isF)
# [1]  TRUE FALSE FALSE
sapply( c( FALSE, NA, TRUE), isT)
# [1] FALSE FALSE  TRUE
legal.filename( "a:b\\\\c/d&f") # "a.b.c.d&f"
most.recent( c( FALSE,TRUE,FALSE,TRUE)) # c( 0, 2, 2, 4)
# mwhere for subsetting: find vowels whose alphetic position is a multiple of 5
df <- data.frame( x=1:10, y=LETTERS[ 1:10])
# Base-R pipes may not exist for the R version being used here
# So, try to parse the expression first...
pp <- try( parse( text=
 'df |> mwhere( x \%\% 5 == 0) |> mwhere( y \%in\% cq( A,E,I,O,U))'
))
if( pp \%is.not.a\% 'try-error') eval( pp[[1]]) # just E-row
sapply( named( cq( alpha, beta)), nchar)  # c( alpha=5, beta=4)
pos( cq( quick, lazy), "the quick brown fox jumped over the lazy dog")
# matrix( c( 5, 37), nrow=2)
pos( "quick", c( "first quick", "second quick quick", "third"))
# matrix( c( 7,8,0, 0,14,0), nrow=3)
pos( "quick", "slow") # matrix( 0)
x <- c( Cat='good', Dog='bad')
rename.els( x, Cat='Armadillo')
# Armadillo       Dog
#    "good"     "bad"
try( rename.els( x, Zorilla='Bandicoot'))
# Error in rename.els(x, Zorilla = "Bandicoot") : all(present) is not TRUE
rename.els( x, Zorilla='Bandicoot', ignore.missing=TRUE)
#    Cat    Dog
# "good"  "bad"
f <- function() \{ a <- 9; return( returnList( a, a*a, a2=a+a)) \}
f() # list( a=9, 81, a2=18)
scatn( 'Things \%i', 1:3)
nscat( 'Things \%i', 1:3)
nscatn( 'Things \%i', 1:3)
to.regexpr( "a{{") # "a\\\\\{\\\\\{"
    test <- undent( r"--{
        I can indent this
          howsoever I like.
        New paragraph!
      }--")
as.cat( test)
glurp <- function( x) const + x
attr( glurp, 'const') <- 44
sourceable( glurp)
# To avoid an intermediate file, use
# ... eval(parse(text=<>))) in place of source
oglurp <- glurp
eval( parse( text= sourceable( klunge))) # will overwrite klunge
identical( klunge, oklunge) # yes
longstring <- 'Bollocks, then the good stuff, then more bollocks'
longstring |> xsub( ',[^,]*$', '') |> xsub( '.*, *', '')
# "then the good stuff"
\dontrun{
mkdir( "subdirectory.of.getwd")
yes.no( "OK (Y/N)? ")
masking( 1)
masked( 5)
}
}
\keyword{misc}

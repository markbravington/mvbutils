\name{numvbderiv_parallel}
\alias{numvbderiv_parallel}
\alias{numvbderiv}
\title{Economy numerical derivatives}
\description{
\code{numvbderiv} does simple two-point symmetric numerical differentiation of any function WRTO a \emph{vector} parameter, via \code{(f(x+delta)-f(x-delta))/(2*delta)}. Your function can return a vector/matrix/array of real or complex type, and if the \code{x} parameter is not scalar, then the result has one extra dimension at the end for the per-parameter-element derivatives.

For multi-parameter (\code{length(x)>=4} or so) derivatives of slow functions, you can speed things up a lot with parallel processing, by setting \code{PARALLEL=TRUE} or (better) by directly calling \code{numvbderiv_parallel}. But, be aware there is substantial learning-curve-pain-cost to all this parallel shenanigans in R{}. \code{numvbderiv_parallel} uses the \pkg{foreach} package to diff wrto each component \code{x[i]} in parallel, using however many cores at a time you tell it to. You have to set up a "parallel cluster" beforehand in R{}. See \bold{Examples}--- it took me a long time to get this working, but now it's good.

\code{numvbderiv} is definitely "economy model" and for many many years I have kept it out of \code{mvbutils}, because it is not particularly accurate nor incredibly robust, and I didn't want to have to deal with people's questions! But I use \code{numvbderiv} and \code{numvbderiv_parallel} all the time in code that I want to share (sometimes with different names, omitting the "mv"), and in 2024 it just became too annoying to have to distribute them separately. So here they are, with nice new names, and tarted-up documentation that you are now enjoying, but still warts and all.
\subsection{Faq}{
\itemize{
\item Q: Surely there are well-known methods to produce more accurate and robust numerical derivatives?
\item A: Yep.
\item Q: I want something more!
\item A: Then use something else!
\item Q: Oh well. But I guess \code{numvbderiv} is easy to use, right?
\item A: Yep.
}

As to accuracy: IME \code{numvbderiv} is usually fine, and computationally cheap! The relevant parameter is \code{eps}; to compute \code{Df(x)/dx|x=x0} your function \code{f} is evaluated at \code{x0+/-eps*x0} (unless \code{x0==0} exactly, in which case it is at \code{+/-eps}). The bigger you go with \code{eps}, the less mathematically accurate the result, since the neglected higher-order terms are bigger; but if you go too small, then the answer becomes computationally inaccurate because of rounding etc. The default is crude but has usually worked OK for me, given this is \emph{not} a high-accuracy routine. I sometimes play around with values between 1e-3 and 1e-7. If you're worried, try two different values that differ by an order-of-mag.

Unlike eg the \pkg{numDeriv} package or \code{pracma::numderiv}, which use more function evaluations at various step-sizes to account for higher-order terms in the finite-difference approximation, \code{numvbderiv} does not try to be \emph{very} accurate, and you do have to specify the step yourself (see \bold{Arguments}) or trust the default. Nevertheless, I expect my \code{numvbderiv} to be \emph{more} accurate than the original (or still-current default) of \code{stats::numericDeriv} because the latter appears \emph{not} to do symmetric calculation, based on the code in "Writing R{} Extensions" section 5.11. IE, it just does \code{(f(x0+e)-f(x0))/e}. [Update in 2024: AFAIK \code{stats::numericDeriv} used never to even have a symmetric option, but it now appears to have added one now via its \code{central} argument--- although that defaults to FALSE :/ .] Also, \code{stats::numericDeriv} is pretty horrible to use TBH; AFAIK its main historical purpose was just to show how to interface C to R{}, not to actually differentiate stuff!
}
}
\usage{
numvbderiv( f, x0, eps=0.0001, param.name=NULL, ...,
  SIMPLIFY=TRUE, PARALLEL=FALSE,
  TWICE.TO.GET.SENSIBLE.ANSWERS=TRUE)
numvbderiv_parallel(f, x0, eps = 0.0001, param.name = NULL, ...,
    SIMPLIFY = TRUE,  PARALLEL = TRUE,
    PROGRESS_BAR = interactive() && .Platform$OS.type!='unix',
    PROGRESS_BAR_FILE = "",  FOREACH_ARGS = list())
}
\arguments{
\item{ f}{function of one or more arguments}
\item{ x0}{value to numdiff around}
\item{ eps}{Relative step-size. Evaluation is at \code{x0+/-eps*x0} unless \code{x0==0} exactly, in which case it is at \code{+/-eps}. See \bold{Faq}.}
\item{ param.name}{Unless the parameter you want to diff WRTO comes first in the argument-list of \code{f}, you need to specify its name, eg \code{param.name="c"} if your function is \code{f(a,b,c)} and you wanna diff wirto the third one.}
\item{ ...}{Other args that your \code{f} wants.}
\item{ SIMPLIFY}{If TRUE and \code{f} appears to return a "scalar-equivalent" result (eg all-but-one of its dimensions are of extent 1, as you can sometimes get eg from a matrix-multiply I guess if you use R{}'s built-in routine), then this will turn the result into a pure vector. Avoids you getting tedious \code{N*1} or \code{1*N} "matrix" results that you then have to \code{c()} yourself.}
\item{ PARALLEL}{if FALSE, use the scalar version. If TRUE \emph{and} \code{length(x0)>1} \emph{and} the \pkg{foreach} package is available \emph{and} there is a "currently registered doPar backend" [sic], then parallel woop-woop magic will be used. \code{numvbderiv/numvbderiv_parallel} have defaults \code{PARALLEL=FALSE/TRUE} respectively.}
\item{ FOREACH_ARGS}{things to pass to \code{foreach::foreach}, eg \code{.packages} or perhaps \code{.exports} so your function can find stuff it needs when it is invoked in a new cold lonely R{} session.}
\item{ PROGRESS_BAR}{If you are bothering to use the parallel version, then presumably things are fairly slow; you can set \code{PROGRESS_BAR=TRUE} to see how it's going. I don't know if it works on Linux, coz it relies on \code{flush.console}, so the default there is FALSE, but you can give it a try.)}
\item{ PROGRESS_BAR_FILE}{I use \code{numvbderiv_parallel} during interactive R{} sessions in RGui, and the default of appearing in the console seems ideal. For other uses, you might need to tell the progress bar to appear somewhere else, via this argument which is passed as the \code{file} argument of \code{\link{txtProgressBar}}.}
\item{ TWICE.TO.GET.SENSIBLE.ANSWERS}{Leave it alone!!! Not for you.}
}
\details{
\subsection{The progress bar}{The progress bar (parallel case only) uses a \code{\link{txtProgressBar}} and some excellent Github code from K Vasilopoulos. It's no good trying to get your own function to show its progress or call-count in the parallel case, because it will be executing in separate invisible R{} processes and messages don't get sent back, so this is the only convenient way. However, the nature of \code{foreach} means that this progress bar is only updated when a task finishes, and since all deriv-steps will take about the same time, you'll probably get the first 4 finishing all-at-once, so that progress will update in a very clunky fashion and if your parameter is of low dimension, the bar may not help. The \code{numvbderiv_parallel} code actually does try to update the progress-bar before the paralleling begins, immediately after the very first function call which is to \code{f(x0)} itself, so in principle you \emph{should} "quickly" get some idea of how long it's all gonna take--- but that update doesn't always seem to show up. Displaying the bar relies on a call to \code{utils::flush.console} (qv) so prolly doesn't work under Unix; maybe there's another way. Future versions of \code{numvbderivParallel} may let you supply your own progress-bar rather than forcing \code{txtProgressBar}. For now, be grateful for what you have been given.
}
}
\value{Normally, an array/matrix with same dimensions as \code{f(x0)} except for an extra one at the end, of \code{length(x0)}. If \code{SIMPLIFY=TRUE} (see \bold{Arguments}) and a pure vector "makes sense", then the dimensions will be stripped and you'll get a pure vector.
}
\seealso{\code{pracma::numderiv}; the \pkg{numDeriv} package; \code{stats::numericDeriv}, the \pkg{foreach} package, the \pkg{doParallel} package
}
\examples{
# Complex numbers are OK:
numvbderiv( function( x) x*x, complex( real=1, imaginary=3))
# [1] 2+6i
# Parallel example...  the whole point is to show speed and generality
# Works fine on my machine
# But if testing under CRAN, which I normally never do,
# then CRAN's ludicrous 2-core limit, and deliberate inability to
# check CRANality (or even number of cores _allowed_) while running,
# makes this completely ridiculous
# Not for the first time
# I have used the function 'get_ncores_CRANal' to try to get round this...
if( require( 'doParallel') && require( 'foreach'))\{
  ncores <- parallel::detectCores( logical=FALSE)
  if( ncores > 2 )\{ # pointless otherwise
    # Need a slowish example. 1e5 is too small; 1e7 better,
    # ... but hard on auto builders eg R-universe
    BIGGOVAL <- 1e5
    slowfun <- function( pars, BIGGO)
      sum( sqr( 1+1/outer( seq_len( BIGGO), pars)))
    parstart <- rep( 2, 8)
    system.time(
      dscalar <- numvbderiv( slowfun, parstart,
          BIGGO=BIGGOVAL # named extra param (part of ...)
        )
    ) # scalar
    # Make "doPar back end". I do not know what I am doing ...
    # NB I like to leave some cores spare, hence "-1"--
    # superstition, really
    ncores_target <- min( ncores-1, length( parstart))
    # Anti CRANky: ignore on your own machine:
    # ncores_target should just work
    ncores_avail <- get_ncores_CRANal( ncores_target)
    CLUSTO <- makeCluster( ncores_avail)
    registerDoParallel( CLUSTO, ncores_avail)
    # Next bit ensures slaves can find packages... sigh.
    # Necessary _here_ coz example, but you may not need it
    # clusterCall does not work properly :/, so the "obvious" fails:
    # clusterCall( CLUSTO, .libPaths, .libPaths())
    # Instead, we are forced into this nonsense:
    eval( substitute(
        clusterEvalQ( CLUSTO, .libPaths( lb)),
        list( lb=.libPaths())))
    # Need 'mvbutils::sqr', hence '.packages' arg
    system.time(
      dpara <- numvbderiv_parallel( slowfun, parstart,
          BIGGO=BIGGOVAL, # named extra parameter
          FOREACH_ARGS=list( .packages= 'mvbutils')
        )
      )
    rbind( dscalar, dpara)
    # To refer to other data (ie beside params)
    # best practice is to put it into function's environment
    # (generally true, not just for numvbderiv)
    e <- new.env()
    e$paroffset <- c( 6, -3)
    fun2 <- function( pars) \{ # not a speed test, can be smaller
        sum( sqr( 1+1/outer( 1:1e3, pars+paroffset)))
      \}
    environment( fun2) <- e
    numvbderiv( fun2, parstart)
    # Parallel version should still work, coz function's environment
    # is also passed to slaves
    try(\{
      numvbderiv_parallel( fun2, parstart,
          FOREACH_ARGS=list( .packages= 'mvbutils')
        )
      \})
    # Sometimes you do need to explicitly export stuff to the slave processes
    # Here's a version that will get paroffset from datenv
    # datenv must exist...
    alt_fun2 <- function( pars)\{
      environment( fun2) <- list2env( datenv)
      fun2( pars)
    \}
    datenv <- as.list( e)
    numvbderiv_parallel( alt_fun2, parstart,
        FOREACH_ARGS=list(
          .packages= 'mvbutils',
          .export= cq( datenv, fun2) # stuff that alt_fun2 refers to
          )
    )
    # Always tidy up your clusters once you have finished playing
    stopImplicitCluster()
    stopCluster( CLUSTO)
    rm( CLUSTO)
  \} # if ncores>2
\} # parallel
}
\keyword{misc}

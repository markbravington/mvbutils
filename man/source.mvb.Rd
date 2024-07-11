\name{source.mvb}
\alias{source.mvb}
\alias{current.source}
\alias{from.here}
\title{Read R code and data from a file or connection}
\description{\code{source.mvb} works like \code{source(local=TRUE)}, except you can intersperse free-format data into your code. \code{current.source} returns the connection that's currently being read by \code{source.mvb}, so you can redirect input accordingly. To do this conveniently inside \code{read.table}, you can use \code{from.here} to read the next lines as data rather than R{} code.
}
\usage{
source.mvb( con, envir=parent.frame(), max.n.expr=Inf,
  echo=getOption( 'verbose'), print.eval=echo,
  prompt.echo=getOption( 'prompt'), continue.echo=getOption( 'continue'))
current.source()
from.here( EOF=as.character(NA)) # Don't use it like this!
# Use "from.here" only inside "read.table", like so:
# read.table( file=from.here( EOF=), ...)
}
\arguments{
\item{ con}{a filename or connection}
\item{ envir}{an environment to evaluate the code in; by default, the environment of the caller of \code{source}}
\item{ max.n.expr}{finish after evaluating \code{max.n.expr} complete expressions, unless file ends first.}
\item{ EOF}{line which terminates data block; lines afterwards will again be treated as R{} statements.}
\item{ ...}{other args to \code{read.table}}
\item{ echo, print.eval, prompt.echo, continue.echo}{as per \code{source}}
}
\details{
Calls to \code{source.mvb} can be nested, because the function maintains a stack of connections currently being read by \code{source.mvb}. The stack is stored in the list \code{source.list} in the \code{mvb.session.info} environment, on the search path. \code{current.source} returns the last (most recent) entry of \code{source.list}.

The sequence of operations differs from vanilla \code{source}, which parses the entire file and then executes each expression in turn; that's why it can't cope with interspersed data. Instead, \code{source.mvb} parses one statement, then executes it, then parses the next, then executes that, etc. Thus, if you include in your file a call to e.g.

\code{text.line <- readLines( con=current.source(), n=1)}

then the next line in the file will be read in to \code{text.line}, and execution will continue at the following line. \code{\link{readLines.mvb}} can be used to read text whose length is not known in advance, until a terminating string is encountered; lines after the terminator, if any, will again be evaluated as R{} expressions by \code{source.mvb}.

After \code{max.n.expr} statements (i.e. syntactically complete R{} expressions) have been executed, \code{source.mvb} will return.

If the connection was open when \code{source.mvb} is called, it is left open; otherwise, it is closed.

If you want to use \code{read.table} or \code{scan} etc. inside a \code{source.mvb} file, to read either a known number of lines or the rest of the file as data, you can use e.g. \code{read.table( current.source(), ...)}.

If you want to \code{read.table} to read an \emph{unknown} number of lines until a terminator, you could explicitly use \code{\link{readLines.mvb}}, as shown in the demo "source.mvb.demo.R". However, the process is cumbersome because you have to explicitly open and close a \code{textConnection}. Instead, you can just use \code{read.table( from.here( EOF=...), ...)} with a non-default \code{EOF}, as in \bold{Usage} and the same demo (but see \bold{Note}). \code{from.here} \emph{shouldn't} be used inside \code{scan}, however, because a temporary file will be left over.

\code{current.source()} can also be used inside a source file, to work out the source file's name. Of course, this will only work if the file is being handled by \code{source.mvb} rather than \code{source}.

If you type \code{source.list} at the R{} command prompt, you should always see an empty list, because all \code{source.mvb} calls should have finished. However, the source list can occasionally become corrupt, i.e. containing invalid connections (I have only had this happen when debugging \code{source.mvb} and quitting before the exit code can clean up). If so, you'll get an error message on typing \code{source.list} (?an R{} bug?). Normally this won't matter at all. If it bothers you, try \code{source.list <<- list()}.
}
\value{\code{source.mvb} returns the value of the last expression executed, but is mainly called for its side-effects of evaluating the code. \code{from.here} returns a connection, of class \code{c( "selfdeleting.file", "file", "connection")}; see \bold{Details}. \code{current.source} returns a connection.
}
\section{Limitations}{Because \code{source.mvb} relies on \code{pushBack}, \code{con=stdin()} won't work.
}
\note{
\code{from.here} creates a temporary file, which should be automatically deleted when \code{read.table} finishes (with or without an error). Technically, the connection returned by \code{from.here} is of class \code{selfdeleting.file} inheriting from \code{file}; this class has a specific \code{close} method, which unlinks the \code{description} field of the connection. This trick works inside \code{read.table}, which calls \code{close} explicitly, but not in \code{scan} or \code{closeAllConnections}, which ignore the \code{selfdeleting.file} class.

\code{from.here()} without an explicit terminator is equivalent to \code{readLines( current.source())}, and the latter avoids temporary files.
}
\seealso{\code{source}, \code{\link{readLines.mvb}}, \code{\link{flatdoc}}, the demo in "source.mvb.demo.R"
}
\examples{
# You wouldn"t normally do it like this:
tt <- tempfile()
cat( "data <- scan( current.source(), what=list( x=0, y=0))",
"27 3",
"35 5",
file=tt, sep="\\n")
source.mvb( tt)
unlink( tt)
data # list( x=c( 27, 35), y=c(3, 5))
# "current.source", useful for hacking:
tt <- tempfile()
cat( "cat( \\\"This code is being read from file\\\",",
"summary( current.source())$description)", file=tt)
source.mvb( tt)
cat( "\\nTo prove the point:\\n")
cat( scan( tt, what="", sep="\\n"), sep="\\n")
unlink( tt)
}
\keyword{misc}

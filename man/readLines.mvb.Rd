\name{readLines.mvb}
\alias{readLines.mvb}
\title{Read text lines from a connection}
\description{Reads text lines from a connection (just like \code{readLines}), but optionally only until a specfied string is found.
}
\usage{  readLines.mvb( con=stdin(), n=-1, ok=TRUE, EOF=as.character( NA), line.count=FALSE)
}
\arguments{
\item{ con}{A connection object or a character string.}
\item{ n}{integer. The (maximal) number of lines to read. Negative values indicate that one should read up to the end of the connection.}
\item{ ok}{logical. Is it OK to reach the end of the connection before `n > 0' lines are read? If not, an error will be generated.}
\item{ EOF}{character. If the current line matches the EOF, it's treated as an end-of-file, and the read stops. The connection is left OPEN so that subsequent reads work.}
\item{ line.count}{(default FALSE) see \bold{Value}.}
}
\details{Apart from stopping if the EOF line is encountered, and as noted with \code{line.count==TRUE}, behaviour should be as for \code{readLines}.
}
\value{A character vector of length the number of lines read. If \code{line.count==TRUE}, it will also have an attribute "line.count" showing the number of lines read.
}
\examples{
tt <- tempfile()
cat( letters[ 1:6], sep="\\n", file=tt)
the.data <- readLines.mvb( tt, EOF="d")
unlink( tt)
the.data # [1] "a" "b" "c"
}
\seealso{\code{\link{source.mvb}}, \code{\link{current.source}}, \code{\link{flatdoc}}
}
\keyword{IO
}

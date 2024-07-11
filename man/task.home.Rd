\name{task.home}
\alias{task.home}
\title{Organizing R workspaces}
\description{Returns file path to current task, or to a file in that task.
}
\usage{
# Often: task.home()
task.home(fname)
}
\arguments{\item{ fname}{file name, a character(1)}
}
\details{
Without any arguments, \code{task.home} returns the path of the current task. With a filename argument, the filename is interpreted as relative to the current task, and its full (non-relative) path is returned.

\code{task.home} is almost obsolete in R{}, since the working directory tracks the current task. It is more important in the S+ version of \code{mvbutils}.
}
\examples{
\dontrun{
task.home( "myfile.c") # probably the same as file.path( getwd(), "myfile.c")
task.home() # probably the same as getwd()
}
}
\seealso{\code{\link{cd}}, \code{getwd}, \code{file.path}
}
\author{Mark Bravington}
\keyword{utilities
}

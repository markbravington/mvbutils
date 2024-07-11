\name{fix.order}
\alias{fix.order}
\title{Shows functions and scriptlets sorted by date of edit}
\description{\code{fix.order} sorts the functions and scriptlets according to the filedates of their backups (in the .Backup.mvb directory). This is very useful for reminding yourself what you were working on recently. It only works if functions and scriptlets have been edited using the \code{\link{fixr}} system.
}
\usage{fix.order( env=1)
}
\arguments{\item{ env}{a single number, character string, or environment. Numbers and characters are interpreted as search path positions. The environment must be an attached mvb-style task.}
}
\details{Only objects that have a BU*** backup file will appear. Objects that have a BU*** file but have been deleted will not appear.
}
\value{Character vector of functions and scriptlets sorted by date/time of last modification.
}
\section{To do}{
Probably should modify this so it takes an arbitrary task path instead of a search position only. Task doesn't really need to be attached.

Add a \code{pattern} argument a la find.funs.
}
\examples{
\dontrun{
## Need to create backups and do some function editing first
fix.order() # functions in .GlobalEnv
fix.order( "ROOT") # functions in your startup task
}
}
\seealso{\code{\link{fixr}}
}
\author{Mark Bravington}
\keyword{utilities
}

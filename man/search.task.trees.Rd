\name{search.task.trees}
\alias{search.task.trees}
\title{Locate loaded tasks on search path.}
\description{Returns the search positions of loaded tasks, with names showing the attached branch of the tree-- see \bold{Examples}.
}
\usage{search.task.trees()
}
\value{Increasing numeric vector with names such as "ROOT", "ROOT/top.task", "ROOT/top.task/sub.task".
}
\seealso{\code{\link{cd}}
}
\examples{
\dontrun{
search.task.trees() # c( ROOT=1) if you haven't used cd yet
cd( mytask)
search.task.trees() # c( "ROOT/mytask"=1, ROOT=2)
}
}
\keyword{misc}

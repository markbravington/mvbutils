\name{set.finalizer}
\alias{set.finalizer}
\title{Obsolete but automatic finalization for persistent objects created in C.}
\description{
[Almost certainly obsolete; \code{.Call} really is the way to go for newer code, complexity notwithstanding.]

Suppose you want to create persistent objects in C-- i.e. objects that can be accessed from R{} by subsequent calls to C. The usual advice is that \code{.C} won't work safely because of uncertain disposal, and that you should use \code{.Call} and "externalptr" types instead. However, \code{.Call} etc is very complicated, and is much harder to use than \code{.C} in e.g. numerical settings. As an alternative, \code{set.finalizer} provides a safe way to ensure that your \code{.C}-created persistent object will tidy itself up when its R{} pointer is no longer required, just as you can with \code{externalptr} objects. There is no need for \code{on.exit} or other precautions.
}
\usage{
# Always assign the result to a variable-- usually a temporary var inside a function...
# ... which R will destroy when the function ends. EG:
# keeper <- set.finalizer( handle, finalizer.name, PACKAGE=NULL)
set.finalizer( handle, finalizer.name, PACKAGE=NULL)
}
\arguments{
\item{ handle}{[integer vector]. Pointer to your object, of length 1 on 32-bit systems or 2 on 64-bit systems. Will have been returned by your object-creation function in C.}
\item{ finalizer.name}{Preferably a "native symbol" corresponding to a registered routine in a DLL; alternatively a string that names your \code{.C}-callable disposal routine. The routine must take exactly one argument, a 32-bit or 64-bit integer (the handle).}
\item{ PACKAGE}{[string] iff \code{finalizer.name} is \code{character}, this is a PACKAGE argument that specifies the DLL.}
}
\details{
You \bold{must} assign the result to a variable, otherwise your object will be prematurely terminated!

\code{set.finalizer} provides a wrapper for R{}'s own \code{reg.finalizer}, setting up a dummy "trigger" environment with a registered finalizer. The trigger is defined as an environment rather than the more obvious choice of an external pointer, because the latter would require me to get fancy with \code{.Call}. The role of \code{reg.finalizer} is to prime the trigger, so that when the trigger is subsequently garbage-collected, your specified \code{.C} function is called to do the finalization.

Note that finalization will only happen after \emph{all copies} of \code{keeper} have been deleted. If you make a "temporary" copy in the global environment, remember to delete it! (Though presumably finalizers are de-registered if R{} is restarted and the keeper is reloaded, so there shouldn't be cross-session consequences.). Finalization won't necessarily happen immediately the last copy is deleted; you can call \code{gc()} to force it.
}
\value{A list with elements \code{handle} and \code{trigger}, the second being the environment that will trigger the call when discarded. The first is the original handle; it has storage mode integer so, as per \bold{Examples}, you don't need to coerce it when subsequently passing it to \code{.C}.
}
\examples{
\dontrun{
myfun <- function( ...) \{
  ...0
  # Create object, return pointer, and ensure safe disposal
  keeper <- set.finalizer( .C( "create_thing", handle=integer(2), ...1)$handle,
      "dispose_of_thing")
  "cause" + "crash" # whoops, will cause crash: but finalizer will still be called
  # "dispose_of_thing" had better be the name of a DLL routine that takes a...
  # ... single integer argument, of length 1 or 2
  # Intention was to use the object. First param of DLL routine "use_thing" should
  # be pointer to thing.
  .C( "use_thing", keeper$handle, ...2)
\}
myfun(...)
}
}
\seealso{.C, .Call, reg.finalizer
}
\keyword{misc}

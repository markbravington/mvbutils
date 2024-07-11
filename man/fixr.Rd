\name{fixr}
\alias{fixr}
\alias{fixtext}
\alias{readr}
\alias{FF}
\alias{autoedit}
\title{Editing functions, text objects, and scriptlets}
\description{
\code{fixr} opens a function (or text object, or "script" stored as an R{} \code{expression}--- see \bold{Scriptlets}) in your preferred text editor. Control returns immediately to the R{} command line, so you can keep working in R{} and can be editing several objects simultaneously (cf \code{edit}). A session-duration list of objects being edited is maintained, so that each object can be easily sourced back into its rightful workspace. These objects will be updated automatically on file-change if you've run \code{autoedit( TRUE)} (e.g. in your \code{.First}), or manually by calling \code{FF()}. There is an optional automatic text backup facility. It is designed to work with the \code{\link{cd}} system, and may well \emph{not}work outside of that system (note that you are automatically in that system if you call \code{library(mvbutils)}).

The safest is to call \code{fixtext} to edit text objects, and \code{fixr} for functions and everything else. However, \code{fixr} can handle both, and for objects that already exist it will preserve the type. For new objects, though, you have to specify the type by calling either \code{fixr} or \code{fixtext}. If you forget--- ie if you really wanted to create a new text object, but instead accidentally typed \code{fixr( mytext)}--- you will (probably) get a parse error, and \code{mytext} will then be "stuck" as a broken function. Your best bet is to copy the actual contents in the text-editor to the clipboard, type \code{fixtext( mytext)} in R{}, paste the old contents into the text-editor, and save the file; R{} will then reset the type and all should be well.

\code{readr} also opens a file in your text editor, but in read-only mode, and doesn't update the backups or the list of objects being edited.

\code{fixr} is designed for interfacing stand-alone text editors with R{}. I've never tried to interface it with e.g. ESS or Rstudio; that \emph{might} be possible, and even desirable, not least because of the next subsection.
\subsection{Packages}{\code{fixr} works with package \pkg{mvbutils} package-maintenance system, and in fact is probably the only way to edit stuff inside that system. If your \emph{maintained} package \code{mypack} (see \code{\link{maintain.packages}}) is already loaded, changes will be reflected immediately in the namespace of \code{mypack}, and also in any packages that import \code{mypack}, and to lists of S3 methods. The sole exception is if your function already exists in \code{asNamespace(mypack)} and consists of a single call to \code{.Call} or \code{.External}. In that case, it is assumed to be an automated wrapper to low-level code that was created when the DLL was loaded, and thus should not be mucked about with; a warning is issued (but it's usually fine). [To force an overwrite without a rebuild, you'd have to first delete the function directly from the namespace--- yikes; but you're probably at a point of needing to rebuild the package anyway.] The newly-edited version will still go into the source-package for \code{mypack}, though, even though it is just a placeholder. The reason you might want to do that, is merely to note the existence of this wrapper-to-low-level (and what its arguments are), and to document it (either internally for use in the package, or for actual export if the user can call it directly).
}
}
\usage{
  # Usually: fixr( x) or fixr( x, new.doc=T)
  fixr( x, new=FALSE, install=FALSE, what, fixing, pkg=NULL,
      character.only=FALSE, new.doc=FALSE, force.srcref=FALSE,
      stop.fixing=character())
  # fixtext really has exact same args as fixr, but technically its args are:
  fixtext( x, ...)
  # Usually: readr( x) but exact same args as fixr, though the defaults are different
  readr( x, ...)
  FF() # manual check and update, usually only needed...
      # ... temporarily if autoedit() stops working
  autoedit( do=TRUE) # stick this line in your .First
}
\arguments{
\item{ x}{a quoted or unquoted name of a function, text object, or expression. You can also write \code{mypack$myfun}, or \code{mypack::myfun}, or \code{mypack:::myfun}, or \code{..mypack$myfun}, to simultaneously set the \code{pkg} argument (only if \code{mypack} has been set up with \code{maintain.packages}). Note that \code{fixr} uses non-standard evaluation of its \code{x} argument, unless you specify \code{character.only=TRUE}. If your object has a funny name, either quote it and set \code{character.only=TRUE}, or pass it directly as...}
\item{ character.only}{(logical or character) if TRUE, \code{x} is treated as a string naming the object to be edited, rather than the unquoted object name. If \code{character.only} is a string, it is treated as the name of \code{x}, so that eg \code{fixr(char="funny\%name")} works.}
\item{ new.doc}{(logical) if TRUE, add skeleton plain-text R-style documentatation, as per \code{add.flatdoc.to}. Also use this to create an empty scriptlet for a general (non-function, non-text) object.}
\item{ force.srcref}{(logical) Occasionally there have been problems transferring old code into "new" R{}, especially when a function has text attributes such as (but not limited to) \code{doc}; the symptom is, they appear in the editor just as "# FLAT-FORMAT DOCUMENTATION". This sometimes requires manual poking-around, but usually can be sorted out by calling \code{fixr(...,force.srcref=TRUE)}.}
\item{ new}{(logical, seldom used) if TRUE, edit a blank function template, rather than any existing object of that name elsewhere in the search path. New edit will go into \code{.GlobalEnv} unless argument \code{pkg} is set.}
\item{ install}{(logical, rarely used) logical indicating whether to go through the process of asking you about your editor}
\item{ what}{Don't use this-- it's "internal"! [Used by \code{fixtext}, which calls \code{fixr} with \code{what=""} to force text-mode object. \code{what} should be an object with the desired class.]}
\item{ fixing}{(logical, rarely used) FALSE for read-only (i.e. just opening editor to examine the object)}
\item{ pkg}{(string or environment) if non-NULL, then specifies in which package a specific maintained package (see \code{\link{maintain.packages}}) \code{x} should be looked for.}
\item{ do}{(logical) TRUE => automatically update objects from altered files; FALSE => don't.}
\item{ ...}{other arguments, except \code{what} in \code{fixtext}, and \code{fixing} in \code{readr}, are passed to \code{fixr}.}
\item{ stop.fixing}{(character vector) removes these items from fix list.}
}
\details{
When \code{fixr} is run for the first time (or if you set \code{install=TRUE}), it will ask you for some basic information about your text editor. In particular, you'll need to know what to type at a command prompt to invoke your text editor on a specific file; in Windows, you can usually find this by copying the Properties/Shortcut/Target field of a shortcut, followed by a space and the filename. After supplying these details, \code{fixr} will launch the editor and print a message showing some \code{options} (\code{"backup.fix"}, \code{"edit.scratchdir"} and \code{"program.editor"}), that will need to be set in your \code{.First}. function. You should now be able to do that via \code{fixr(.First)}.

Changes to the temporary files used for editing can be checked for automatically whenever a valid R{} command is typed (e.g. by typing 0<ENTER>; <ENTER> alone doesn't work). To set this up, call \code{autoedit()} once per session, e.g. in your \code{.First}. The manual version (ie what \code{autoedit} causes to run automatically) is \code{FF()}. If any file changes are detected by \code{FF}, the code is sourced back in and the appropriate function(s) are modified. \code{FF} tries to write functions back into the workspace they came from, which might not be \code{.GlobalEnv}. If not, you'll be asked whether you want to \code{\link{Save}} that workspace (provided it's a task-- see \code{\link{cd}}). \code{FF} should still put the function in the right place, even if you've called \code{\link{cd}} after calling \code{fixr} (unless you've detached the original task) or if you \code{\link{move}}d it. If the function was being \code{mtrace}d (see \code{package?debug}), \code{FF} will re-apply \code{mtrace} after loading the edited version. If there is a problem with parsing, the \code{source} attribute of the function is updated to the new code, but the function body is invisibly replaced with a \code{stop} call, stating that parsing failed.

If something goes wrong during an automatic call to \code{FF}, the automatic-call feature will stop working; this is rare, but can be caused eg by hitting <ESC> while being prompted whether to save a task. To restart the feature in the current R{} session, do \code{autoedit(F)} and then \code{autoedit(T)}. It will come back anyway in a new R{} session.

\code{readr} requires a similar installation process. To get the read-only feature, you'll need to add some kind of option/switch on the command line that invokes your text editor in read-only mode; not all text editors support this. Similarly to \code{fixr}, you'll need to set \code{options( program.reader=<<something>>)} in your \code{.First}; the installation process will tell you what to use.

\code{fixr}, and of course \code{fixtext}, will also edit character vectors. If the object to be edited exists beforehand and has a class attribute, \code{fixr} will not change its class; otherwise, the class will be set to "cat". This means that \code{\link{print}} invokes the \code{\link{print.cat}} method, which displays text more readably than the default. Any other attributes on character vectors are stripped.

For functions, the file passed to the editor will have a ".r" extension. For character vectors or other things, the default extension is ".txt", which may not suit you since some editors decide syntax-highlighting based on the file extension. (EG if the object is a character-vector "R script", you might want R-style syntax highlighting.) You can somewhat control that behaviour by setting \code{options()$fixr.suffices}, eg

\preformatted{
  options( fixr.suffices=c( r='.r', data='.dat'))
}

which will mean that non-function objects whose name ends \code{.r} get written to files ending ".r.r", and objects whose name ends \code{.data} get written to files ending ".data.dat"; any other non-functions will go to files ending ".txt". This does require you to use some discipline in naming objects, which is no bad thing; FWIW my "scripts" always do have names ending in \code{.r}, so that I can see what's what.

\code{fixr} creates a blank function template if the object doesn't exist already, or if \code{new=TRUE}. If you want to create a new character vector as opposed to a new function, call \code{fixtext}, or equivalently set \code{what=""} when you call \code{fixr}.

If the function has attributes, the version in the text editor will be wrapped in a \code{structure(...)} construct (and you can do this yourself). If a \code{doc} attribute exists, it's printed as free-form text at the end of the file, and the call to \code{structure} will end with a line similar to:

\preformatted{
  ,doc=flatdoc( EOF="<<end of doc>>"))
}

When the file is sourced back in, that line will cause the rest of the file-- which should be free-format text, with no escape characters etc.-- to be read in as a \code{doc} attribute, which can be displayed by \code{\link{help}}. If you want to add plain-text documentation, you can also add these lines yourself-- see \code{\link{flatdoc}}. Calling \code{fixr( myfun, new.doc=TRUE)} sets up a documentation template that you can fill in, ready for later conversion to Rd format in a package (see \code{\link{mvbutils.packaging.tools}}).

The list of functions being edited by \code{fixr} is stored in the variable \code{fix.list} in the \code{mvb.session.info} environment. When you quit and restart R{}, the function files you have been using will stay open in the editor, but \code{fix.list} will be empty; hence, updating the file "myfun.r" will not update the corresponding R{} function. If this happens, just type \code{fixr(myfun)} in R{} and when your editor asks you if you want to replace the on-screen version, say no. Save the file again (some editors require a token modification, such as space-then-delete, first) and R{} will notice the update. Very very occasionally, you may want to tell R{} to stop trying to update one of the things it's editing, via eg \code{fixtext <<- fixtext[-3,]} if the offending thing is the third row in \code{fixlist}; note the double arrow.

An automatic text backup facility is available from \code{fixr}: see \code{?get.backup}. The backup system also allows you to sort edited objects by edit date; see \code{?fix.order}.
\subsection{Changes with r 2 14}{
Time was, functions had their source code (including comments, author's preferred layout, etc) stored in a "source" attribute, a simple character vector that was automatically printed when you looked at the function. Thanks to the fiddly, convoluted, opaque "srcref" system that has replaced "source" as of R{} 2.14--- to no real benefit that I can discern--- \code{fixr} in versions of \code{mvbutils} prior to 2.5.209 didn't work correctly with R{} 2.14 up. Versions of \code{mvbutils} after 2.5.509 should work seamlessly.

The technical point is that, from R{} 2.14 onwards, basic R{} will \emph{not} show the \code{source} attribute when you type a function name without running the function; unless there is a \code{srcref} attribute, all you will see is the deparsed raw code. Not nice; so the replacement to \code{\link{print.function}} in \code{mvbutils} will show the \code{source} attribute if it, but no \code{srcref} attribute, is present. As soon as you change a function with \code{fixr} post-R-2.14, it automatically loses any \code{source} attribute and acquires a "proper" \code{srcref} attribute, which will from then on.
}
\subsection{Local function groups}{
There are several ways to work with "nested" (or "child" or "lisp-style macro") functions in R{}, thanks to R{}'s scoping and environment rules; I've used at least four, most often \code{\link{mlocal}} in package \pkg{mvbutils}. One is to keep a bunch of functions together in a \code{\link{local}} environment so that they (i) know about each other's existence and can access a shared variable pool, (ii) can be edited en bloc, but (iii) don't need to clutter up the "parent" code with the definitions of the children. \code{fixr} will happily create & edit such a function-group, as long as you make sure the last statement in \code{local} evaluates to a function. For example:

\preformatted{
  # after typing 'fixr( secondfun)' in R, put this into your text editor:
  local({
    tot <- 0
    firstfun <- function( i) tot <<- tot+i
    # secondfun is defined in the next few lines:
    # entirely optional to precede them with 'secondfun <-'
    function( j) {
        for( ii in 1:j)
          firstfun( ii)
        tot
      }
  })
}

Note that it's \emph{not} necessary to assign the last definition to a variable inside the \code{local} call, unless you want to be able to reach that function recursively from one of the others, as in the first example for \code{local}. Note also that \code{firstfun} will not be visible "globally", only from within \code{secondfun} when it executes.

\code{secondfun} above can be debugged as usual with \code{mtrace} in the \pkg{debug} package. If you want to turn on mtracing for \code{firstfun} without first mtracing \code{secondfun} and manually calling \code{mtrace(firstfun)} when \code{secondfun} appears, do \code{mtrace(firstfun, from=environment( secondfun))}.

\bold{Note}: I \emph{think} all this works OK in normal use (Oct 2012), but be careful! I doubt it works when building a package, and I'm not sure that R-core intend that it should; you might have to put the local-building code into the \code{.onLoad}.
}
\subsection{Scriptlets}{
\bold{Note}: I've really gone off "scriptlets" (writing this in mid 2016). These days I prefer to keep "scripts" as R{} character-vector objects (because I dislike having lots of separate files), edited by \code{fixtext} and manually executed as required by \code{mrun}--- which also has a debugging option that automatically applies \code{debug::mtrace}. I'm not going to remove support for scriptlets in \code{fixr}, but I'm not going to try hard to sort out any bugs either. Instructions below are unchanged, and unchecked, from some years ago.

You can also maintain "scriptlets" with \code{fixr}, by embedding the instructions (and comments etc) in an \code{expression(...)} statement. Obviously, the result will be an \code{expression}; to actually execute a scriptlet after editing it, use \code{eval()}. The scriptlet itself is stored in the "source" attribute as a character vector of class \code{cat}, and the expression itself is given class \code{thing.with.source} so that the source is displayed in preference to the raw expression. Backup files are maintained just as for functions. Only the \emph{first} syntactically complete statement is returned by \code{fixr} (though subsequent material, including extra comments, is always retained in the \code{source} attribute); make sure you wrap everything you want done inside that call to \code{expression(...)}.

Two cases I find useful are:

\itemize{
\item instructions to create data.frames or matrices by reading from a text file, and maybe doing some initial processing;
\item expressions for complicated calls with particular datasets to model-fitting functions such as \code{glm}.
}

\preformatted{
  # Object creator:
  expression( { # Brace needed for multiple steps
    raw.data <- read.table( "bigfile.txt", header=TRUE, row=NULL)
    # Condense date/time char fields into something more useful:
    raw.data <- within( raw.data, {
      Time <- strptime( paste( DATE, TIME, sep=' '), format="\%Y-\%m-\%d \%H:\%M:\%S")
      rm( DATE, TIME)
    })
    cat( "'raw.data' created OK")
  })
}

and

\preformatted{
  # Complicated call:
  expression(
    glm( LHS ~ captain + beard \%in\% soup, data=alldata \%where\% (mushroom=='magic'), family=binomial( link=caterpillar))
  )
}

Bear in mind that \code{eval(myscriptlet)} takes place in \code{.GlobalEnv} unless you tell it not to, so the first example above actually creates \code{raw.data} even though it returns NULL. To trace evaluation of \code{myscriptlet} with the \pkg{debug} package, call \code{debug.eval( myscriptlet)}.

For a new scriptlet \code{mything}, the call to \code{fixr} should still just be \code{fixr(mything)}. However, if you have trouble with this, try \code{fixr( mything, what=list())} instead, even if \code{mything} won't be a \code{list()}. For an existing non-function, you'll need the \code{new=T} argument, e.g. \code{fixr( oldthing, new=T)}, and you'll then have to manually copy/paste the contents.

Note that you \bold{can't} use \code{quote()} instead of \code{expression()}, because any attempt to display the object will cause it to run instead; this is a quirk of S3 methods!
\subsection{For the brave}{
In principle, you can also edit non-expressions the same way. For example, you can create a \code{list} directly (not requiring subsequent \code{eval()}) via a scriptlet like this:

\preformatted{
  list(
    a = 1, # a number
    b = 'aardvark' # a character
  )
}

Nowadays I tend to avoid this, because the code will be executed immediately R{} detects a changed file, and you have no other (easy) control over when it's evaluated. Also, note that the result will have class \code{thing.with.source} (prepended to any other S3 classes it might have), which has its own print method that shows the source; hence you won't see the contents directly when you just type its name, which may or may not be desirable.
}
}
}
\section{Troubleshooting}{
Rarely, \code{fixr} (actually \code{FF}) can get confused, and starts returning errors when trying to update objects from their source files. (Switching between "types" of object with the same name--- function, expression, character vector--- can do this.) In such cases, it can be useful to purge the object from the \code{fix.list}, a session-duration data.frame object in workspace \code{mvb.session.info} on the search path. Say you are having trouble with object "badthing": then

\preformatted{
  fix.list <<- fix.list[ names( fix.list) != 'bad.thing',]
}

will do the trick (note the double arrow). This means \code{FF} will no longer look for updates to the source file for \code{badthing}, and you are free to again \code{fixr( badthing)}.

To purge the entire \code{fix.list}, do this:

\preformatted{
}

 fix.list <<- fix.list[ 0,]
}
\seealso{\code{.First}, \code{edit}, \code{\link{cd}}, \code{\link{get.backup}}, \code{\link{fix.order}}, \code{\link{move}}, \code{\link{maintain.packages}}
}
\keyword{utilities}
\keyword{programming
}

\name{cd}
\alias{cd}
\title{Organizing R workspaces}
\description{\code{cd} allows you to set up and move through a hierarchically-organized set of R{} workspaces, each corresponding to a directory. While working at any level of the hierarchy, all higher levels are attached on the search path, so you can see objects in the "parents". You can easily switch between workspaces in the same session, you can move objects around in the hierarchy, and you can do several hierarchy-wide things such as searching, even on parts of the hierarchy that aren't currently attached.
}
\usage{
# Occasionally: cd()
# Usually: cd(to)
# Rarely:
 cd(to, execute.First = TRUE, execute.Last = TRUE)
}
\arguments{
\item{  to}{the path of a task to move to or create, as an unquoted string. If omitted, you'll be given a menu. See \bold{Details}.}
\item{  execute.First}{should the \code{.First.task} code be executed on attachment? Yes, unless there's a bug in it.}
\item{  execute.Last}{should the \code{.Last.task} code be executed on detachment? Yes, unless there's a bug in it.}
}
\details{
R{} workspaces can become very cluttered, so that it becomes difficult to keep track of what's what (I have seen workspaces with over 1000 objects in them). If you work on several different projects, it can be awkward to work out where to put "shared" functions-- or to remember where things are, if you come back to a project after some months away. And if you just want to test out a bit of code without leaving permanent clutter, but while still being able to "see" your important objects, how do you do it? \code{cd} helps with all such problems, by letting you organize all your projects into a single tree structure, regardless of where they are stored on disk. Each workspace is referred to (for historical reasons) as a "task".

Note that there is a basic choice when working with R{}: do you keep everything you write in a text file which you \code{source} every time you start; or do you store all the objects in a workspace as a binary image in a ".RData" file, and rely on \code{save} and \code{load}? [Hybrids are possible, too.] Some people prefer the text-based approach, but others including me prefer the binary image approach; my reasons are that binary images let me organize my work across tasks more systematically, and that repeated text-sourcing is much too slow when lengthy analyses or data extractions are involved. The \code{cd} system is really geared to the binary image model and, before \code{cd} moves to a new task, either up or down the hierarchy, the current workspace is automatically saved to a binary image. Nevertheless, I don't think \code{cd} is incompatible with other ways of working, as long as the ".RData" file (actually the \code{tasks} object) is not destroyed from session to session. At any rate, some people who work by \code{source}ing large code files still seem to find \code{cd} useful; it's even possible to use the \code{.First.task} feature to auto-load a task's source files into a text editor when you \code{cd} to that task. With the ".RData"-only approach, it is highly advisable to have some way of keeping separate text backups, at least of function code. The \code{\link{fixr}} editing system is geared up to this, and I presume other systems such as ESS are too.

To use the \code{cd} system, you will need to start R{} in the \bold{same} workspace every time. This will become your ROOT or home task, from which all other tasks stem. There need not be much in this workspace except for an object called \code{tasks} (see below), though you can use it for shared functions that you don't want to organize into a package. From the ROOT task, your first action in a new R{} session will normally be to use \code{cd} to switch to a real task. The \code{cd} command is used both to switch between existing tasks, and to create new ones.

To set yourself up for working with \code{cd}, it's probably a good idea to make the ROOT task a completely new blank workspace, so the first step is to (outside R) create an empty folder with some name like "Rstart". [In MS-Windows, you should think about \bold{where} to put this, to save yourself inordinate typing later on. If you are planning to create a completely new set of folders for your R{} projects, you might want to put this ROOT folder near the top of the disk directory structure, rather than in the insane default that Windows proffers, which usually looks something like "c:\\document...\\local...\\long...\\ridiculous...". However, if you are planning instead to link existing folders into the task hierarchy, then it's better to create the ROOT folder just above, or parallel to, the location of these folders.] Start R{} in this folder, type \code{library( mvbutils)}, and then start linking your existing projects into the task hierarchy. [Of course, this assumes that you do have existing projects. If you don't, then just start creating new tasks.] To link in a project, just type \code{cd()} and a menu will appear. The first time, there will be only one option: "CREATE NEW TASK". Select it (or type 0 to quit if you are feeling nervous), and you will be prompted for a "task name", by which R{} will always subsequently refer to the task. Keep the name short; it doesn't have to be related to the location of the disk directory where the .RData lives. Avoid spaces and weird characters-- use periods as separators. Task names are case-sensitive. Next, you'll be asked which disk directory this task refers to. By default, \code{cd} expects that you are creating a new task, and therefore suggests putting the directory immediately below the current task directory. However, if you are linking in an existing project, you'll need to supply the directory name. You can save huge amounts of typing by using "." to refer to the current directory, and on *nix systems you can use "~" too. Next, you'll be returned to the R{} command prompt-- but the prompt will have changed, so that the ">" is preceded by the task name. If you type \code{search()}, you'll see your ROOT task in position 2, below .GlobalEnv as usual. Despite the name, though, the new .GlobalEnv contains the project you've just linked, and if you type \code{ls()}, you should see some familiar objects. Now type \code{cd(0)} to move back to the ROOT task (note the changed prompt), type \code{search()} and \code{ls()} again to orient yourself, and proceed as before to link the rest of your pre-existing tasks into the hierarchy. When you now type \code{cd()}, the menu will have more choices. If you select an existing task rather than creating a new one, you will switch straightaway to that workspace; watch the prompt.

Once you have a hierarchy set up, you can switch the current workspace within the hierarchy by calling e.g. \code{cd(existing.task)} (note the lack of quotes), or by calling \code{cd()} and picking off the menu. You can move through several levels of the hierarchy at once, using a path specifier such as \code{cd(mytask/data/funcs)} or \code{cd(../child.of.sibling)}. Path specifiers are just like Unix or Windows disk paths with "/" as the separator, so that "." means "current task" and ".." means "parent". However, the character 0 must be used to denote the ROOT task, so that you have to type \code{cd(0/different.task)} rather than \code{cd(/different.task}). You can display the entire hierarchy by calling \code{cdtree(0)}, or graphically via \code{plot( cdtree( 0))}.

When you first set up your task hierarchy, you'll also want to create or modify the \code{.First} function in your ROOT task. At a minimum, this should call \code{library( mvbutils)}, but you may also want to set some options controlling the behaviour of \code{cd} (see the \bold{Options} section). If you use other features of \code{mvbutils} such as the function-editing interface in \code{\link{fixr}}, there will be further options to be set in \code{.First}. [MAC users: for some strange reason \code{.First} just doesn't get called if you are using the "usual" RGUI for MACs. So what you need to do is create a ".Rprofile" file in your ROOT folder using a text editor; this file should both contain the definition of the \code{.First} function, and should also call \code{.First()} directly. You can also put the \code{.First} commands directly into the ".Rprofile" file, but watch out for the side-effect of creating objects in \code{.GlobalEnv}.]

You can create a fully hierarchical structure, with subtasks within subtasks within tasks, etc. Even if your projects don't naturally look like this, you may find the facility useful. When I create a new task, I tend to start with just one level of hierarchy, containing data, function code, and results. When this gets unspeakably messy, I often create one (or more) subtasks, usually putting the basic data at the top level, and functions and results at the lower level. Apart from tidiness, this provides some degree of protection against overwriting the original data. And when even this gets too messy-- in one task, I have more than 150 functions, and it is very easy to generate 100s of analysis results-- I create another level, keeping "established" functions at the second tier and using the third tier for temporary workspace and results. There are no hard-and-fast rules here, of course, and different people use R{} in very different ways.

A task can have \code{.First.task} and/or \code{.Last.task} functions, which get called immediately after \code{cd}ing into the task from its parent, or immediately before \code{cd}ing back to its parent, respectively (see \bold{Arguments}). These can be useful for dynamic loading, loading scripts into a text editor, attaching & detaching datasets, etc., and facilitate the use of tasks as informal packages.

For turning tasks into formal R{} packages, consult \code{\link{mvbutils.packaging.tools}}.
}
\section{How it works}{
The mechanism underlying the tree structure is very simple: each task that has any subtasks will contain a character vector called \code{tasks}, whose names are the R{} names of the tasks, and whose elements are the corresponding disk directories. Your ROOT task need contain no more than a \code{.First} function and a \code{tasks} object.

You can manually modify the \code{tasks} vector, and sometimes this is essential. If you decide to move a disk directory, for example, you can manually change the corresponding element of \code{tasks} to reflect the change. (Though if you are moving a whole task hierarchy, e.g. when migrating to a new machine, consult \code{\link{cd.change.all.paths}}. Having said that, the ability to use relative pathnames in tasks, which is present since about mvbutils version 2.0, makes \code{\link{cd.change.all.paths}} partly redundant.) You can also rename a task very easily, via something like

\preformatted{
  names( tasks)[ names( tasks)=="my.old.name"] <- "my.new.name"
}

You can use similar methods to "reparent" a subtask without changing the directory structure.

There is (deliberately, to avoid accidents) no completely automatic way of removing tasks. To "hide" a task from the \code{cd} system, you first need to be \code{cd}ed to its parent; then remove the corresponding element of the \code{tasks} object, most easily via e.g.

\preformatted{
  tasks <- tasks \%without.name\% "mysubtask"
}

If you want to remove the directories corresponding to "mysubtask", you have to do so manually, either in the operating system or (for the brave) in R{} code.

Remember to \code{Save()} at some point after manually modifying \code{tasks}.
}
\section{Options}{
Various \code{options()} can be set, as follows. Remember to put these into your \code{.First} function, too.

\code{write.mvb.tasks=TRUE} causes a sourceable text representation of the \code{tasks} object to be maintained in each directory, in the file \code{tasks.r}. This helps in case you accidentally wipe out the .RData file and lose track of where the child tasks live. To create these text representations for the first time throughout the hierarchy, call \code{cd.write.mvb.tasks(0)}. You need to put the the \code{options} call in your \code{.First}.

\code{abbreviate.cdprompt=n} controls the length of the prompt string. Only the first \code{n} characters of all ancestral task names will be shown. For example, \code{n=1} would replace the prompt \code{long.task.name/data/funcs>} with \code{l/d/funcs>}.

\code{mvbutils.update.history.on.cd=FALSE} will prevent automatic saving & reloading of the history file when \code{cd} is called.

\code{cd} checks the \code{R_HISTFILE} environment variable and, if unset, sets it to \code{file.path( getwd()), ".Rhistory")}. This (combined with the \code{mvbutils} replacement of the standard versions of \code{savehistory} and \code{loadhistory}-- see \code{package?mvbutils}) ensures that the same history file is used throughout each and every R{} session. My experience is that a single master history file is safer. However, if you want to override this behaviour-- e.g. if you want to use a separate history file for each task-- call something like \code{Sys.setenv( R_HISTFILE=".Rhistory")} \bold{before} the \bold{first} use of \code{cd}.
}
\note{
\code{cd} calls \code{setwd} so that file searches will default to the task directory (see also \code{\link{task.home}}).

\code{cd} always calls \code{\link{Save}} before attaching a child task on top or moving back up the hierarchy. If you have many and/or big objects, the default behaviour can be slow. You can speed this up-- sometimes dramatically-- by "mcacheing" some of your objects so that they are stored in separate files-- see \code{\link{mlazy}}.

If there are no changes to the ".RData" file, \code{cd} will not modify the file-- in particular, its date-of-access will be unchanged. This helps avoid unnecessary file copying on subsequent synchronization. However, there are several seemingly innocuous operations which change the workspace: calling a random number function (changes \code{.Random.seed}), causing an error (creates \code{.Traceback}), and causing a warning (creates \code{last.warning}). To avoid forcing a change to the entire ".RData" file whenever one of these changes, you can set \code{option( mvbutils.quick.cd=TRUE)}; this turns on \code{mcache}ing for those objects (see \code{\link{mlazy}}), so that they are stored in separate mini-files.

\code{cd} is only meant to be called interactively, and has only been tested in that context.

\code{cd} will issue a warning and refuse to move back up the hierarchy if it detects a non-task attached in position 2. You will need to manually detach any such objects before \code{cd}ing back up, or write a \code{.Last.task} function to automatically do the detaching. To make sure that \code{library} (and any automatic loading of packages, e.g. if triggered by \code{load}ing a file referring to a namespace) always inserts packages below ROOT, the \code{.onLoad} code in \code{mvbutils} makes a minor hack to \code{library}, changing the default \code{\link{pos}} argument accordingly.

Two objects in the \code{mvb.session.info} search environment (see \code{search()}) help keep track of what parts of the hierarchy are currently attached; \code{.First.top.search} and \code{.Path}. The former is set when \code{mvbutils} loads, and the latter is updated by \code{cd}. Attached tasks can be identified by having a \code{path} attribute consisting of a \emph{named} character vector. Normal packages also have a \code{path} attribute, but without \code{names}.
}
\seealso{\code{\link{move}}, \code{\link{task.home}}, \code{\link{cdtree}}, \code{\link{cdfind}}, \code{\link{cditerate}}, \code{\link{cd.change.all.paths}}, \code{\link{cd.write.mvb.tasks}}, \code{\link{cdprompt}}, \code{\link{fixr}}, \code{\link{mlazy}}
}
\author{Mark Bravington}
\keyword{utilities
}

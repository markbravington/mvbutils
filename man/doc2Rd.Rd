\name{doc2Rd}
\alias{doc2Rd}
\alias{docotest}
\title{Converts plain-text documentation to Rd format}
\description{
\code{doc2Rd} converts plain-text documentation into an Rd-format character vector, optionally writing it to a file. You probably won't need to call \code{doc2Rd} yourself, because \code{\link{pre.install}} and \code{\link{patch.install}} do it for you when you are building a package; the entire documentation of package \pkg{mvbutils} was produced this way. The main point of this helpfile is to describe plain-text documentation details. However, rather than wading through all the material below, just have a look at a couple of R{}'s help screens in the pager, e.g. via \code{help( glm, help_type="text")}, copy the result into a text editor, and try making one yourself. Don't bother with indentation, except in item lists as per \bold{More details} below (the pager's version is not 100\% suitable). See \code{\link{fixr}} and its \code{new.doc} argument for how to set up an empty template: also \code{\link{help2flatdoc}} for how to convert existing Rd-format doco.

\code{docotest} allows you to quickly check what your plain-text doco would look life
}
\usage{
doc2Rd( text, file=NULL, append=, warnings.on=TRUE, Rd.version=,
    def.valids=NULL, check.legality=TRUE)
docotest( fun.or.text, ...)
}
\arguments{
For \code{doc2Rd}:
\item{ text}{(character or function) character vector of documentation, or a function with a \code{doc} attribute that is a c.v. of d..}
\item{ file}{(string or connection) if non-NULL, write the output to this file}
\item{ append}{(logical) only applies if \code{!is.null(file)}; should output be appended rather than overwriting?}
\item{ warnings.on}{(logical) ?display warnings about apparently informal documentation?}
\item{ Rd.version}{(character) what Rdoc version to create "man" files in? Currently "1" means pre-R2.10, "2" means R2.10 and up. Default is set according to what version of R{} is running.}
\item{ def.valids}{(character) objects or helpfiles for which links should be generated automatically. When \code{doc2Rd} is being called from \code{pre.install}, this will be set to all documented objects in your package. Cross-links to functions in other packages are not currently generated automatically (in fact not at all, yet).}
\item{ check.legality}{if TRUE and \code{Rd.version} is 2 or more, then the output Rd will be run thru \code{parse_Rd} and a \code{try-error} will be returned if that fails; normal return otherwise. Not applicable if \code{Rd.version} is 1.}
For \code{docotest}:
\item{ fun.or.text}{(character or function) character vector of documentation, or a function with a \code{doc} attribute that is a c.v. of d.. NB if maintaining a package, you need to run this on the "raw" code (e.g. \code{..mypack$myfun}), not on the installed function (e.g. not \code{myfun} or \code{mypack::myfun}).}
\item{ ...}{other args passed to \code{Rd2HTML} when it tries to convert \code{doc2Rd} output to HTML. I've no idea what these might be, since they wouldn't be used in reality by \code{pre.install} when it assembles your source package.}
}
\value{Character vector containing the text as it would appear in an Rd file, with \code{class} of "cat" so it prints nicely on the screen.
}
\section{More details}{
Flat-format (plain-text) documentation in \code{doc} attributes, or in stand-alone character objects whose name ends with ".doc", can be displayed by the replacement \code{\link{help}} in \code{mvbutils} (see \code{\link{dochelp}}) without any further ado. This is very useful while developing code before the package-creation stage, and you can write such documentation any way you want. For display in an HTML browser (as opposed to R{}'s internal pager), and/or when you want to generate a package, \code{doc2Rd} will convert pretty much anything into a legal Rd file. However, if you can follow a very few rules, using \code{doc2Rd} will actually give nice-looking authentic R{} help. For this to work, your documentation basically needs to look like a plain-text help file, as displayed by \code{help(..., help_type="text")}.

Rather than wading through this help file to work out how to write plain-text help, just have a look at a couple of R{}'s help screens in the pager, and try making one yourself. You can also use \code{\link{help2flatdoc}} to convert an existing plain-text help file. Also check the file "sample.fun.rrr" in the "demostuff" subdirectory of this package (see \bold{Examples}). If something doesn't work, delve more deeply...

\itemize{
\item There are no "escape characters"-- the system is "text WYSIWYG". For example, if you type a \\ character in your doc, \code{\link{help}} will display a \\ in that spot. Single quotes and percent signs can have special implications, though-- see below.
\item Section titles should either be fully capitalized, or end with a : character. The capitalized version shows up more clearly in informal help. Replace any spaces with periods, e.g. SEE.ALSO not SEE ALSO. The only non-alpha characters allowed are hyphens.
\item Subsections are like sections, except they start with a sequence of full stops, one per nesting level. See also \bold{Subsections}.
\item "Item lists", such as in the \bold{Arguments} section and sometimes the \bold{Value} section (and sometimes other sections), should be indented and should have a colon to separate the item name from the item body.
\item General lists of items, like this bullet-point list, should be indented and should start with a "-" character, followed by a space.
\item Your spacing is generally ignored (exceptions: \bold{Usage}, \bold{Examples}, multi-line code blocks; see previous point). Tabs are converted to spaces. Text is wrapped, so you should write paragraphs as single lines without hard line breaks. Use blank lines generously, to make your life easier; also, they will help readability of informal helpfiles.
\item To mark \emph{in-line} code fragments (including variable names, package names, etc-- basically things that R{} could parse), put them in single quotes. Hence you can't use single quotes within in-line code fragments.
}

#ifdef flub

This flatdoc help file can't show you an example of what you can't do in a flatdoc help file!

#endif

An example of what you couldn't include:

\code{'myfun( "'No no no!'")'}

\itemize{
\item Single quotes are OK within multi-code blocks, \bold{Usage}, and \bold{Examples}. For multi-line code blocks in other sections, don't bother with the single-quotes mechanism. Instead, insert a "\%\%#" line before the first line of the block, and make sure there is a blank line after the block.
\item You can insert "hidden lines", starting with a \% character, which get passed to the Rd conversion routines. If the line starts with \%\%, then the Rd conversion routines will ignore it too. The "\%\%#" line to introduce multi-line code blocks is a special case of this.
\item Some other special constructs, such as links, can be obtained by using particular phrases in your documentation, as per \bold{Special fields}.
}
\subsection{Subsections}{
 I've bolded some of these meta-refs to sections

Subsections are a nice new feature in R{} 2.11. You can use them to get better control over the order in which parts of documentation appear. R{} will order sections thus: \bold{Usage}, \bold{Arguments}, \bold{Details}, \bold{Value}, other sections you write in alphabetical section order, \bold{Notes}, \bold{See also}. That order is not always useful. You can add subsections to \bold{Details} so that people will see them in the order you want. If you want \bold{Value} to appear before \bold{Details}, then just rename \bold{Details} to "MORE.DETAILS", and put subsections inside that.

In plain-text, subsection headings are just like section headings, except they start with a period (don't use the initial periods when cross-referencing to it elsewhere in the doco). You can have nested subsections by adding extra periods at the start, like this:
\subsection{Another depth of nesting}{In the plain text version of this doco, the SUBSECTIONS line starts with one period, and the ANOTHER.DEPTH.OF.NESTING line starts with two. If you try to increase subsection depth by more than one level, i.e. with 2+ full stops more than the previous (sub)section, then \code{doc2Rd} will correct your "mistake".
}
}
\subsection{Special fields}{
Almost anything between a pair of single quotes will be put into a \\code\{\} or \\code\{\\link\{\}\} or \\pkg\{\} or \\env\{\} construct, and the quotes will be removed. A link will be used if the thing between the quotes is a one-word name of something documented in your package (assuming \code{doc2Rd} is being called from \code{\link{pre.install}}). A link will also be used in all cases of the form "See XXX" or "see XXX" or "XXX (qv)", where XXX is in single quotes, and any " (qv)" will be removed. With "[pP]ackage XXX" and "XXX package", a \\pkg\{\} construct will be used. References to \code{.GlobalEnv} and \code{.BaseNamespaceEnv} go into \\env\{\} constructs. Otherwise, a \\code\{\} construct will be used, unless the following exceptions apply. The first exception is if the quotes are inside \bold{Usage}, \bold{Examples}, or a multi-line code block. The second is if the first quote is preceded by anything other than " ", "(" or "-". The final semi-exception is that a few special cases are put into other constructs, as next.

URLs and email addresses should be enclosed in <...>; they are auto-detected and put into \\url\{\} and \\email\{\} constructs respectively.

Lines that start with a \% will have the \% removed before conversion, so their contents will be passed to RCMD Rdconv later (unless you start the line with \%\%). They aren't displayed by \code{\link{dochelp}}, though, so can be used to hide an unhelpful USAGE, say, or to hide an "#ifdef windows".

A solitary capital-R is converted to \\R. Triple dots \emph{used to be} converted to \\dots (regardless of whether they're in code or normal text) but I've stopped doing so because this conversion was taking 97\% of the total runtime!

Any reasonable "*b*old" or "_emphatic stuff_" constructions (no quotes, just the asterisks) will go into \\bold\{\} and \\emph\{\} constructs respectively, to give \bold{b}old or \emph{emphatic stuff}. (Those first two didn't, because they are "unreasonable"-- in particular, they're quoted.) No other fancy constructs are supported (yet).
}
\subsection{Format for non-function help}{For documenting datasets, the mandatory sections seem to be \bold{Description}, \bold{Usage}, and \bold{Format}; the latter works just like \bold{Arguments}, in that you specify field names in a list. Other common sections include \bold{Examples}, \bold{Source}, \bold{References}, and \bold{Details}.
}
\subsection{Extreme details}{
The first line should be the docfile name (without the Rd) followed by a few spaces and the package descriptor, like so:

utility-funs package:mypack

When \code{doc2Rd} runs, the docfile name will appear in both the \\name\{\} field and the first \\alias\{\} field. \code{\link{pre.install}} will actually create the file "utility-funs.Rd". The next non-blank lines form the other alias entries. Each of those lines should consist of one word, preceded by one or more spaces for safety (not necessary if they have normal names).

"Informal documentation" is interpreted as any documentation that doesn't include a "DESCRIPTION" (or "Description:") line. If this is the case, \code{doc2Rd} first looks for a blank line, treats everything before it as \\alias\{\} entries, and then generates the \bold{Description} section into which all the rest of your documentation goes. No other sections in your documentation are recognized, but all the special field substitutions above are applied. (If you really don't want them to be, use the multi-line code block mechanism.) Token \bold{Usage}, \bold{Arguments}, and \bold{Keywords} sections are appended automatically, to keep RCMD happy.

Section titles built into Rd are: \bold{Description}, \bold{Usage}, \bold{Synopsis} (defunct for R>=3.1), \bold{Arguments}, \bold{Value}, \bold{Details}, \bold{Examples}, \bold{Author} or \bold{Author(s)}, \bold{See also}, \bold{References}, \bold{Note}, \bold{Keywords} and, for data documentation only, \bold{Format} and \bold{Source}. Other section titles (in capitals, or terminated with a colon) can be used, and will be sentence-cased and wrapped in a \\section\{\} construct. Subsections work like sections, but begin with a sequence of full stops, one per nesting level. Most cross-refs to (sub)sections will be picked up automatically and put into \bold{bold}, so that e.g. "see MY.SECTION" will appear as "see \bold{My section}"; when referring to subsections, omit the initial dots. To force a cross-reference that just doesn't want to appear, use e.g. "MY.SECTION (qv)", or just wrap it in "*...*".

The \\docType field is set automatically for data documentation (iff a \bold{Format} section is found) and for package documentation (iff the name on the first line includes "-package").

Spacing within lines does matter in \bold{Usage} (qv), \bold{Examples}, and multi-line code blocks, where what you type really is what you get (except that a fixed indent at the start of all lines in such a block is removed, usually to be reinstated later by the help facilities). The main issue is in the package "manual" that RCMD generates for you, where the line lengths are very short and overflows are common. (Overflows are also common with in-line code fragments, but little can be done about that.) The "RCMD Rd2dvi --pdf" utility is helpful for seeing how individual helpfiles come out.

In \bold{See also}, the syntax is slightly different; names of things to link to should \emph{not} be in single quotes, and should be separated by commas or semicolons; they will be put into \\code\{\\link\{\}\} constructs. You can split SEE.ALSO across several lines; this won't matter for pager help, but can help produce tidier output in the file "***-manual.tex" produced by RCMD CHECK.

In \bold{Examples}, to designate "don't run" segments, put a "## Don't run" line before and a "## End don't run" line after.

I never bother with \bold{Keywords}, but if you do, then separate the keywords with commas, semicolons, or line breaks; don't use quotes. A token \bold{Keywords} section will be auto-generated if you don't include one, to keep RCMD happy.
}
\subsection{Infrequently asked questions}{
\bold{Q:} Why didn't you use Markdown/MyPetBargainSyntax?

\bold{A:} Mainly because I didn't know about them, to be honest. But WRTO MarkDown it seemed to me that the hard-line-breaks feature would be a pain. If anyone thinks there's really good alternative standard, please let me know.

 \bold{Q:} I have written a fancy \emph{displayed} equation using \\deqn\{\} and desperately want to include it. Can I?

 \bold{A:} Yes (though are you sure that a fancy equation really belongs in your function doco? how about in an attached PDF, or vignette?). Just prefix all the lines of your \\deqn with \%. If you want something to show up in informal help too, then make sure you also include lines with the text version of the equation, as per the next-but-one question.

 \bold{Q:} I have written a fancy \emph{in-line} equation using \\eqn\{\} and desperately want to include it. Can I?

 \bold{A:} No. Sorry.

 \bold{Q:} For some reason I want to see one thing in informal help (i.e. when the package isn't actually loaded but just sitting in a task on the search path), but a different thing in formal help. Can I do that?

 \bold{A:} If you must. Use the \%-line mechanism for the formal help version, and then insert a line "\%#ifdef flub" before the informal version, and a line "\%#endif" after it. Your text version will show up in informal help, and your fancy version will show up in all help produced via Rd. (Anyone using the "flub" operating system will see both versions...)

 \bold{Q:} How can I insert a file/kbd/samp/option/acronym etc tag?

 \bold{A:} You can't. They all look like single quotes in pager-style help, anyway.

 \bold{Q:} What about S3?

 \bold{A:} S3 methods often don't need to be documented. However, they can be documented just like any other function, except for one small detail: in the \bold{Usage} section, the call should use the generic name instead of your method name, and should be followed by a comment "# S3 method for <class>"; you can append more text to the comment if you wish. E.G.: if you are documenting a method \code{\link{print.cat}}, the \bold{Usage} section should contain a call to \code{print(x,...) # S3 method for cat} rather than \code{print.cat(x,...)}. The version seen by the user will duplicate this "S3 method..." information, but never mind eh.

 If you are also (re)defining an S3 generic and documenting it in the same file as various methods, then put a comment \code{# generic} on the relevant usage line. See \code{?print.function} for associated requirements.

 Confusion will deservedly arise with a function that looks like an S3 method, but isn't. It will be not be labelled as S3 by \code{\link{pre.install}} because you will of course have used the full name in the \bold{Usage} section, because it isn't a method. However, it can still be found by \code{NextMethod} etc., so you shouldn't do that. (Though \code{mvbutils::max.pkg.ver} currently does exactly that...)

 S3 classes themselves need to be documented either via a relevant method using an alias line, or via a separate \code{myclass.doc} text object.

 \bold{Q:} What about S4?

 \bold{A:} I am not a fan of S4 and have found no need for it in many 1000s of lines of R{} code... hence I haven't included any explicit support for it so far. Nevertheless, things might well work anyway, unless special Rd constructs are needed. If \code{doc2Rd} \emph{doesn't} work for your S4 stuff (bear in mind that the \%-line mechanism may help), then for now you'll still have to write S4 Rd files yourself; see \code{\link{pre.install}} for where to put them. However, if anyone would like the flatdoc facility for S4 and is willing to help out, I'm happy to try to add support.
}
}
\seealso{
The file "sample.fun.rrr" in subdirectory "demostuff", and the demo "flatdoc.demo.r".

To do a whole group at once: \code{\link{pre.install}}.

To check the results: \code{docotest(myfun)} to check the HTML (or \code{patch.installed(mypack)} and then \code{?myfun}). TODO something to easily check PDF (though R{}'s PDF doco is pointless IMO); for now you need to manually generate the file, then from a command-line prompt do something like "RCMD Rd2dvi --pdf XXX.Rd" and "RCMD Rdconv -t=html XXX.Rd" and/or "-t=txt"

To convert existing Rd documentation: \code{\link{help2flatdoc}}.

If you want to tinker with the underlying mechanisms: \code{\link{flatdoc}}, \code{\link{write.sourceable.function}}
}
\examples{
## Needs a function with the right kind of "doc" attr
## Look at file "demostuff/sample.fun.rrr"
sample.fun <- source.mvb( system.file( file.path(
    'demostuff', 'sample.fun.rrr'), package='mvbutils'))
print( names( attributes( sample.fun)))
cat( '***Original plain-text doco:***\\n')
print( as.cat( attr( sample.fun, 'doc'))) # unescaped, ie what you'd actually edit
cat( '\\n***Rd output:***\\n')
sample.fun.Rd <- doc2Rd( sample.fun)
print( sample.fun.Rd) # already "cat" class
\dontrun{
docotest( sample.fun) # should display in browser
}
}
\keyword{programming
}

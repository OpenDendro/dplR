\name{latexify}
\alias{latexify}
\title{
  Convert Character Strings for Use with LaTeX
}
\description{
  Some characters cannot be entered directly into a LaTeX document.
  This function converts the input \code{character} vector to a form
  suitable for inclusion in a LaTeX document in text mode.  It can be
  used together with \samp{\Sexpr} in vignettes.
}
\usage{
latexify(x, doublebackslash = TRUE, dashdash = TRUE,
         quotes = c("straight", "curved"),
         packages = c("fontenc", "textcomp"))
}
\arguments{
  \item{x}{ a \code{character} vector }
  \item{doublebackslash}{ a \code{logical} flag.  If \code{TRUE},
    backslashes in the output are doubled.  It seems that Sweave needs
    \code{TRUE} and knitr \code{FALSE}. }
  \item{dashdash}{ a \code{logical} flag.  If \code{TRUE} (the default),
    consecutive dashes (\dQuote{-}) in the input will be rendered as
    separate characters.  If \code{FALSE}, they will not be given any
    special treatment, which will usually mean that two dashes are
    rendered as an en dash and three dashes make an em dash. }
  \item{quotes}{ a \code{character} string specifying how single and
    double quotes (\acronym{ASCII} codes 39 and 34) and stand-alone
    grave accents (\acronym{ASCII} code 96) in the input are treated.
    The default is to use straight quotes and the proper symbol for the
    grave accent.  The other option is to use curved right side
    (closing) quotes, and let LaTeX convert the grave accent to opening
    curved quotes.  Straight double quotes are not available in the
    default OT1 font encoding of LaTeX.  Straight single quotes and the
    grave accent symbol require the \dQuote{textcomp} package.  See
    \code{\var{packages}}. }
  \item{packages}{ a \code{character} vector specifying the LaTeX
    packages allowed.  The use of some symbols in LaTeX requires
    commands or characters made available by an add-on package.  If a
    package required for a given character is not marked as available, a
    fallback solution is silently used.  For example, curved quotes may
    be used instead of straight quotes.  The supported packages are
    \code{"eurosym"} (not used by default), \code{"fontenc"} and
    \code{"textcomp"}.  Including \code{"fontenc"} in the vector means
    that some other encoding than OT1 is going to be used.  Note that
    straight quotes are an exception in the sense that a reasonable
    substitute (curved quotes) is available.  In many other cases,
    \code{"textcomp"} and \code{"fontenc"} are silently assumed. }
}
\details{

  The function is intended for use with unformatted inline text.
  Newlines, tabs and other whitespace characters (\code{"[:space:]"} in
  \link{regex}) are converted to spaces.  Control characters
  (\code{"[:cntrl:]"}) that are not whitespace are removed.  Other more
  or less special characters in the \acronym{ASCII} set are \sQuote{\{},
  \sQuote{\}}, \sQuote{\\}, \sQuote{#}, \sQuote{$}, \sQuote{\%},
  \sQuote{^}, \sQuote{&}, \sQuote{_}, \sQuote{~}, double quote,
  \sQuote{/}, single quote, \sQuote{<}, \sQuote{>}, \sQuote{|}, grave
  and \sQuote{-}.  They are converted to the corresponding LaTeX
  commands.  Some of the conversions are affected by user options,
  e.g. \code{\var{dashdash}}.

  Before applying the substitutions described above, input elements with
  \code{\link{Encoding}} set to \code{"bytes"} are printed and the
  result is captured using the current text encoding.  The result of
  this intermediate stage is \acronym{ASCII} text where some characters
  are shown as their byte codes using a hexadecimal pair prefixed with
  \code{"\\x"}.  This set includes tabs, newlines and control
  characters.  The substitutions are then applied to the intermediate
  result.

  The quoting functions \code{\link{sQuote}} and \code{\link{dQuote}}
  may use non-\acronym{ASCII} quote characters, depending on the locale.
  Also these quotes are converted to LaTeX commands.  This means that
  the quoting functions are safe to use with any LaTeX input encoding.
  Similarly, some other non-\acronym{ASCII} characters, e.g. letters,
  currency symbols, punctuation marks and diacritics, are converted to
  commands.

  Adding \code{"eurosym"} to \code{\var{packages}} enables the use of the
  euro sign as provided by the \code{"eurosym"} package (\samp{\euro}).

  The result is converted to UTF-8 encoding, Normalization Form C (NFC).
  Note that this function will not add any non-\acronym{ASCII}
  characters that were not already present in the input.  On the
  contrary, some non-\acronym{ASCII} characters, e.g. all characters in
  the \code{"latin1"} (\acronym{ISO}-8859-1) \code{\link{Encoding}}
  (character set), are removed when converted to LaTeX commands.  Any
  remaining non-\acronym{ASCII} character has a good chance of working
  when the document is processed with XeTeX or LuaTeX, but the Unicode
  support available with pdfTeX is limited.

  Assuming that \samp{pdflatex} is used for compilation, suggested
  package loading commands in the document preamble are: \preformatted{\usepackage[T1]{fontenc}    \% no '"' in OT1 font encoding
\usepackage{textcomp}       \% some symbols e.g. straight single quote
\usepackage[utf8]{inputenx} \% UTF-8 input encoding
\input{ix-utf8enc.dfu}      \% more supported characters} 
  
}
\value{
  A \code{character} vector
}
\references{
  INRIA.  Tralics: a LaTeX to XML translator, HTML documentation of all
  TeX commands.  \url{http://www-sop.inria.fr/marelle/tralics/}

  Levitt, N., Persch, C. and Unicode, Inc. (2013) GNOME Character Map,
  application version 3.10.1.

  Mittelbach, F. and Goossens, M. with Braams, J., Carlisle, D. and
  Rowley, C. (2004) \emph{The LaTeX Companion, Second Edition.}
  Addison-Wesley.  \acronym{ISBN-13}: 978-0-201-36299-2.

  Pakin, S. (2009) The Comprehensive LaTeX Symbol
  List. \url{http://www.ctan.org/tex-archive/info/symbols/comprehensive}

  The Unicode Consortium.  The Unicode Standard.
  \url{http://www.unicode.org/versions/latest/}
}
\author{
  Mikko Korpela
}
\examples{
x1 <- "clich\xe9\nma\xf1ana"
Encoding(x1) <- "latin1"
x1
x2 <- x1
Encoding(x2) <- "bytes"
x2
x3 <- enc2utf8(x1)
testStrings <-
    c("different     kinds\nof\tspace",
      "control\a characters \ftoo",
      "{braces} and \\\\backslash",
      '#various$ \%other^ &characters_ ~escaped"/coded',
      x1,
      x2,
      x3)
latexStrings <- latexify(testStrings, doublebackslash = FALSE)
## All should be "unknown"
Encoding(latexStrings)
cat(latexStrings, sep="\n")
## Input encoding does not matter
identical(latexStrings[5], latexStrings[7])
}
\keyword{ utilities }

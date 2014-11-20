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
latexify(x, doublebackslash = TRUE,
         quotes = c("straight", "curved"),
         packages = c("fontenc", "textcomp"))
}
\arguments{
  \item{x}{ a \code{character} vector }
  \item{doublebackslash}{ a \code{logical} flag.  If \code{TRUE},
    backslashes in the output are doubled.  It seems that Sweave needs
    \code{TRUE} and knitr \code{FALSE}. }
  \item{quotes}{ a \code{character} string specifying how single and
    double quotes (ASCII codes 39 and 34) in the input are
    treated.  The default is to use straight quotes.  The other option
    is to use curved right side (closing) quotes.  Straight double
    quotes are not available in the default OT1 font encoding of LaTeX.
    Straight single quotes require the \dQuote{textcomp} package.  See
    \code{\var{packages}}. }
  \item{packages}{ a \code{character} vector specifying the LaTeX
    packages allowed.  The use of some symbols in LaTeX requires
    commands or characters made available by an add-on package.  If a
    package required for a given character is not marked as available, a
    fallback solution is silently used.  For example, curved quotes may
    be used instead of straight quotes.  Including \code{"fontenc"} in
    the vector means that some other encoding than OT1 is going to be
    used.  }
}
\details{

  The function is intended for use with unformatted inline text.
  Newlines, tabs and other whitespace characters (\code{"[:space:]"} in
  \link{regex}) are converted to spaces.  Control characters
  (\code{"[:cntrl:]"}) that are not whitespace are removed.  Other
  special characters are \sQuote{\{}, \sQuote{\}}, \sQuote{\\},
  \sQuote{#}, \sQuote{$}, \sQuote{\%}, \sQuote{^}, \sQuote{&},
  \sQuote{_}, \sQuote{~},
  \sQuote{"}, and \sQuote{/}.
  They are converted to the corresponding LaTeX commands.

  Before applying the substitutions described above, input elements with
  \code{Encoding} set to \code{"bytes"} are printed and the result is
  captured using the current text encoding.  The result of this
  intermediate stage is ASCII text where some characters are shown as
  their byte codes using a hexadecimal pair prefixed with \code{"\\x"}.
  This set includes tabs, newlines and control characters.  The
  substitutions are then applied to the intermediate result.

  The quoting functions \code{\link{sQuote}} and \code{\link{dQuote}} may
  use non-ASCII quote characters, depending on the locale.  Also these
  quotes are converted to LaTeX commands.  This means that the quoting
  functions are safe to use with any LaTeX input encoding.

  The result is converted to UTF-8 encoding, Normalization Form C (NFC).
  Note that this function will not add any non-ASCII characters that
  were not already present in the input.

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
  Pakin, S. (2009) The Comprehensive LaTeX Symbol
  List. \url{http://www.ctan.org/tex-archive/info/symbols/comprehensive}

  The Unicode Consortium. The Unicode Standard. \url{http://www.unicode.org/versions/latest/}
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
testStrings <-
    c("different     kinds\nof\tspace",
      "control\a characters \ftoo",
      "{braces} and \\\\backslash",
      '#various$ \%other^ &characters_ ~escaped"/coded',
      x1,
      x2)
latexStrings <- latexify(testStrings, doublebackslash = FALSE)
## 5th element should be "UTF-8", the rest "unknown"
Encoding(latexStrings)
cat(latexStrings, sep="\n")
}
\keyword{ utilities }

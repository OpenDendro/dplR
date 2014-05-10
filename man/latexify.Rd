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
latexify(x)
}
\arguments{
  \item{x}{ a \code{character} vector }
}
\details{

  The function is intended for use with unformatted inline text.
  Newlines, tabs and other whitespace characters (\code{"[:space:]"} in
  \link{regex}) are converted to spaces.  Control character
  (\code{"[:cntrl:]"}) that are not whitespace are removed.  Other
  special characters are {, }, \, #, $, \%, ^, &, _, ~, \" and /.  They
  are converted to the corresponding LaTeX commands.

  Before applying the substitutions described above, input elements with
  \code{Encoding} set to \code{"bytes"} are printed and the result is
  captured using the current text encoding.  The result of this
  intermediate stage is ASCII text where some characters are shown as
  their byte codes using a hexadecimal pair prefixed with \code{"\\x"}.
  This set includes tabs, newlines and control characters.  The
  substitutions are then applied to the intermediate result.

  Input elements with \code{"unknown"} encoding are assumed to be in the
  current encoding.  These and \code{"latin1"} encoded elements are
  converted to UTF-8.

  Suggested package loading commands in the document preamble
  are:\preformatted{\usepackage[T1]{fontenc}    \% required for "
\usepackage[utf8]{inputenx} \% UTF-8 input encoding
\input{ix-utf8enc.dfu}      \% more supported characters} 
  
}
\value{
  A \code{character} vector
}
\references{
  Pakin, S. (2009) The Comprehensive LaTeX Symbol
  List. \url{http://www.ctan.org/tex-archive/info/symbols/comprehensive}
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
latexStrings <- latexify(testStrings)
## 5th element should be "UTF-8", the rest "unknown"
Encoding(latexStrings)
cat(latexStrings, sep="\n")
}
\keyword{ utilities }

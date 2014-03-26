\name{read.ids}
\alias{read.ids}
\alias{autoread.ids}
\title{ Read Site-Tree-Core \acronym{ID}s }
\description{
  These functions try to read site, tree, and core \acronym{ID}s from a
  rwl \code{data.frame}.
}
\usage{
read.ids(rwl, stc = c(3, 2, 3), ignore.site.case = FALSE,
         ignore.case = FALSE, fix.typos = FALSE, typo.ratio = 5,
         use.cor = TRUE)

autoread.ids(rwl, ignore.site.case = TRUE, ignore.case = "auto",
             fix.typos = TRUE, typo.ratio = 5, use.cor = TRUE)
}
\arguments{

  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}} or
    \code{\link{ca533}} }

  \item{stc}{ a vector of three integral values or character string
    "auto".  The numbers indicate the number of characters to split the
    site code (\code{\var{stc}[1]}), the tree \acronym{ID}s
    (\code{\var{stc}[2]}), and the core \acronym{ID}s
    (\code{\var{stc}[3]}).  Defaults to \code{c(3, 2, 3)}.  If "auto",
    tries to automatically determine the split locations.  See Details
    for further information. }

  \item{use.cor}{ a \code{logical} flag.  If \code{TRUE} and
    \code{\var{stc} is "auto"}, correlation clustering may be used for
    determining the length of the tree and core parts.  See Details. }

  The following parameters affect the handling of suspected typing
  errors.  Some have different default values in \code{read.ids} and
  \code{autoread.ids}.

  \item{ignore.site.case}{ a \code{logical} flag.  If \code{TRUE}, the
    function does not distinguish between upper case and lower case
    letters in the site part of the series names. }

  \item{ignore.case}{ a \code{logical} flag or "auto".  If \code{TRUE},
    the function does not distinguish between upper case and lower case
    letters in the tree / core part of the series names.  The default in
    \code{read.ids} is \code{FALSE}, i.e. the difference matters.  The
    default in \code{read.ids} is \code{"auto"}, which means that the
    function tries to be smart with respect to case sensitivity.  In
    \code{"auto"} mode, the function generally ignores case differences,
    unless doing so would result in additional duplicate combinations of
    tree and core \acronym{ID}s.  Also, when in \code{"auto"} mode and
    \code{\var{stc}} is \code{"auto"}, case sensitivity is used in
    highly heuristic ways when deciding the boundary between the site
    part and the tree part in uncertain cases. }

  \item{fix.typos}{ a \code{logical} flag.  If \code{TRUE}, the function
    will try to detect and fix typing errors. }

  \item{typo.ratio}{ a \code{numeric} value larger than \code{1},
    affecting the eagerness of the function to fix typing errors.  The
    default is 5.  See Details. }
}
\details{

  Because dendrochronologists often take more than one core per tree, it
  is occasionally useful to calculate within vs. between tree variance.
  The International Tree Ring Data Bank (\acronym{ITRDB}) allows the
  first eight characters in an rwl file for series \acronym{ID}s but
  these are often shorter.  Typically the creators of rwl files use a
  logical labeling method that can allow the user to determine the tree
  and core \acronym{ID} from the label.

  Argument \code{\var{stc}} tells how each series separate into site,
  tree, and core \acronym{ID}s.  For instance a series code might be
  \code{"ABC011"} indicating site \code{"ABC"}, tree 1, core 1.  If this
  format is consistent then the \code{\var{stc}} mask would be
  \code{c(3, 2, 3)} allowing up to three characters for the core
  \acronym{ID} (i.e., pad to the right).  If it is not possible to
  define the scheme (and often it is not possible to machine read
  \acronym{ID}s), then the output \code{data.frame} can be built
  manually.  See Value for format.

  The function \code{autoread.ids} is a wrapper to \code{read.ids} with
  \code{stc="auto"}, i.e. automatic detection of the site / tree / core
  scheme, and different default values of some parameters.  In automatic
  mode, the names in the same \code{\var{rwl}} can even follow different
  site / tree / core schemes.  As there are numerous possible encoding
  schemes for naming measurement series, the function cannot always
  produce the correct result.

  With \code{stc="auto"}, the site part can be one of the following.
  \itemize{

    \item In names mostly consisting of numbers, the longest common
    prefix is the site part

    \item Alphanumeric site part ending with alphabet, when followed by
    numbers and alphabets

    \item Alphabetic site part (quite complicated actual
    definition). Setting \code{\var{ignore.case}} to \code{"auto"}
    allows the function to try to guess when a case change in the middle
    of a sequence of alphabets signifies a boundary between the site
    part and the tree part.

    \item The characters before the first sequence of space /
    punctuation characters in a name that contains at least two such
    sequences } These descriptions are somewhat general, and the details
    can be found in regular expressions inside the function.  If a name
    does not match any of the descriptions, it is matched against a
    previously found site part, starting from the longest.

  The following \acronym{ID} schemes are detected and supported in the
  tree / core part.  The detection is done per site. \itemize{

    \item Numbers in tree part, core part starts with something else

    \item Alphabets in tree part, core part starts with something else

    \item Alphabets, either tree part all lower case and core part all
    upper case or vice versa.  For this to work,
    \code{\var{ignore.case}} must be set to \code{"auto"} or
    \code{FALSE}.

    \item All digits.  In this case, the number of characters belonging
    to the tree and core parts is detected with one of the following
    methods. \itemize{

      \item If numeric tree parts were found before, it is assumed that
      the core part is missing (one core per tree).

      \item It the series are numbered continuously, one core per tree
      is assumed.
      
      \item Otherwise, try to find a core part as the suffix so that the
      cores are numbered continuously.

    } If none of the above fits, the tree / core split of the all-digit
    names will be decided with the methods described further down the
    list, or finally with the fallback mechanism.

    \item The combined tree / core part is empty or one character.
    In this case, the core part is assumed to be missing.

    \item Tree and core parts separated by a punctuation or white space
    character

  } If the split of a tree / core part cannot be found with any of the
  methods described above, the prefix of the string is matched against a
  previously found tree part, starting from the longest. The fallback
  mechanism for the still undecided tree / core parts is one of the
  following. The first one is used if \code{\var{use.cor}} is
  \code{TRUE}, number two if it is \code{FALSE}.

  \enumerate{

    \item Pairwise correlation coefficients are computed between all
    remaining series.  Pairs of series with above median correlation
    are flagged as similar, and the other pairs are flagged as
    dissimilar.  Each possible number of characters (minimum 1) is
    considered for the share of the tree \acronym{ID}.  The
    corresponding unique would-be tree \acronym{ID}s determine a set of
    clusterings where one cluster is formed by all the measurement
    series of a single tree.  For each clustering (allocation of
    characters), an agreement score is computed.  The agreement score is
    defined as the sum of the number of similar pairs with matching
    cluster number and the number of dissimilar pairs with non-matching
    cluster number.  The number of characters with the maximum agreement
    is chosen.

    \item If the majority of the names in the site use \var{k}
    characters for the tree part, that number is chosen.  Otherwise, one
    core per tree is assumed.  Parameter \code{\var{typo.ratio}} has a
    double meaning as it also defines what is meant by majority here: at
    least \code{\var{typo.ratio} / (\var{typo.ratio} + 1) *
    \var{n.tot}}, where \var{n.tot} is the number of names in the site.
    
  } In both fallback mechanisms, the number of characters allocated for
  the tree part will be increased until all trees have a non-zero
  \acronym{ID} or there are no more characters.

  Suspected typing errors will be fixed by the function if
  \code{\var{fix.typos}} is \code{TRUE}.  The parameter
  \code{\var{typo.ratio}} affects the eagerness to fix typos, i.e. the
  number of counterexamples required to declare a typo.  The following
  main typo fixing mechanisms are implemented: \describe{

    \item{ Site \acronym{ID}s. }{ If a rare site string resembles an at
      least \code{\var{typo.ratio}} times more frequent alternative, and
      if fixing it would not create any name collisions, make the fix.
      The alternative string must be unique, or if there is more than
      one alternative, it is enough if only one of them is a look-alike
      string.  Any kind of substitution in one character place is
      allowed if the alternative string has the same length as the
      original string.  The alternative string can be one character
      longer or one character shorter than the original string, but only
      if it involves interpreting one digit as the look-alike alphabet
      or vice versa.  There are requirements to how long a site string
      must be in order to be eligible for replacement / typo fixing,
      i.e. cannot be shortened to zero length, cannot change the only
      character of a site string. The parameters
      \code{\var{ignore.case}} and \code{\var{ignore.site.case}} have
      some effect on this typo fixing mechanism. }

    \item{ Tree and core \acronym{ID}s. }{ If all tree / core parts of a
      site have the same length, each character position is inspected
      individually.  If the characters in the \var{i}:th position are
      predominantly digits (alphabets), any alphabets (digits) are
      changed to the corresponding look-alike digit (alphabet) if there
      is one.  The look-alike groups are \{0, O, o\}, \{1, I, i\}, \{5,
      S, s\} and \{6, G\}.  The parameter \code{\var{typo.ratio}}
      determines the decision threshold of interpreting the type of each
      character position as alphabet (digit): the ratio of alphabets
      (digits) to the total number of characters must be at least
      \code{\var{typo.ratio} / (\var{typo.ratio} + 1)}. If a name
      differs from the majority type in more than one character
      position, it is not fixed. Also, no fixes are performed if any of
      them would cause a possible monotonic order of numeric prefixes to
      break. }
    
  }

  The function attempts to convert the tree and core substrings to
  integral values.  When this succeeds, the converted values are copied
  to the output without modification.  When non-integral substrings are
  observed, each unique tree is assigned a unique integral value.  The
  same applies to cores within a tree, but there are some subtleties
  with respect to the handling of duplicates.  Substrings are sorted
  before assigning the \code{numeric} \acronym{ID}s.

  The order of columns in \code{\var{rwl}}, in most cases, does not
  affect the tree and core \acronym{ID}s assigned to each series.
  
}
\value{

  A \code{data.frame} with column one named \code{"tree"} giving an
  \acronym{ID} for each tree and column two named \code{"core"} giving
  an \acronym{ID} for each core.  The original series \acronym{ID}s are
  copied from rwl as rownames.  The order of the rows in the output
  matches the order of the series in \code{\var{rwl}}.  If more than one
  site is detected, an additional third column named \code{"site"} will
  contain a site \acronym{ID}.  All columns have integral valued
  \code{numeric} values.
  
}
\author{ Andy Bunn (original version) and Mikko Korpela (patches,
  \code{\var{stc}="auto"}, \code{\var{fix.typos}}, etc.). }
\seealso{ \code{\link{rwi.stats}}, \code{\link{read.rwl}} }
\examples{data(ca533)
read.ids(ca533, stc = c(3, 2, 3))
autoread.ids(ca533)
}
\keyword{ misc }

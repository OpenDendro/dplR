\name{uuid.gen}
\alias{uuid.gen}
\title{
  \acronym{UUID} Generator
}
\description{
  Initializes and returns a generator of universally unique identifiers.
  Use the returned function repeatedly for creating one or more
  \acronym{UUID}s, one per function call.
}
\usage{
uuid.gen(more.state = "")
}
\arguments{
  \item{more.state}{
    A \code{character} string for altering the state of the generator
  }
}
\details{
  This function returns a function (closure) which generates
  \acronym{UUID}s.  The state of that anonymous function is set when
  \code{uuid.gen} is called.  The state consists of the following:
  \itemize{
    \item System and user information (\code{\link{Sys.info}})
    \item \R version (\code{\link{R.version}})
    \item Platform information (\code{\link{.Platform}})
    \item Working directory
    \item Process \acronym{ID} of the \R session
    \item Time when \code{uuid.gen} was called (precision of seconds or
    finer)
    \item The text in parameter \code{\var{more.state}}
  }

  The Pseudo Random Number Generator of \R (see
  \code{\link{.Random.seed}}) is used in the generation of
  \acronym{UUID}s.  No initialization of the \acronym{PRNG} is done.
  Tampering with the state of the \R \acronym{PRNG} while using a given
  \acronym{UUID} generator causes a risk of non-unique identifiers.
  Particularly, setting the state of the \acronym{PRNG} to the same
  value before two calls to the \acronym{UUID} generator guarantees two
  identical identifiers.  If two \acronym{UUID} generators have a
  different state, it is \emph{not} a problem to have the \acronym{PRNG}
  going through or starting from the same state with both generators.

  The user is responsible for selecting a \acronym{PRNG} with a
  reasonable number of randomness.  Usually, this doesn't require any
  action.  For example, any \acronym{PRNG} algorithm available in \R
  works fine.  However, the uniqueness of \acronym{UUID}s can be
  destroyed by using a bad user-supplied \acronym{PRNG}.

  The \acronym{UUID}s produced by \code{uuid.gen} generators are Version
  4 (random) with 122 random bits and 6 fixed bits.  The \acronym{UUID}
  is presented as a \code{character} string of 32 hexadecimal digits and
  4 hyphens:

  \samp{xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx}

  where \var{x} is any hexadecimal digit and \var{y} is one of
  \code{"8"}, \code{"9"}, \code{"a"}, or \code{"b"}.  Each \var{x} and
  \var{y} in the example is an independent variables (for all practical
  purposes); subscripts are omitted for clarity.  The \acronym{UUID}
  generator gets 32 hex digits from the \acronym{MD5} message digest
  algorithm by feeding it a string consisting of the constant generator
  state and 5 (pseudo) random numbers.  After that, the 6 bits are fixed
  and the hyphens are added to form the final \acronym{UUID}.
}
\value{
  A parameterless function which returns a single \acronym{UUID}
  (\code{character} string)
}
\references{
  Leach, P., Mealling, M. and Salz, R. (July 2005) A Universally Unique
  IDentifier (\acronym{UUID}) \acronym{URN} Namespace, \acronym{RFC}
  4122. [Online].  Available: \url{http://tools.ietf.org/html/rfc4122}
}
\author{
  Mikko Korpela
}
\seealso{
  \code{\link{digest}}, \code{\link{Random}}
}
\examples{
## Normal use
ug <- uuid.gen()
uuids <- character(100)
for(i in 1:100){
  uuids[i] <- ug()
}
length(unique(uuids)) == 100 # TRUE, UUIDs are unique with high probability

## Do NOT do the following unless you want non-unique IDs
rs <- .Random.seed
set.seed(0L)
id1 <- ug()
set.seed(0L)
id2 <- ug()
id1 != id2 # FALSE, The UUIDs are the same
.Random.seed <- rs

## Strange usage pattern, but will probably produce unique IDs
ug1 <- uuid.gen("1")
set.seed(0L)
id1 <- ug1()
ug2 <- uuid.gen("2")
set.seed(0L)
id2 <- ug2()
id1 != id2 # TRUE, The UUIDs are different with high probability
.Random.seed <- rs
}
\keyword{ utilities }

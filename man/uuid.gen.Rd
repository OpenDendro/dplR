\name{uuid.gen}
\alias{uuid.gen}
\title{
  UUID Generator
}
\description{
  Initializes and returns a generator of Universally Unique IDentifiers.
  Use the returned function repeatedly for creating one or more UUIDs,
  one per function call.
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
  This function returns a function (closure) which generates UUIDs.  The
  state of that anonymous function is set when \code{uuid.gen} is
  called.  The state consists of the following:
  \itemize{
    \item System and user information (\code{Sys.info})
    \item \R version (\code{R.version})
    \item Platform information (\code{.Platform})
    \item Working directory
    \item Process ID of the \R session
    \item Time when \code{uuid.gen} was called (precision of seconds or
    finer)
    \item The text in parameter \code{more.state}
  }

  The Pseudo Random Number Generator of \R (see \code{.Random.seed}) is
  used in the generation of UUIDs.  No initialization of the PRNG is
  done.  Tampering with the state of the \R PRNG while using a given
  UUID generator causes a risk of non-unique identifiers.  Particularly,
  setting the state of the PRNG to the same value before two calls to
  the UUID generator guarantees two identical identifiers.  If two UUID
  generators have a different state, it is \emph{not} a problem to have
  the PRNG going through or starting from the same state with both
  generators.

  The user is responsible for selecting a PRNG with a reasonable number
  of randomness.  Usually, this doesn't require any action.  For
  example, any PRNG algorithm available in \R works fine.  However, the
  uniqueness of UUIDs can be destroyed by using a bad user-supplied
  PRNG.

  The UUIDs produced by \code{uuid.gen} generators are Version 4
  (random) with 122 random bits and 6 fixed bits.  The UUID is presented
  as a character string of 32 hexadecimal digits and 4 hyphens:

  xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx

  where x is any hexadecimal digit and y is one of 8, 9, a, or b.  The
  UUID generator gets 32 hex digits from the MD5 message digest
  algorithm by feeding it a string consisting of the generator state and
  5 (pseudo) random numbers.  After that, the 6 bits are fixed and the
  hyphens are added to form the final UUID.
}
\value{
  A parameterless function which returns a single UUID (character string)
}
\references{
  P. Leach, M. Mealling and R. Salz, \dQuote{A Universally Unique
  IDentifier (UUID) URN Namespace}, RFC 4122, July 2005. [Online].
  Available: \url{http://tools.ietf.org/html/rfc4122}
}
\author{
  Mikko Korpela
}
\seealso{
  \code{\link[digest]{digest}}
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

### Written by Mikko Korpela.
###
### Creates a generator of Universally Unique IDentifiers (UUIDs).
### Returns a parameterless function (with some randomish internal state),
### a call to which returns a vector of length one containing
### the character representation of a Version 4 UUID (RFC 4122).
uuid.gen <- function(more.state=""){
  ## We know that the PRNG algorithms of R are reasonably good, but there
  ## is no guarantee about the quality of an arbitrary user supplied PRNG.
  if(RNGkind()[1] == "user-supplied")
    warning("a user-coded (pseudo) random number generator is in use")
  ## Pastes together system and software information and current time.
  op <- options(digits.secs=6)
  prefix <- paste(c(Sys.info(), unlist(R.version),
                    unlist(.Platform), getwd(), Sys.getpid(),
                    format(Sys.time(), "%Y%m%d%H%M%S", usetz=TRUE),
                    more.state),
                  collapse="")
  options(op)
  ## Lookup table from hex to hex. Corresponds to setting
  ## * the most significant bit to 1 and
  ## * the second most significant bit to 0.
  ## Linear search with string keys seems to be faster than
  ## alternatives using a) a hashed environment or
  ## b) the hex converted to integer +1 as an index to the table.
  uuid.17.lookup <-
    c("0" = "8", "1" = "9", "2" = "a", "3" = "b",
      "4" = "8", "5" = "9", "6" = "a", "7" = "b", "8" = "8", "9" = "9",
      "a" = "a", "b" = "b", "c" = "8", "d" = "9", "e" = "a", "f" = "b",
      "A" = "A", "B" = "B", "C" = "8", "D" = "9", "E" = "A", "F" = "B")

  function(){
    ## We extract "enough" pseudo randomness, even preparing for a true
    ## heavy user of UUIDs: 5 numbers correspond to 150-160 varying bits,
    ## depending on the random number generator used (NO SAFETY GUARD
    ## against the stupid use of a bad custom PRNG).
    dgst <- digest(paste(prefix, paste(runif(5), collapse="")),
                   algo = "md5",
                   serialize = FALSE)
    ## The UUID is formatted with hyphen-minus characters.
    ## Some bits are set to fixed values according to UUID Version 4.
    paste(substr(dgst, 1, 8), "-",
          substr(dgst, 9, 12), "-",
          "4", substr(dgst, 14, 16), "-",
          uuid.17.lookup[substr(dgst, 17, 17)], substr(dgst, 18, 20), "-",
          substr(dgst, 21, 32), sep = "")
  }
}

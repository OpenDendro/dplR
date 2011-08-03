test.gini.coef <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    ## Test
    checkEquals(0, gini.coef(rep(42, SAMP.SIZE)),
                msg="Gini coefficient of a set with total equality is 0")
    ## Needs more tests
}

test.glk <- function() {
    ## Setup
    seq.inc <- seq_len(10)
    seq.dec <- seq.int(from = -1, to = -10)
    seq.rand <- sample(x = seq.inc, size = 10, replace = FALSE)
    seq.step <- rep(seq.rand, each = 2)
    seq.step <- seq.step[1:(length(seq.step) - 1)]
    glk.4col <- glk(data.frame(seq.rand, seq.rand, seq.rand, seq.rand))
    ## Test
    checkEquals(4, nrow(glk.4col),
                msg="Number of rows in result is correct")
    checkEquals(4, ncol(glk.4col),
                msg="Number of columns in result is correct")
    checkTrue(all(glk.4col[upper.tri(x = glk.4col, diag = FALSE)] == 1),
              msg="Upper triangle has correct values")
    checkTrue(all(is.na(glk.4col[lower.tri(x = glk.4col, diag = TRUE)])),
              msg="Lower triangle and diagonal have NA values")
    checkEquals(1, glk(data.frame(seq.inc, seq.inc + 1))[1, 2],
                msg="glk() is 1 when comparing two strictly monotonic sequences (both increasing)")
    checkEquals(0, glk(data.frame(seq.inc, seq.dec))[1, 2],
                msg="glk() is 0 when comparing two strictly monotonic sequences (one increasing, the other decreasing)")
    checkEquals(1, glk(data.frame(seq.rand, seq.rand + 1))[1, 2],
                msg="glk() is 1 when two sequences agree about the signs of all the differences and there are no zero differences")
    checkEquals(0, glk(data.frame(seq.rand, -seq.rand))[1, 2],
                msg="glk() is 0 when two sequences disagree about the signs of all the differences and there are no zero differences")
    checkEquals(0, glk(data.frame(seq.step, -seq.step))[1, 2],
                msg="glk() is 0 when two sequences agree about the location of zero differences and disagree about the signs of nonzero differences")
    checkEquals(0.5, glk(data.frame(seq.rand, rep(1, length(seq.rand))))[1, 2],
                msg="glk() is 0.5 when one sequence is constant and the other only has nonzero differences")
    checkEquals(0.5, glk(data.frame(seq.step, seq.step))[1, 2],
                msg="glk() is 0.5 when comparing a sequence where exactly half of the differences are zero with itself")
    checkEquals(0.25, glk(data.frame(seq.step, rep(1, length(seq.step))))[1, 2],
                msg="glk() is 0.25 when comparing a constant sequence and a sequence where exactly half of the differences are zero")
}

test.hanning <- function() {
    ## Setup
    SAMP.SIZE <- 101
    FILTER.LENGTH <- c(7, 51)
    HALF.SIZE <- 50
    x.constant <- rep(42, SAMP.SIZE)
    x.impulse <- c(rep(0, HALF.SIZE), 1, rep(0, HALF.SIZE))
    for(filter.length in FILTER.LENGTH){
        length.str <- paste("(filter length ", filter.length, ")", sep="")
        not.na.length <- SAMP.SIZE - filter.length + 1
        y.constant <- hanning(x.constant, n=filter.length)
        y.impulse <- hanning(x.impulse, n=filter.length)
        not.na.constant <- which(!is.na(y.constant))
        ## Test
        checkEquals(not.na.length, length(not.na.constant),
                    msg=paste("Filtering returns a certain number of NA values",
                    length.str, sep=" "))
        checkEquals(rep(42, not.na.length), y.constant[not.na.constant],
                    msg=paste("A constant series is still constant after filtering",
                    length.str, sep=" "))
        checkEquals(1, sum(y.impulse, na.rm=TRUE),
                    msg=paste("Sum of the filter coefficients is 1. Thus, filtering a unit impulse creates a result that sums to 1.",
                    length.str, sep=" "))
        ## Needs more tests (?)
    }
    checkException(hanning(x.constant, n=2), silent=TRUE,
                   msg="Filter length < 3 is an error")
}

test.sens1 <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    ## Test
    checkEquals(0, sens1(rep(42, SAMP.SIZE)),
                msg="Mean sensitivity (1) of a constant series is 0")
    ## Needs more tests
}

test.sens2 <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    ## Test
    checkEquals(0, sens2(rep(42, SAMP.SIZE)),
                msg="Mean sensitivity (2) of a constant series is 0")
    ## Needs more tests
}

test.tbrm <- function() {
    ## Setup
    SAMP.SIZE <- 1000
    half.32.52 <- c(rep(32, SAMP.SIZE / 2), rep(52, SAMP.SIZE / 2))
    outliers.in.42 <- c(rep(42, SAMP.SIZE / 2 + 1),
                        rep(-1e6, SAMP.SIZE / 4), rep(1e6, SAMP.SIZE / 4))
    seq.odd <- seq_len(5)
    seq.even <- seq_len(6)
    ## Test
    checkTrue(is.nan(tbrm(NA)), msg="NA input returns NaN")
    checkTrue(is.nan(tbrm(numeric(0))), msg="empty input returns NaN")
    checkEquals(42, tbrm(rep(42, SAMP.SIZE)),
                msg="(Robust) mean of repeated x is x")
    checkEquals(42, tbrm(half.32.52, C=1),
                msg="When C is roughly at least 1, the (robust) mean of a data set consisting of equally sized subsets of two distinct numbers is the mean of those numbers")
    checkTrue(is.nan(tbrm(half.32.52, C=0.5)),
              msg="When C is 0.5, the (robust) mean of a data set consisting of equally sized subsets of two distinct numbers is NaN (all numbers are outliers)")
    ## In the following two tests, the value of C should not matter
    checkEquals(42, tbrm(outliers.in.42, C=1e300),
                msg="When the median of the absolute deviation from the median of the data set is zero, outliers are discarded when using a large C")
    checkEquals(42, tbrm(outliers.in.42, C=0),
                msg="When the median of the absolute deviation from the median of the data set is zero, outliers are discarded when using a small C")
    ## In the following, we see what happens when the median
    ## at first belongs and then does not belong to the set
    checkEquals(mean(seq.odd), tbrm(seq.odd, C=0),
                msg="Robust mean of an odd length sequence of consecutive integers is the mean of the sequence, even with C==0")
    checkTrue(is.nan(tbrm(seq.even, C=0)),
              msg="Robust mean of an even length sequence of consecutive integers is NaN when using a small C")
}

test.uuid.gen <- function() {
    ## Setup
    SAMP.SIZE <- 100
    ugen <- uuid.gen()
    uuids <- character(SAMP.SIZE)
    for(i in seq_len(SAMP.SIZE))
        uuids[i] <- ugen()
    uuids.split <- strsplit(uuids, split="-", fixed=TRUE)
    unique.nchar <- unique(t(sapply(uuids.split, nchar)))
    unique.chars <-
        unique(strsplit(paste(sapply(uuids.split, paste, collapse=""),
                              collapse=""), split=character(0))[[1]])
    all.4 <- unique(substr(uuids, 15, 15))
    one.of.four <- unique(substr(uuids, 20, 20))
    ## Test
    checkEquals(SAMP.SIZE, length(unique(uuids)), msg="Unique IDs are unique")
    checkTrue(all(nchar(uuids) == 36), msg="IDs have correct length")
    checkTrue(all(sapply(uuids.split, length) == 5),
              msg="IDs have 5 parts separated by dashes")
    checkTrue(nrow(unique.nchar) == 1 &&
              all(as.vector(unique.nchar) == c(8, 4, 4, 4, 12)),
              msg="The parts have lengths 8, 4, 4, 4, and 12")
    checkTrue(all(unique.chars %in% c(as.character(0:9), letters[1:6])),
              msg="In addition to dashes, IDs only contain hexadecimal digits")
    checkEquals("4", all.4,
                msg="IDs have a constant character \"4\" in one position")
    checkTrue(all(one.of.four %in% c("8", "9", "a", "b")),
              msg="IDs have a restricted character (4/16 choices) in one position")
}

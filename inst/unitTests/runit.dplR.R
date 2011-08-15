test.bai.in <- function() {
    ## Test
    base.seq <- pi * seq(from=3, by=2, length.out=19)
    ones <- rep(1, 20)
    checkEquals(c(pi, base.seq),
                bai.in(data.frame(ones))[[1]],
                msg="Testing with constant ring widths, zero (default) d2pith")
    checkEquals(c(base.seq, 41 * pi),
                bai.in(data.frame(x1 = ones),
                       d2pith = data.frame(series="x1", d2pith=1))[[1]],
                msg="Testing with constant ring widths, nonzero d2pith")
}

test.bai.out <- function() {
    ## Test
    base.seq <- pi * seq(from=3, by=2, length.out=19)
    ones <- rep(1, 20)
    checkEquals(c(pi, base.seq),
                bai.out(data.frame(ones))[[1]],
                msg="Testing with constant ring widths, zero (default) diam")
    checkEquals(c(base.seq, 41 * pi),
                bai.out(data.frame(x1 = ones),
                        diam = data.frame(series="x1", diam=42))[[1]],
                msg="Testing with constant ring widths, nonzero diam")
}

test.ccf.series.rwl <- function() {
    ## Setup
    srs1 <- seq(from=1, to=2, length.out=500)
    names(srs1) <- seq_along(srs1)
    dat1 <- data.frame(srs1, srs1 + 0.05, srs1 + 0.1)
    ## perfect correlation at lag 0 (mean of dat1 is srs1 + constant)
    res1.1 <- ccf.series.rwl(rwl = dat1, series = srs1,
                             seg.length = 100, bin.floor = 100,
                             prewhiten = FALSE, biweight = TRUE,
                             make.plot = FALSE, floor.plus1 = FALSE)
    res1.2 <- ccf.series.rwl(rwl = dat1, series = srs1,
                             seg.length = 100, bin.floor = 100,
                             prewhiten = FALSE, biweight = FALSE,
                             make.plot = FALSE, floor.plus1 = TRUE)
    res1.3 <- ccf.series.rwl(rwl = dat1, series = srs1,
                             seg.length = 100, bin.floor = 100,
                             prewhiten = TRUE, biweight = FALSE,
                             make.plot = FALSE, floor.plus1 = TRUE)
    bins1.1 <- res1.1$bins
    bins1.2 <- res1.2$bins
    bins1.3 <- res1.3$bins
    nrow1.3 <- nrow(bins1.3)

    srs2 <- sin(pi / 4 * seq_len(500)) + 1.5 # period is 8
    names(srs2) <- seq_along(srs2)
    dat2 <- data.frame(srs2)
    ## perfect correlation at lag 0 (the single column dat2 is a copy of srs2)
    res2 <- ccf.series.rwl(rwl = dat2, series = srs2,
                           seg.length = 250, bin.floor = 100,
                           prewhiten = FALSE, lag.max = 7,
                           make.plot = FALSE, floor.plus1 = TRUE)
    ccf2 <- res2$ccf
    bins2 <- res2$bins
    rnames2 <- rownames(ccf2)

    ## Test
    checkEquals(7, nrow(bins1.1),
                msg="Correct number of bins is 7")
    checkEquals(9, nrow(bins1.2),
                msg="Correct number of bins is 9")
    checkEqualsNumeric(100, bins1.1[1, 1],
                       msg="First bin starts at 100")
    checkEqualsNumeric(1, bins1.2[1, 1],
                       msg="First bin starts at 1")
    checkEqualsNumeric(499, bins1.1[7, 2],
                       msg="Last bin ends at 499")
    checkEqualsNumeric(500, bins1.2[9, 2],
                       msg="Last bin ends at 500 (test 2)")
    checkEqualsNumeric(500, bins1.3[nrow1.3, 2],
                       msg="Last bin ends at 500 (test 3)")
    checkEqualsNumeric(rep(1, 7), res1.1$ccf["lag.0", ],
                       msg="Correlation at lag 0 is 1 (test 1)")
    checkEqualsNumeric(rep(1, 9), res1.2$ccf["lag.0", ],
                       msg="Correlation at lag 0 is 1 (test 2)")
    checkEqualsNumeric(rep(1, nrow1.3), res1.3$ccf["lag.0", ],
                       msg="Correlation at lag 0 is 1 (test 3)")

    checkEquals(3, nrow(bins2),
                msg="Correct number of bins is 3")
    checkEquals(15, length(rnames2),
                msg="Correct number of lags is 15 (1 + 7 + 7)")
    checkEqualsNumeric(c(1, 126, 251), bins2[, 1],
                       msg="Bins start at 1, 126, 251")
    checkEqualsNumeric(c(250, 375, 500), bins2[, 2],
                       msg="Bins end at 250, 375, 500")
    checkTrue(all(rnames2[apply(abs(ccf2), 2, which.min)] %in%
                  c("lag.-6", "lag.-2", "lag.2", "lag.6")),
              msg="Smallest absolute correlation is at a phase difference of 1/4 or 3/4 cycles")
    checkTrue(all(rnames2[apply(ccf2, 2, which.min)] %in%
                  c("lag.-4", "lag.4")),
              msg="Largest negative correlation is at a phase difference of 1/2 cycles")
    checkTrue(all(rnames2[apply(ccf2, 2, which.max)] == "lag.0"),
              msg="Largest positive correlation is at lag 0")
}

test.combine.rwl <- function() {
    ## Setup
    v.1 <- 1 + runif(300)
    range.1 <- 51:400
    rnames.1 <- as.character(range.1)
    range.2 <- range.1 + 150
    rnames.2 <- as.character(range.2)
    range.3 <- range.1 + 350
    rnames.3 <- as.character(range.3)
    range.4 <- range.1 + 450
    rnames.4 <- as.character(range.4)
    df.1 <- data.frame(col1 = c(v.1, rep(NA, 50)),
                       col2 = c(rep(NA, 25), v.1, rep(NA, 25)),
                       col3 = c(rep(NA, 50), v.1),
                       row.names = rnames.1)
    df.2 <- df.1
    rownames(df.2) <- rnames.2
    df.3 <- df.1
    rownames(df.3) <- rnames.3
    df.4 <- df.1
    rownames(df.4) <- rnames.4
    res.3 <- combine.rwl(list(df.1))
    res.4 <- combine.rwl(list(df.1, df.2, df.3, df.4))
    res.5 <- combine.rwl(df.1, df.1)
    res.6 <- combine.rwl(df.1, df.2)
    res.7 <- combine.rwl(df.1, df.3)
    res.8 <- combine.rwl(df.1, df.4)
    ## Test
    ## 1. x is an empty list (error)
    checkException(combine.rwl(list()),
                   msg="Nothing to combine in an empty list")
    ## 2. x is a data.frame, y is not given (error)
    checkException(combine.rwl(df.1),
                   msg="Nothing to combine in a single data.frame")
    ## 3. x is a list with one data.frame
    checkEquals(df.1, res.3,
                msg="Result is correct (test 3)")
    ## 4. x is a list with four data.frames
    checkEquals(12, ncol(res.4), msg="Result has 12 columns (test 4)")
    checkEquals(df.1, res.4[1:350, 1:3],
                msg="1st part of result is correct (test 4)")
    checkEquals(df.2, res.4[150+(1:350), 4:6],
                msg="2nd part of result is correct (test 4)")
    checkEquals(df.3, res.4[350+(1:350), 7:9],
                msg="3rd part of result is correct (test 4)")
    checkEquals(df.4, res.4[450+(1:350), 10:12],
                msg="4th part of result is correct (test 4)")
    ## x and y are data.frames that...
    ## 5. ...are identical
    checkEquals(6, ncol(res.5), msg="Result has 6 columns (test 5)")
    checkEquals(df.1, res.5[1:3],
                msg="1st part of result is correct (test 5)")
    checkEquals(df.1, res.5[4:6],
                msg="2nd part of result is correct (test 5)")
    ## 6. ...have partially overlapping years
    checkEquals(6, ncol(res.6), msg="Result has 6 columns (test 6)")
    checkEquals(500, nrow(res.6), msg="Result has 500 rows (test 6)")
    checkEquals(df.1, res.6[1:350, 1:3],
                msg="1st part of result is correct (test 6)")
    checkEquals(df.2, res.6[150+(1:350), 4:6],
                msg="2nd part of result is correct (test 6)")
    ## 7. ...have separate sets of years so that the result is continuous
    ## (y starts where x ends)
    checkEquals(6, ncol(res.7), msg="Result has 6 columns (test 7)")
    checkEquals(700, nrow(res.7), msg="Result has 700 rows (test 7)")
    checkEquals(df.1, res.7[1:350, 1:3],
                msg="1st part of result is correct (test 7)")
    checkEquals(df.3, res.7[350+(1:350), 4:6],
                msg="2nd part of result is correct (test 7)")
    ## 8. ...have separate sets of years so that the result is discontinuous
    checkEquals(6, ncol(res.8), msg="Result has 6 columns (test 8)")
    checkEquals(800, nrow(res.8), msg="Result has 800 rows (test 8)")
    checkEquals(df.1, res.8[1:350, 1:3],
                msg="1st part of result is correct (test 8)")
    checkEquals(df.4, res.8[450+(1:350), 4:6],
                msg="2nd part of result is correct (test 8)")
}

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
    seq.step <- seq.step[-length(seq.step)]
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
    checkTrue(all(unique.chars %in% c(as.character(0:9), letters[seq_len(6)])),
              msg="In addition to dashes, IDs only contain hexadecimal digits")
    checkEquals("4", all.4,
                msg="IDs have a constant character \"4\" in one position")
    checkTrue(all(one.of.four %in% c("8", "9", "a", "b")),
              msg="IDs have a restricted character (4/16 choices) in one position")
}

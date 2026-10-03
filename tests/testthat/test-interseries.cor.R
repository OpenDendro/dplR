context("interseries.cor")

test.interseries.cor <- function() {
    data(gp.rwl, package = "dplR", envir = environment())

    ## The leave-one-out masters are computed only where each series has a
    ## value (Oct 2026). Rebuild them the old way, over every year, and
    ## check the correlations are unchanged.
    old.way <- function(rwl, biweight) {
        tmp <- dplR:::normalize.xdate(rwl = as.matrix(rwl), n = NULL,
                                      prewhiten = TRUE, biweight = biweight,
                                      leave.one.out = TRUE)
        s <- tmp$series
        good <- colSums(!is.na(s)) > 3
        vapply(seq_len(ncol(s)), function(i) {
            g <- good
            g[i] <- FALSE
            m <- if (biweight) {
                apply(s[, g, drop = FALSE], 1, dplR:::tbrm, C = 9)
            } else {
                rowMeans(s[, g, drop = FALSE], na.rm = TRUE)
            }
            unname(cor.test(s[, i], m, method = "spearman",
                            alternative = "greater")$estimate)
        }, numeric(1))
    }

    test_that("leave-one-out masters give the same correlations as before", {
        for (bw in c(TRUE, FALSE)) {
            res <- suppressWarnings(interseries.cor(gp.rwl, biweight = bw))
            expect_equal(res$res.cor,
                         suppressWarnings(old.way(gp.rwl, bw)))
        }
    })

    test_that("each master is NA where its series has no value", {
        tmp <- dplR:::normalize.xdate(rwl = as.matrix(gp.rwl), n = NULL,
                                      prewhiten = TRUE, biweight = TRUE,
                                      leave.one.out = TRUE)
        ## (it can also be NA where the series has a value but no other
        ## series does, as before)
        expect_true(all(is.na(tmp$master[is.na(tmp$series)])))
    })
}
test.interseries.cor()

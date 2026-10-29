test_that("bakker returns data frames with the years and series of rwl", {
    data(zof.rwl, package = "dplR", envir = environment())
    data(zof.anc, package = "dplR", envir = environment())
    out <- bakker(zof.rwl, zof.anc)
    expect_s3_class(out$baiBakker_raw, "bai")
    expect_s3_class(out$baiBakker_raw, "data.frame")
    expect_true(is.data.frame(out$DBHhist_raw))
    for (x in out) {
        expect_identical(dim(x), dim(zof.rwl))
        expect_identical(rownames(x), rownames(zof.rwl))
        expect_identical(colnames(x), colnames(zof.rwl))
    }
    expect_identical(attr(out$baiBakker_raw, "dplR.bai")$fun, "bakker")
    ## chron() takes the areas as areas, without calling them widths
    expect_silent(chron(out$baiBakker_raw))
})

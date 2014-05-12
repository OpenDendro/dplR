### Script for compiling math-dplR.pdf
###
### Run with 'source("build-math-dplR.R")' in R prompt or 'R CMD BATCH
### build-math-dplR.R' on the command line, where the initial R is the
### command to launch R.
###
### Written by Mikko Korpela
SOURCE_NAME <- "math-dplR"
SOURCE_EXT <- "Rnw"
DUMMY_EXT <- "txt"
CLEAN <- TRUE

builder <- function(sourceName, sourceExt, dummyExt, clean) {
    sourceFile <- paste0(sourceName, ".", sourceExt)
    regExt <- sprintf("\\.%s$", sourceExt)
    texFile <- sub(regExt, ".tex", sourceFile)
    pdfFile <- sub(regExt, ".pdf", sourceFile)

    if (!file.exists(sourceFile)) {
        dummyFile <- paste0(sourceFile, ".", dummyExt)
        if (!file.exists(dummyFile)) {
            stop(sprintf("File does not exist: %s", dummyFile))
        } else {
            message(sprintf("Temporarily copying %s to %s",
                            dummyFile, sourceFile))
            file.copy(dummyFile, sourceFile)
            on.exit(unlink(sourceFile))
        }
    }

    if (!require(knitr)) {
        stop("Please install knitr")
    }
    knit(sourceFile, encoding = "UTF-8", envir=globalenv()) # produces .tex
    if (isTRUE(clean)) {
        on.exit(unlink(texFile), add = TRUE) # remove .tex at exit
    }
    purl(sourceFile) # produces .R
    tools::texi2pdf(texFile, clean=isTRUE(clean)) # produces .pdf
    tools::compactPDF(pdfFile, gs_quality = "ebook")
}
builder(SOURCE_NAME, SOURCE_EXT, DUMMY_EXT, CLEAN)

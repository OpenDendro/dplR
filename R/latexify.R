### Helpers for vignettes.

## Return a string representing the given date(s) (default: current date)
## in the format used by \today in LaTeX.
## Example: latexDate("2013-12-06") returns "December 6, 2013"
latexDate <- function(x = Sys.Date(), ...) {
    ltDate <- as.POSIXlt(x, ...)
    sprintf("%s %d, %d",
            month.name[ltDate[["mon"]] + 1],
            ltDate[["mday"]],
            1900 + ltDate[["year"]])
}

## Usage: \Sexpr{latexify(string_produced_by_R_code)}
##
## Make arbitrary character() vector compatible with LaTeX by escaping
## special characters, then convert to UTF-8 encoding.  Any formatting
## (newlines, tabs, etc.) will be lost.  Note that the set of
## characters actually supported depends on the font, LaTeX engine and
## set of packages used.
##
## It seems that Sweave needs doublebackslash = TRUE
## but knitr needs doublebackslash = FALSE.
latexify <- function(x, doublebackslash = TRUE,
                     quotes = c("straight", "curved"),
                     packages = c("fontenc", "textcomp")) {
    y <- as.character(x)
    ## Kludge for converting from "byte" to the current encoding
    ## in a way which preserves the hex notation.
    encBytes <- Encoding(y) == "bytes"
    if (any(encBytes)) {
        foo <- character(0) # dummy line
        tc <- textConnection("foo", "w", local = TRUE)
        sink(tc)
        on.exit(sink())
        on.exit(close(tc), add = TRUE)
        ## Embedded newlines (if any) in y[encBytes] will not cause
        ## line breaks with cat().
        cat(y[encBytes], sep = "\n")
        y[encBytes] <- foo
    }
    fontenc <- "fontenc" %in% packages
    textcomp <- "textcomp" %in% packages
    straightQuotes <- match.arg(quotes) == "straight"
    ## Remove control characters (not spaces!)
    y <- gsub("(?![[:space:]])[[:cntrl:]]", "", y, perl=TRUE)
    ## Convert any sequence of whitespace to a single space.  This
    ## substitution must be done before control characters because
    ## newline belongs to both groups.
    y <- gsub("[[:space:]]+", " ", y)
    ## Escape LaTeX special characters.
    ## Source: Scott Pakin (2009) The Comprehensive LaTeX Symbol List.
    ## Accessible through "texdoc symbols".
    ## Particularly section 8.6 "ASCII and Latin 1 quick reference".
    ##
    ## The order of the elements in the list matters!
    ## First, { and } are replaced with \{ and \}, respectively.
    ## Then, \ is replaced with \textbackslash{},
    ## but not if followed by { or }.
    ## After that, the order does not matter.
    substitutions <-
        list(c("\\{", "\\\\{"),
             c("\\}", "\\\\}"),
             c("\\\\(?!\\{|\\})", "\\\\textbackslash{}"),
             c("\\#", "\\\\#"),
             c("\\$", "\\\\$"),
             c("%", "\\\\%"),
             c("\\^", "\\\\textasciicircum{}"),
             c("&", "\\\\&"),
             c("_", "\\\\_"),
             c("~", "\\\\textasciitilde{}"),
             if (textcomp && straightQuotes) {
                 c("'", "\\\\textquotesingle{}")
             },
             c('"', if (fontenc && straightQuotes) {
                 "\\\\textquotedbl{}"
             } else {
                 "\\\\textquotedblright{}"
             }),
             c("/", "\\\\slash{}"))
    if (isTRUE(l10n_info()[["MBCS"]])) {
        ## The output of sQuote() and dQuote() may contain
        ## non-ASCII quoting characters.  If the input is ASCII,
        ## it may be a surprise to the user that an UTF-8 input
        ## encoding is then needed in LaTeX.  Converting the
        ## quotes to commands solves this problem.
        substitutions <-
            c(substitutions,
              list(c("\u2018", "\\\\textquoteleft{}"),
                   c("\u2019", "\\\\textquoteright{}"),
                   c("\u201c", "\\\\textquotedblleft{}"),
                   c("\u201d", "\\\\textquotedblright{}")))
    }
    ## Remove empty group after command when followed by a backslash
    Letters <- paste(c(LETTERS, letters), collapse="")
    substitutions <- c(substitutions,
                       list(c(sprintf("(\\\\[%s]+)\\{\\}(?=\\\\)",
                                      Letters), "\\1")))

    for (subst in substitutions[!vapply(substitutions, is.null, logical(1))]) {
        y <- gsub(subst[1], subst[2], y, perl = TRUE)
    }
    if (isTRUE(doublebackslash)) {
        y <- gsub("\\", "\\\\", y, fixed=TRUE)
    }
    ## Convert result to UTF-8 NFC encoding
    stri_trans_nfc(y)
}

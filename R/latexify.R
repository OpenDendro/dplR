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
latexify <- function(x) {
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
    ## Convert any sequence of whitespace to a single space.  This
    ## substitution must be done before control characters because
    ## newline belongs to both groups.
    y <- gsub("[[:space:]]+", " ", y)
    ## Remove control characters
    y <- gsub("[[:cntrl:]]", "", y)
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
             c("\\\\(?!(\\{|\\}))", "\\\\textbackslash{}"),
             c("\\#", "\\\\#"),
             c("\\$", "\\\\$"),
             c("%", "\\\\%"),
             c("\\^", "\\\\^{}"),
             c("&", "\\\\&"),
             c("_", "\\\\_"),
             c("~", "\\\\~{}"),
             c('"', "\\\\textquotedbl{}"),
             c("/", "\\\\slash{}"))
    for (subst in substitutions) {
        y <- gsub(subst[1], subst[2], y, perl = TRUE)
    }
    ## gsub() may have changed encodings. Therefore we check them
    ## again.
    encs <- Encoding(y)
    encLatin <- which(encs == "latin1")
    if (length(encLatin) > 0) {
        y[encLatin] <- iconv(y[encLatin], from = "latin1", to = "UTF-8")
    }
    encUnknown <- which(encs == "unknown")
    if (length(encUnknown) > 0) {
        y[encUnknown] <- iconv(y[encUnknown], to = "UTF-8")
    }
    y
}

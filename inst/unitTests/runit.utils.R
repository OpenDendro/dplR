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

test.latexify <- function(testDocument=FALSE) {
    ## Number of test strings
    ## (including one "extra difficult" case and one empty string)
    SAMP.SIZE <- 50
    stopifnot(SAMP.SIZE >= 2)
    MIN.LENGTH <- 1
    MAX.LENGTH <- 1000
    MAX.NCHAR <- 20 # maximum length of a "word"
    ## All ASCII characters except NUL (0)
    characters <- rawToChar(as.raw(1:127), multiple = TRUE)
    ## LaTeX special characters.  Some of these must be converted to
    ## commands.  Others (single and double quote) are converted to
    ## commands or other characters for improved compatibility with
    ## other packages or to get a particular glyph (upright quote)
    ## instead of the default (curved quote).
    ##
    ## NOTE that the handling (what kind of treatment if any) of some
    ## characters (currently single quote) depends on the (default)
    ## arguments of latexify().
    specialChars <-
        c("{", "}", "\\", "#", "$", "%", "^", "&", "_", "~", "\"", "/", "'")
    specialStr <- paste(specialChars, collapse="")
    ## latexify() is designed to convert any sequence of space
    ## characters to a single regular space
    spaceChars <- c("\t", "\n", "\v", "\f", "\r", " ")
    spaceStr <- paste(spaceChars, collapse="")
    ## latexify() is designed to drop control characters excluding spaces
    controlChars <- setdiff(rawToChar(as.raw(c(1:31, 127)), multiple = TRUE),
                            spaceChars)
    controlStr <- paste(controlChars, collapse="")
    ## Decide the length of each test string
    stringLengths <- sample(MIN.LENGTH:MAX.LENGTH, SAMP.SIZE - 2)
    nTotal <- sum(stringLengths)
    ## Create the test strings:
    ## * The last element is a "difficult case".
    ## * The other elements consist of a random sample of characters.
    strStop <- cumsum(stringLengths)
    strStart <- strStop - (stringLengths - 1)
    nSpaces <- round(0.2 * nTotal) # In addition to spaces in 'characters'
    testStrings <-
        c(substring(paste(sample(c(rep(characters,
                                       length.out = nTotal - nSpaces),
                                   rep(" ", nSpaces))),
                          collapse=""), strStart, strStop),
          paste(c(specialChars, " ",
                  rev(specialChars), " ",
                  unlist(lapply(lapply(specialChars,rep,3), c, " ")),
                  paste(specialChars, " \t")),
                collapse=""),
          "")

    ## Run latexify() on testStrings
    ltxDouble <- latexify(testStrings, doublebackslash=TRUE)
    ltxSingle <- latexify(testStrings, doublebackslash=FALSE)

    ## Tests
    checkEquals(ltxDouble,
                gsub("\\", "\\\\", ltxSingle, fixed=TRUE, useBytes=TRUE),
                msg="doublebackslash argument works as expected")
    checkTrue(!any(grepl(sprintf("[%s]", controlStr),
                         ltxSingle, useBytes=TRUE)),
              msg="No control characters")
    checkTrue(!any(grepl(sprintf("[%s]{2,}", spaceStr),
                         ltxSingle, useBytes=TRUE)),
              msg="Sequence of space characters collapses into one space")
    checkTrue(!any(grepl("\\\\", ltxSingle, fixed=TRUE)),
              msg="No line breaks (double backslash)")
    Letters <- paste(c(LETTERS, letters), collapse="")
    textCommand <- sprintf("\\\\[%s]+", Letters)
    commandAndGroup <- paste(textCommand, "(\\{\\}|(?=\\\\))", sep="")
    commandsAt <- gregexpr(commandAndGroup, ltxSingle, perl=TRUE)
    checkEquals(lapply(gregexpr(textCommand, ltxSingle), as.vector),
                lapply(commandsAt, as.vector),
                msg="Command name is followed by empty group or backslash")
    escape <- sprintf("\\\\[^%s](\\{\\})?", Letters)
    escapesAt <- gregexpr(escape, ltxSingle)

    ## specialMap: record of special character -> command mapping
    specialMap <- vector(mode="list", length = SAMP.SIZE)
    ## Test that each test string was converted properly and prepare
    ## specialMap
    multiSpaceClass <- sprintf("[%s]+", spaceStr)
    controlClass <- sprintf("[%s]", controlStr)
    specialClass <- sprintf("[%s]", specialStr)
    for (i in seq_len(SAMP.SIZE)) {
        nChars <- nchar(ltxSingle[i])
        ## ltxChars: split ltxSingle[i] into strings representing one
        ## character each
        if (nChars > 0) {
            coms <- commandsAt[[i]]
            escs <- escapesAt[[i]]
            comLengths <- attr(coms, "match.length")
            escLengths <- attr(escs, "match.length")
            if (length(coms) == 1 && coms == -1) {
                coms <- numeric(0)
                comLengths <- numeric(0)
            }
            if (length(escs) == 1 && escs == -1) {
                escs <- numeric(0)
                escLengths <- numeric(0)
            }
            comsAndEscs <- c(coms, escs)
            idx <- order(comsAndEscs)
            comEsc <- comsAndEscs[idx]
            comEscLen <- c(comLengths, escLengths)[idx]
            nComEsc <- length(comEsc)
            charIdx <- numeric(nChars)
            prv <- 0
            prvIdx <- 0
            for (j in seq_along(comEsc)) {
                thisStart <- comEsc[j]
                nSingle <- thisStart - prvIdx - 1
                charIdx[seq(from = prvIdx + 1, length.out = nSingle)] <-
                    seq(from = prv + 1, length.out = nSingle)
                prv <- prv + nSingle + 1
                prvIdx <- thisStart + (comEscLen[j] - 1)
                charIdx[thisStart:prvIdx] <- prv
            }
            nSingle <- nChars - prvIdx
            charIdx[seq(from = prvIdx + 1, length.out = nSingle)] <-
                seq(from = prv + 1, length.out = nSingle)
            ## Each element of ltxChars should represent one character or
            ## a space between words
            strStart <- which(diff(c(0, charIdx)) > 0)
            strStop <- c(strStart[-1] - 1, nChars)
            ## Strip off empty group following a command
            ltxChars <- sub(sprintf("([%s])\\{\\}$", Letters), "\\1",
                            substring(ltxSingle[i], strStart, strStop))
        } else {
            ltxChars <- character(0)
        }
        ## stripChars: "Independently" do a part of what latexify()
        ## does, i.e. remove control characters and use single spaces
        ## only
        stripChars <-
            strsplit(gsub(multiSpaceClass,
                          " ",
                          gsub(controlClass,
                               "",
                               testStrings[i])),
                     "")[[1]]
        ## Compare ltxChars and stripChars
        checkEqualsNumeric(length(stripChars), length(ltxChars),
                           msg=sprintf("Number of characters correct (%.0f)",
                           i),
                           tolerance=0)
        singleFlag <- nchar(ltxChars) == 1
        checkTrue(!any(grepl(specialClass,
                             ltxChars[singleFlag], useBytes=TRUE)),
                  msg=sprintf("No specials left unescaped (%.0f)", i))
        checkEquals(ltxChars[singleFlag], stripChars[singleFlag],
                    msg=sprintf("Normal characters preserved (%.0f)", i))
        specialMap[[i]] <- unique(cbind(stripChars[!singleFlag],
                                        ltxChars[!singleFlag]))
    }
    ## specialMap becomes a combination of the unique rows across its
    ## elements
    specialMap <- do.call(rbind, specialMap)
    specialMap <- unique(specialMap)
    ## Check that special characters are mapped to LaTeX commands in a
    ## consistent manner
    checkEqualsNumeric(length(specialChars), nrow(specialMap),
                       msg="Correct number of character mappings",
                       tolerance=0)
    checkTrue(all(specialChars %in% specialMap[, 1]),
              msg="Each special character has a mapping")
    ## A test for encoding conversion
    latin1String <- "clich\xe9 ma\xf1ana"
    Encoding(latin1String) <- "latin1"
    utf8fy <- latexify(latin1String)
    checkEquals("UTF-8", Encoding(utf8fy),
                msg="Declared encoding is UTF-8")
    checkEquals(as.raw(c(0x63, 0x6c, 0x69, 0x63, 0x68, 0xc3, 0xa9, 0x20,
                         0x6d, 0x61, 0xc3, 0xb1, 0x61, 0x6e, 0x61)),
                charToRaw(utf8fy),
                msg="Conversion to UTF-8 NFC succeeded")
    ## A test for other than default quoting options
    quoteString <- "\"It's five o'clock\", he said."
    res1 <- latexify(quoteString, doublebackslash=FALSE)
    res2 <- latexify(quoteString, quotes="curved", doublebackslash=FALSE)
    res3 <- latexify(quoteString, packages=character(0), doublebackslash=FALSE)
    res4 <- latexify(quoteString, packages="fontenc", doublebackslash=FALSE)
    res5 <- latexify(quoteString, packages="textcomp", doublebackslash=FALSE)
    exp2 <- gsub("\"", "\\\\textquotedblright{}", quoteString)
    exp4 <- gsub("\"", "\\\\textquotedbl{}", quoteString)
    exp5 <- gsub("'", "\\\\textquotesingle{}", exp2)
    exp1 <- gsub("'", "\\\\textquotesingle{}", exp4)
    checkEquals(exp1, res1, msg="Default straight quotes")
    checkEquals(exp2, res2, msg="Curved quotes")
    checkEquals(res2, res3, msg="Fallback to curved quotes")
    checkEquals(exp4, res4, msg="Fallback to curved single quotes")
    retVal <- checkEquals(exp5, res5, msg="Fallback to curved double quotes")
    ## Check that non-ASCII quotes used by dQuote() and sQuote() are
    ## converted to LaTeX commands
    if (isTRUE(l10n_info()[["MBCS"]])) {
        nestQuotes <- paste0("You said, \u201cHe said, ",
                             "\u2018Have a nice day.\u2019\u201d")
        nq <- latexify(nestQuotes, doublebackslash=FALSE)
        retVal <-
            checkEquals(gsub("\\{\\}(?=\\\\)", "",
                             gsub("\u2018", "\\\\textquoteleft{}",
                                  gsub("\u2019", "\\\\textquoteright{}",
                                       gsub("\u201c", "\\\\textquotedblleft{}",
                                            gsub("\u201d",
                                                 "\\\\textquotedblright{}",
                                                 nestQuotes)))),
                             perl=TRUE),
                        nq)
    }
    ## When used independently outside the test suite, the function
    ## can create a test document
    if (isTRUE(testDocument) && isTRUE(l10n_info()[["UTF-8"]])) {
        preamble <- c("\\documentclass[a4paper]{article}",
                      "\\usepackage[T1]{fontenc}",
                      "\\usepackage{textcomp}",
                      "\\usepackage{parskip}",
                      "\\usepackage[utf8]{inputenx}",
                      "\\input{ix-utf8enc.dfu}")
        id <- c(testStrings, latin1String, rep(quoteString, 5), nestQuotes)
        extraInfo <- c(rep("", length(testStrings) + 1),
                       paste0(" (", c("default", "curved", "no packages",
                                      "only fontenc", "only textcomp"), ")"),
                       "")

        ## Record how R prints inputDescription
        inputDescription <- character(length(id)) # dummy line
        tc <- textConnection("inputDescription", "w", local = TRUE)
        sink(tc)
        on.exit(sink())
        on.exit(close(tc), add = TRUE)
        for (i in seq_along(id)) {
            print(id[i])
        }
        sink()
        close(tc)
        on.exit()

        allOutput <- c(ltxSingle, utf8fy, res1, res2, res3, res4, res5, nq)
        filename <- tempfile(pattern = "latexify", fileext = ".tex")
        co <- file(filename, open = "wt", encoding = "UTF-8")
        writeLines(preamble, co)
        writeLines("\\begin{document}", co)
        writeLines("\\begin{enumerate}", co)
        writeLines(paste0("% ", inputDescription, extraInfo, "\n",
                          "\\item\\relax ", allOutput,
                          "% ", seq_along(allOutput)),
                   co, sep = "\n\n")
        writeLines("\\end{enumerate}", co)
        writeLines("\\end{document}", co)
        close(co)
        filename
    } else {
        retVal
    }
}

test.latexDate <- function() {
    dates <- Sys.Date() + round(runif(100, min = -1000, max = 1000))
    latexDates <- latexDate(dates)
    checkEqualsNumeric(length(dates), length(latexDates),
                       msg="Length of output equals length of input",
                       tolerance=0)
    splitDates <- strsplit(latexDates, ", ")
    checkEqualsNumeric(rep(2, length(dates)),
                       vapply(splitDates, length, numeric(1)),
                       msg="Year at end, separated by comma and space",
                       tolerance=0)
    monthsDays <- vapply(splitDates, "[[", character(1), 1)
    yearStr <- vapply(splitDates, "[[", character(1), 2)
    Years <- suppressWarnings(as.numeric(yearStr))
    checkTrue(all(is.finite(Years)), msg="Year is a number")
    splitDates2 <- strsplit(monthsDays, " ")
    checkEqualsNumeric(rep(2, length(dates)),
                       vapply(splitDates2, length, numeric(1)),
                       msg="Month and day separated by space",
                       tolerance=0)
    Months <- match(vapply(splitDates2, "[[", character(1), 1), month.name)
    checkTrue(all(is.finite(Months)),
              msg="Month names match to entries in month.name")
    monthStr <- sprintf("%02.0f", Months)
    Days <- suppressWarnings(as.numeric(vapply(splitDates2,
                                               "[[", character(1), 2)))
    checkTrue(all(is.finite(Days)), msg="Day of month is a number")
    dayStr <- sprintf("%02.0f", Days)
    checkEquals(as.character(dates),
                paste(yearStr, monthStr, dayStr, sep="-"),
                msg="latexDate(x) matches x in ISO format")
}

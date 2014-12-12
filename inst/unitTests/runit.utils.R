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

## If 'testDocument' is TRUE, produces a test document to 'con', which may
## be a connection or a filename.
test.latexify <-
    function(testDocument=FALSE,
             con=tempfile(pattern = "latexify", fileext = ".tex"))
{
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
    ## commands.  Others (e.g. single and double quote, *) are
    ## converted to commands or other characters for improved
    ## compatibility with other packages or to get a particular glyph
    ## (upright quote, centered asterisk) instead of the default
    ## (curved quote, asterisk located higher than the center of the
    ## line).  Some characters would work fine in font encodings other
    ## than OT1 (e.g. <, >, |), but are converted to commands anyway.
    ## The dash is included to prevent -- and --- turning into an n
    ## dash and an m dash, respectively.
    ##
    ## NOTE that the handling (what kind of treatment if any) of some
    ## characters (currently quotes) depends on the (default)
    ## arguments of latexify().
    specialChars <-
        c("{", "}", "\\", "#", "$", "%", "^", "&", "_", "~", "\"", "/", "'",
          "<", ">", "|", "`", "-")
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
    textCommand <- sprintf("\\\\[%s]+(\\{([^}]|\\\\})+})?", Letters)
    commandTerminated <-
        paste(textCommand,
              "((?<=})|\\{\\}| +|(?=$|[[:digit:],.?!;:\\\\}+*/-]))", sep="")
    commandsAt <- gregexpr(commandTerminated, ltxSingle, perl=TRUE)
    checkEquals(lapply(gregexpr(textCommand, ltxSingle), as.vector),
                lapply(commandsAt, as.vector),
                msg="Command name is terminated properly")
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
            ## Strip off empty group or spaces following a command
            ltxChars <- sub(sprintf("([%s])(\\{\\}| +)$", Letters), "\\1",
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
    ## A test for handling of different encodings in the input
    latin1String <- "clich\xe9 ma\xf1ana"
    Encoding(latin1String) <- "latin1"
    latinConverted <- latexify(latin1String, doublebackslash=FALSE)
    checkEquals("clich\\'{e} ma\\~{n}ana",
                latinConverted,
                msg="Conversion of latin1 string succeeded")
    checkEquals(latinConverted,
                latexify(enc2utf8(latin1String), doublebackslash=FALSE),
                msg="Encoding of the input does not matter")
    ## A test for other than default quoting options
    quoteString <- "\"It's five o'clock\", he said."
    res1 <- latexify(quoteString, doublebackslash=FALSE)
    res2 <- latexify(quoteString, quotes="curved", doublebackslash=FALSE)
    res3 <- latexify(quoteString, packages=character(0), doublebackslash=FALSE)
    res4 <- latexify(quoteString, packages="fontenc", doublebackslash=FALSE)
    res5 <- latexify(quoteString, packages="textcomp", doublebackslash=FALSE)
    exp2 <- sub("\"", "\\\\textquotedblright ", quoteString)
    exp2 <- sub("\"", "\\\\textquotedblright", exp2)
    exp4 <- sub("\"", "\\\\textquotedbl ", quoteString)
    exp4 <- sub("\"", "\\\\textquotedbl", exp4)
    exp5 <- gsub("'", "\\\\textquotesingle ", exp2)
    exp1 <- gsub("'", "\\\\textquotesingle ", exp4)
    checkEquals(exp1, res1, msg="Default straight quotes")
    checkEquals(exp2, res2, msg="Curved quotes")
    checkEquals(res2, res3, msg="Fallback to curved quotes")
    checkEquals(exp4, res4, msg="Fallback to curved single quotes")
    checkEquals(exp5, res5, msg="Fallback to curved double quotes")
    ## Check that non-ASCII quotes used by dQuote() and sQuote() are
    ## converted to LaTeX commands
    nestQuotes <- paste0("You said, \u201cHe said, ",
                         "\u2018Have a nice day.\u2019\u201d")
    nq <- latexify(nestQuotes, doublebackslash=FALSE)
    checkEquals(gsub("\\{\\}(?=\\\\)", "",
                     gsub("\u2018", "\\\\textquoteleft ",
                          gsub("\u2019", "\\\\textquoteright",
                               gsub("\u201c", "\\\\textquotedblleft ",
                                    gsub("\u201d",
                                         "\\\\textquotedblright",
                                         nestQuotes)))),
                     perl=TRUE),
                nq, msg="dQuote() and sQuote() are safe")
    diaeresisD <- "o\u0308ljysa\u0308ilio\u0308"
    diasD <- latexify(diaeresisD, doublebackslash=FALSE)
    diaeresisC <- "\u00f6ljys\u00e4ili\u00f6"
    diasC <- latexify(diaeresisC, doublebackslash=FALSE)
    checkEquals(diasD, diasC, msg="Unicode NFD and NFC")
    ## Strings containing practically every non-ASCII character that
    ## will be converted by latexify()
    breakWords <- rep(c("space", "vacation", "movie", "line", "break",
                        "rope", "period"), 2)
    allChars <-
        c("\u0132sselmeer is a lake, Berl\u0133n is Dutch for Berlin.",
          paste0("Other digraphs and ligatures: \u01f1, \u01f2, \u01f3, ",
                 "\u01c4, \u01c5, \u01c6, \u01c7, \u01c8, \u01c9, \u01ca, ",
                 "\u01cb, \u01cc, \ufb00, \ufb01, \ufb02, \ufb03, \ufb04, ",
                 "\ufb05, \ufb06"),
          paste0("\u017c\u00f3\u0142ty, p\u0113c, f\u00eate, ",
                 "vis-\u00e0-vis, pi\u00f1ata, Erd\u0151s, ",
                 "\u00c5ngstr\u00f6m, m\u0103r, t\u0159i, \u0203, t\u0331, ",
                 "fa\u00e7ade, \u1e0c, ",
                 "k\u0361p (there should be a ligature tie), t\u0105sa"),
          "Circled: a\u20dd, \u00f1\u20dd",
          "\u00a1Hola! \u00bfQu\u00e9 pasa? \u2e18Verdad\u203d",
          paste0("Money: \u00a3, \u20ac, \u00a2, \u00a5, \u00a4, \u0e3f, ",
                 "\u20a1, \u20ab, \u20b2, \u20a4, \u20a6, \u20b1, \u20a9"),
          paste0("No-Break\u00a0", breakWords, collapse=" "),
          paste0("Do-Break ", breakWords, collapse=" "),
          "visible\u2423space",
          paste0(rep("Zero\u200bWidth\u200bSpace\u200b", 10), collapse=""),
          paste0(rep(paste0("S\u00ado\u00adf\u00adt\u00ad",
                            "H\u00ady\u00adp\u00adh\u00ade\u00adn\u00ad",
                            "E\u00adv\u00ade\u00adr\u00ady\u00ad",
                            "w\u00adh\u00ade\u00adr\u00ade"), 8),
                 collapse="\u00ad"),
          "Legal \u00a7, \u00a9, \u00ae, \u00b6, \u2117, \u2120, \u2122",
          paste0("Letters \u00c6, \u00e6, \u0152, \u0153, \u00d8, \u00f8, ",
                 "\u0141, \u0142, \u1e9e, \u00df, \u017f, \u00d0, \u00f0, ",
                 "\u0110, \u0111, \u014a, \u014b, \u00de, \u00fe"),
          paste0("Quotes: \u00ab | \u00bb | \u201a | \u201e | \u2039 | \u203a",
                 " | \u2018 | \u2019 | \u201c | \u201d"),
          paste0("mho \u2127, ohm \u03a9, micro \u00b5, Celsius \u2103, ",
                 "degree \u00b0"),
          paste0("Division \u00f7, times \u00d7, fractions \u00bc, \u00bd, ",
                 "\u00be, plusminus \u00b1, root \u221a, minus \u2212, ",
                 "asterisk \u2217, not \u00ac, fraction solidus \u2044, ",
                 "superscript \u00b9, \u00b2, \u00b3"),
          paste0("Discount \u2052, estimated \u212e, \u2116 5, ",
                 "per mille \u2030, parts per ten thousand \u2031"),
          "Bullets: \u2022 Closed, \u25e6 open",
          paste0("Spacing diacritics: double acute \u02dd, acute \u00b4, ",
                 "cedilla \u00b8, breve \u02d8, ",
                 "caron \u02c7, diaeresis \u00a8, macron \u00af"),
          "Arrows: left \u2190, up \u2191, right \u2192, down \u2193",
          "Delimiters: \u3008, \u3009, \u301a, \u301b, \u2045, \u2046",
          paste0("Miscellaneous: feminine ordinal \u00aa, ",
                 "masculine ordinal \u00ba, middle dot \u00b7, ",
                 "daggers \u2020, \u2021, ellipsis \u2026, ",
                 "double vertical line \u2016, large circle \u25ef, ",
                 "blank symbol \u2422, broken bar \u00a6, recipe \u211e, ",
                 "reference mark \u203b, low tilde \u02f7, ",
                 "en dash \u2013, em dash \u2014"))
    ac <- latexify(allChars, doublebackslash=FALSE)
    retVal <- checkTrue(all(Encoding(ac) == "unknown"),
                        msg("No non-ASCII characters left"))
    ## When used independently outside the test suite, the function
    ## can create a test document, but only in a UTF-8 locale.
    if (isTRUE(testDocument) && isTRUE(l10n_info()[["UTF-8"]])) {
        preamble <- c("\\documentclass[a4paper]{article}",
                      "\\usepackage{etoolbox}",
                      "\\usepackage{ifluatex}",
                      "\\usepackage{ifxetex}",
                      "\\usepackage{parskip}",
                      "\\usepackage{textcomp}",
                      "\\ifbool{luatex}{",
                      "\\usepackage{fontspec}",
                      "}{",
                      "\\ifbool{xetex}{",
                      "\\usepackage{fontspec}",
                      "}{",
                      "\\usepackage[T1]{fontenc}",
                      "\\usepackage{lmodern}",
                      "}}")
        id <- c(testStrings, latin1String, rep(quoteString, 5), nestQuotes,
                diaeresisD, diaeresisC, allChars)
        extraInfo <- c(rep("", length(testStrings) + length(latin1String)),
                       paste0(" (", c("default", "curved", "no packages",
                                      "only fontenc", "only textcomp"), ")"),
                       rep("", length(nestQuotes)),
                       rep(" (Unicode NFD)", length(diaeresisD)),
                       rep(" (Unicode NFC)", length(diaeresisC)),
                       rep("", length(allChars)))

        ## Record how R prints the elements in 'id'
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

        allOutput <- c(ltxSingle, latinConverted, res1, res2, res3, res4,
                       res5, nq, diasD, diasC, ac)
        if (is.character(con)) {
            co <- file(con, open = "wt", encoding = "UTF-8")
            on.exit(close(co))
        } else {
            co <- con
            if (!isOpen(co)) {
                open(co, open = "wt")
                on.exit(close(co))
            }
        }
        writeLines(preamble, co)
        writeLines("\\begin{document}", co)
        writeLines("\\begin{enumerate}", co)
        writeLines(paste0("% ", inputDescription, extraInfo, "\n",
                          "\\item\\relax ", allOutput,
                          "% ", seq_along(allOutput)),
                   co, sep = "\n\n")
        writeLines("\\end{enumerate}", co)
        writeLines("\\end{document}", co)
        con
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

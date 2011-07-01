### Written by Mikko Korpela.
simpleXML <- function(fname, root="tridas", xml.ns=NULL,
                      line.term="\x0D\x0A", indent.step=2){
### \x0D\x0A is CR+LF, ASCII carriage return and line feed.
### Another possible choice: \x0A, i.e. just the line feed.

    ## Takes _one_ string x, converts it to UTF-8 (if not UTF-8 already),
    ## and removes characters not allowed in XML.
    to.xml.utf8 <- function(x){
        enc <- Encoding(x) # 3 possible values: "latin1", "UTF-8", or "unknown"
        ## If x not already UTF-8, we convert...
        if(enc == "unknown"){
            y <- iconv(x, from="", to="UTF-8")  # from encoding of the locale
        } else if(enc == "latin1"){
            y <- iconv(x, from=enc, to="UTF-8") # from latin1
        } else{
            y <- x
        }
        y.int <- utf8ToInt(y)
        ## Accepted characters are from XML 1.0 spec
        ## http://www.w3.org/TR/2008/REC-xml-20081126/
        good.chars <-
            (y.int == 0x9 |
             y.int == 0xA |
             y.int == 0xD |
             (y.int >= 0x20 & y.int <= 0xD7FF) |
             (y.int >= 0xE000 & y.int <= 0xFFFD) |
             (y.int >= 0x10000 & y.int <= 0x10FFFF))
        paste(unlist(strsplit(y, split="", fixed=TRUE))[good.chars],
              collapse = "")
    }

    close.tag <- function(){
        if(stack.pointer > 0){
            indent <<- indent - indent.step
            cat(rep(" ", indent),
                "</", tag.stack[stack.pointer], ">", line.term,
                sep="", file=f)
            stack.pointer <<- stack.pointer - 1
        }
    }

    ## Escapes characters not allowed in the value of an attribute
    escape.attribute <- function(x){
        gsub("\t", "&\t;",
             gsub("\n", "&\n;",
                  gsub("\r", "&\r;",
                       gsub('"', "&quot;",
                            gsub("<", "&lt;",
                                 gsub("&", "&amp;", x,
                                      fixed = TRUE, useBytes=FALSE),
                                 fixed = TRUE, useBytes=FALSE),
                            fixed = TRUE, useBytes=FALSE),
                       fixed = TRUE, useBytes=FALSE),
                  fixed = TRUE, useBytes=FALSE),
             fixed = TRUE, useBytes=FALSE)
    }

    ## Escapes characters not allowed in content
    escape.content <- function(x){
        gsub("\r", "&\r;",
             gsub(">", "&gt;",
                  gsub("<", "&lt;",
                       gsub("&", "&amp;", x,
                            fixed = TRUE, useBytes=FALSE),
                       fixed = TRUE, useBytes=FALSE),
                  fixed = TRUE, useBytes=FALSE),
             fixed = TRUE, useBytes=FALSE)
    }

    f <- file(description = fname, open = "w", encoding = "UTF-8")
    tag.stack <- character(10)
    tag.stack[1] <- root
    stack.pointer <- 1
    cat('<?xml version="1.0" encoding="UTF-8"?>', line.term,
        '<', root, sep="", file=f)
    if(!is.null(xml.ns))
        cat(' xmlns="', xml.ns, '"', sep="", file=f)
    cat('>', line.term, sep="", file=f)
    indent <- indent.step

    list(# This list of functions (function closures) is returned
         addTag = function(tag, content=NULL, attrs=character(0), close=TRUE){
             cat(rep(" ", indent), "<", tag, sep="", file=f)
             for(attr in names(attrs))
                 cat(" ", attr, '="',
                     escape.attribute(to.xml.utf8(as.character(attrs[attr]))),
                     '"', sep="", file=f)
             if(close){
                 if(length(content) == 0){
                     cat("/>", line.term, sep="", file=f)
                 } else{
                     cat(">",
                         escape.content(to.xml.utf8(paste(content,
                                                          collapse=" "))),
                         "</", tag, ">", line.term,
                         sep="", file=f)
                 }
             } else{
                 cat(">", line.term, sep="", file=f)
                 indent <<- indent + indent.step
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- tag
             }
         },
         addTag.noCheck = function(tag, content=NULL, attrs=character(0),
         close=TRUE){
             cat(rep(" ", indent), "<", tag, sep="", file=f)
             for(attr in names(attrs))
                 cat(" ", attr, '="', attrs[attr], '"', sep="", file=f)
             if(close){
                 if(length(content) == 0){
                     cat("/>", line.term, sep="", file=f)
                 } else{
                     cat(">", content, "</", tag, ">", line.term,
                         sep="", file=f)
                 }
             } else{
                 cat(">", line.term, sep="", file=f)
                 indent <<- indent + indent.step
                 tag.stack[stack.pointer <<- stack.pointer+1] <<- tag
             }
         },
         closeTag = close.tag,
         close = function(){
             while(stack.pointer > 0)
                 close.tag()
             close(f)
         }
         )
}

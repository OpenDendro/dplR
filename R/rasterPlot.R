rasterPlot <- function(expr, res = 150, region=c("plot", "figure"), antialias,
                       bg = "transparent", interpolate = TRUE, draw = TRUE,
                       Cairo = FALSE, ...) {
    draw2 <- isTRUE(as.logical(draw)[1L])
    Cairo2 <- isTRUE(as.logical(Cairo)[1L])
    ## Plotting commands 'expr' will be evaluated in the environment
    ## of the caller of rasterPlot()
    pf <- parent.frame()
    fallback <- FALSE
    if (draw2 &&
        identical(dev.capabilities("rasterImage")[["rasterImage"]], "no")) {
        message("device does not support raster images")
        fallback <- TRUE
    }
    cairoRaster <- FALSE
    fallback <- TRUE
    for (k in 1:2) {
        if (Cairo2) {
            if (requireNamespace("Cairo", quietly = TRUE)) {
                caps <- Cairo::Cairo.capabilities()
                if (isTRUE(as.vector(caps["raster"]))) {
                    fallback <- FALSE
                    cairoRaster <- TRUE
                } else if (isTRUE(as.vector(caps["png"]))) {
                    fallback <- FALSE
                } else {
                    message("png and raster unsupported in this Cairo library")
                }
                if (!fallback) {
                    if (k == 2) {
                        message("using Cairo device")
                    }
                    break
                }
            } else {
                message("Cairo device unavailable")
            }
            Cairo2 <- FALSE
        } else {
            if (sum(capabilities(c("cairo", "png", "aqua")), na.rm=TRUE) > 0) {
                fallback <- FALSE
                if (k == 2) {
                    message("using png device")
                }
                break
            } else {
                message("png device unavailable")
            }
            Cairo2 <- TRUE
        }
    }
    if (fallback && !draw2) {
        return(NULL)
    }
    region2 <- match.arg(region)
    plotRegion <- region2 == "plot"
    ## Start new plot if one does not exist
    parnew <- tryCatch(par(new = TRUE), warning = function(...) NULL)
    op <- NULL
    marzero <- FALSE
    if (is.null(parnew)) {
        if (!plotRegion && !fallback) {
            plot.new()
            op <- par(no.readonly = TRUE)
            par(mar = c(0, 0, 0, 0))
            marzero <- TRUE
        }
        plot.new()
        parnew <- list(new = FALSE)
    } else if (!parnew[[1L]]) {
        par(new = FALSE)
    }
    usr <- par("usr")
    ## Limits of the plot region in user coordinates
    usrLeft <- usr[1]
    usrRight <- usr[2]
    usrBottom <- usr[3]
    usrTop <- usr[4]
    figCoord <- function() {
        usrWidth <- usrRight - usrLeft
        usrHeight <- usrTop - usrBottom
        plt <- par("plt")
        ## Limits of the plot region proportional to the figure region, 0..1
        pltLeft <- plt[1]
        pltRight <- plt[2]
        pltWidth <- pltRight - pltLeft
        pltBottom <- plt[3]
        pltTop <- plt[4]
        pltHeight <- pltTop - pltBottom
        ## Limits of the figure region in user coordinates
        figLeft <- usrLeft - pltLeft / pltWidth * usrWidth
        figRight <- usrRight + (1 - pltRight) / pltWidth * usrWidth
        figBottom <- usrBottom - pltBottom / pltHeight * usrHeight
        figTop <- usrTop + (1 - pltTop) / pltHeight * usrHeight
        return(c(figLeft, figBottom, figRight, figTop))
    }
    if (fallback) {
        message("using fallback: regular plotting")
        on.exit(par(parnew))
        parxpd <- par(xpd = !plotRegion)
        on.exit(par(parxpd), add = TRUE)
        if (length(bg) != 1 || !is.character(bg) || bg != "transparent") {
            if (plotRegion) {
                rect(usrLeft, usrBottom, usrRight, usrTop,
                     col = bg, border = NA)
            } else {
                fc <- figCoord()
                rect(fc[1], fc[2], fc[3], fc[4], col = bg, border = NA)
            }
        }
        par(new = TRUE)
        eval(expr, pf)
        return(invisible(NULL))
    }
    ## Record number of current device so it can be reactivated later
    curDev <- dev.cur()
    ## Record graphical parameters of the device
    if (is.null(op)) {
        op <- par(no.readonly = TRUE)
    }
    pngWidthHeight <- op[[c(figure="fin", plot="pin")[region2]]]
    op <- op[!(names(op) %in%
               c("ask", "bg", "fig", "fin", "mar", "mfcol", "mfg", "mfrow",
                 "new", "oma", "omd", "omi", "pin", "plt",
                 if (plotRegion) "mai"))]
    ## Open a png device (raster image) using a temporary file.  Width
    ## and height are set to match the dimensions of the figure region
    ## in the original device.  Resolution (points per inch) is the
    ## argument 'res'.
    fname <- tempfile(fileext = ".png")
    if (Cairo2) {
        if (cairoRaster) {
            cairoType <- "raster"
            cairoFile <- ""
        } else {
            cairoType <- "png"
            cairoFile <- fname
        }
        Cairo::Cairo(width = pngWidthHeight[1], height = pngWidthHeight[2],
                     units = "in", dpi = res, bg = bg,
                     type = cairoType, file = cairoFile, ...)
    } else if (missing(antialias)) {
        png(fname, width = pngWidthHeight[1], height = pngWidthHeight[2],
            units = "in", res = res, bg = bg, ...)
    } else {
        png(fname, width = pngWidthHeight[1], height = pngWidthHeight[2],
            units = "in", res = res, bg = bg, antialias = antialias, ...)
    }
    ## Record things to do on exit (will be removed from list one-by-one)
    on.exit(dev.off())
    on.exit(dev.set(curDev), add=TRUE)
    if (!cairoRaster) {
        on.exit(unlink(fname), add=TRUE)
    }
    devAskNewPage(FALSE)
    par(mfcol=c(1,1))
    par(omi=rep(0, 4))
    if (plotRegion) {
        par(mai=rep(0, 4))
    }
    ## Initialize and copy graphical parameters from original device
    plot.new()
    par(op)
    eval(expr, pf)
    if (cairoRaster) {
        ## Capture raster data from device before closing
        rasterData <- dev.capture(native = TRUE)
        on.exit(dev.set(curDev))
        dev.off()
        on.exit()
    } else {
        on.exit(dev.set(curDev))
        on.exit(unlink(fname), add=TRUE)
        dev.off()
        on.exit(unlink(fname))
    }
    ## Return to the original plot (device)
    dev.set(curDev)
    if (!cairoRaster) {
        ## Read the png image to memory
        rasterData <- readPNG(fname, native=TRUE)
        on.exit()
        ## Remove the temporary .png file
        unlink(fname)
    }
    if (!draw2) {
        return(rasterData)
    }
    if (plotRegion || marzero) {
        ## Add a raster image to the plot region of the original plot
        rasterImage(rasterData, xleft = usrLeft, ybottom = usrBottom,
                    xright = usrRight, ytop = usrTop,
                    interpolate = interpolate)
    } else {
        ## Set clipping to figure region, restore at exit
        par(xpd = TRUE)
        on.exit(par(xpd = op[["xpd"]]))
        ## Add a raster image to the figure region of the original plot
        fc <- figCoord()
        rasterImage(rasterData, xleft = fc[1], ybottom = fc[2],
                    xright = fc[3], ytop = fc[4],
                    interpolate = interpolate)
    }
    invisible(NULL)
}

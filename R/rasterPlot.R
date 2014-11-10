rasterPlot <- function(expr, res = 150, region=c("plot", "figure"), antialias,
                       bg = "transparent", interpolate = TRUE, ...) {
    if (identical(dev.capabilities("rasterImage")[["rasterImage"]], "no")) {
        stop("device does not support raster images")
    }
    if (sum(capabilities(c("cairo", "png", "aqua")), na.rm=TRUE) == 0) {
        stop("png device unavailable")
    }
    region2 <- match.arg(region)
    plotRegion <- region2 == "plot"
    ## Record number of current device so it can be reactivated later
    curDev <- dev.cur()
    ## Record graphical parameters of the device
    op <- par(no.readonly = TRUE)
    plt <- op[["plt"]]
    usr <- op[["usr"]]
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
    if (missing(antialias)) {
        png(fname, width = pngWidthHeight[1], height = pngWidthHeight[2],
            units = "in", res = res, bg = bg, ...)
    } else {
        png(fname, width = pngWidthHeight[1], height = pngWidthHeight[2],
            units = "in", res = res, bg = bg, antialias = antialias, ...)
    }
    ## Record things to do on exit (will be removed from list one-by-one)
    on.exit(dev.off())
    on.exit(dev.set(curDev), add=TRUE)
    on.exit(unlink(fname), add=TRUE)
    devAskNewPage(FALSE)
    par(mfcol=c(1,1))
    par(omi=rep(0, 4))
    if (plotRegion) {
        par(mai=rep(0, 4))
    }
    ## Dummy plot for initialization
    plot(1, type = "n", xlab = "", ylab = "", axes=FALSE)
    ## Copy graphical parameters from original device to png:
    ## (margins), coordinates of plot region, etc.
    par(op)
    ## Evaluate the plotting commands 'expr' in the environment of the
    ## caller of rasterPlot()
    pf <- parent.frame()
    eval(expr, pf)
    on.exit(dev.set(curDev))
    on.exit(unlink(fname), add=TRUE)
    ## Close the png device
    dev.off()
    on.exit(unlink(fname))
    ## Return to the original plot (device)
    dev.set(curDev)
    ## Read the png image to memory
    pngData <- readPNG(fname, native=TRUE)
    on.exit()
    ## Remove the temporary .png file
    unlink(fname)
    ## Limits of the plot region in user coordinates
    usrLeft <- usr[1]
    usrRight <- usr[2]
    usrBottom <- usr[3]
    usrTop <- usr[4]
    if (plotRegion) {
        ## Add a raster image to the plot region of the original plot
        rasterImage(pngData, xleft = usrLeft, ybottom = usrBottom,
                    xright = usrRight, ytop = usrTop,
                    interpolate = interpolate)
    } else {
        usrWidth <- usrRight - usrLeft
        usrHeight <- usrTop - usrBottom
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
        ## Set clipping to figure region, restore at exit
        par(xpd = TRUE)
        on.exit(par(xpd = op[["xpd"]]))
        ## Add a raster image to the figure region of the original plot
        rasterImage(pngData, xleft = figLeft, ybottom = figBottom,
                    xright = figRight, ytop = figTop,
                    interpolate = interpolate)
    }
    invisible(NULL)
}

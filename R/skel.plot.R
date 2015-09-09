`skel.plot` <-
    function(rw.vec, yr.vec = NULL, sname = "", filt.weight = 9,
             dat.out = FALSE, master=FALSE, plot=TRUE)
{
    if (length(sname) == 0) {
        sname2 <- ""
    } else {
        sname2 <- as.character(sname)[1]
        if (is.na(sname2)) {
            sname2 <- "NA"
        } else if (Encoding(sname2) == "bytes" ||
                   nchar(sname2, type = "width") > 7) {
            stop("'sname' must be a character string whose width is less than 8")
        }
    }

    ## what about NA. Internal NA?
    na.mask <- is.na(rw.vec)
    rw.vec2 <- rw.vec[!na.mask]

    n.val <- length(rw.vec2)
    if(n.val > 840) {
        cat(gettextf("input series has length of %d\n", n.val))
        stop("long series (> 840) must be split into multiple plots")
    }
    stopifnot(filt.weight >= 3)
    if(n.val < filt.weight) {
        cat(gettextf("input series has length of %d", n.val),
            gettextf("'filt.weight' is %f\n", filt.weight), sep=", ")
        stop("'filt.weight' must not be larger than length of input series")
    }

    ## should wrap this into a function called skel.calc that returns the
    ## dates and skel

    ## if no yr then....
    if(is.null(yr.vec))
        yr.vec2 <- 0:(n.val - 1)
    else
        yr.vec2 <- yr.vec[!na.mask]
    ## pad down to the nearest 10 if not already there
    pad0 <- floor(min(yr.vec2) / 10) * 10
    if(pad0 != min(yr.vec2)){
        pad.length <- min(yr.vec2) - pad0
        rw.df <- data.frame(rw = rep(NA, pad.length),
                            yr = pad0:(pad0 + pad.length - 1))
        rw.df <- rbind(rw.df, data.frame(rw = rw.vec2, yr = yr.vec2))
    }
    else {
        pad.length <- 0
        rw.df <- data.frame(rw = rw.vec2, yr = yr.vec2)
    }

    ## detrend and pad
    rwRw <- rw.df[["rw"]]
    rw.dt <- hanning(rwRw, filt.weight)
    skel <- rep(NA, length(rwRw))
    ## calc rel growth
    n.diff <- length(rwRw) - 1
    idx <- 2:n.diff
    temp.diff <- diff(rwRw)
    skel[idx] <- rowMeans(cbind(temp.diff[-n.diff],
                                -temp.diff[-1])) / rw.dt[idx]
    skel[skel > 0] <- NA
    ## rescale from 0 to 10
    na.flag <- is.na(skel)
    if(all(na.flag))
        skel.range <- c(NA, NA)
    else
        skel.range <- range(skel[!na.flag])
    newrange <- c(10, 1)
    mult.scalar <-
        (newrange[2] - newrange[1]) / (skel.range[2] - skel.range[1])
    skel <- newrange[1] + (skel - skel.range[1]) * mult.scalar
    skel[skel < 3] <- NA
    skel <- ceiling(skel)

    ## Variables for plotting
    ## page width
    pw <- 278
    ## page height
    ph <- 178
    ## row height
    rh <- 22
    ## row width
    rw <- 240
    ## spacer for text and dashed cutting lines
    spcr <- 5

    ## break series into sections of 120 years with an index
    yrs.col <- rw / 2 # n years per row
    n <- length(skel)
    n.rows <- ceiling(n / yrs.col)
    m <- seq_len(n.rows)
    skel.df <- data.frame(yr=rw.df[["yr"]], skel)
    if(plot){
        ## master page
        grid.newpage()
        vps <- vector(mode = "list", length = n.rows)
        y <- ph
        for (i in m) {
            y <- y - (rh + spcr)
            vps[[i]] <-
                viewport(x=unit(19, "mm"), y=unit(y, "mm"),
                         width=unit(246, "mm"), height=unit(rh, "mm"),
                         just=c("left", "bottom"), name=LETTERS[i])
        }

        ## seq for 0 to plot width by 2mm
        tmp.1 <- seq(from=0, to=rw, by=2)
        tmp.2 <- seq(from=0, to=rh, by=2)
        ticks <- seq(from=0, to=rw, by=20)
        vSegments <-
            segmentsGrob(x0 = tmp.1, y0 = 0, x1 = tmp.1, y1 = rh,
                         default.units = "mm",
                         gp = gpar(col="green", lineend = "square",
                         linejoin = "round"))
        hSegments <-
            segmentsGrob(x0 = 0, y0 = tmp.2, x1 = rw, y1 = tmp.2,
                         default.units = "mm",
                         gp = gpar(col="green", lineend = "square",
                         linejoin = "round"))
        ## decadal lines
        decades <-
            segmentsGrob(x0 = ticks, y0 = 0, x1 = ticks, y1 = rh,
                         default.units = "mm",
                         gp = gpar(col = "black", lwd = 1.5, lty = "dashed",
                         lineend = "square", linejoin = "round"))
        ## lines on top and bottom of plot
        topLine <-
            linesGrob(x = c(0, rw), y = c(rh, rh),
                      default.units = "mm",
                      gp = gpar(lwd = 2, lineend = "square",
                      linejoin = "round"))
        bottomLine <-
            linesGrob(x = c(0, rw), y = c(0, 0),
                      default.units = "mm",
                      gp = gpar(lwd = 2, lineend = "square",
                      linejoin = "round"))
        rowTree <- grobTree(vSegments, hSegments, decades, topLine, bottomLine)
        if(!master){
            yy1 <- c(0, 6, 6)
            yy2 <- rh - 1
            sjust <- c("right", "bottom")
            yrjust <- c("center", "bottom")
            yry <- rh + 0.5
        }
        else{
            yy1 <- c(rh, 16, 16)
            yy2 <- 1
            sjust <- c("left", "bottom")
            yrjust <- c("center", "top")
            yry <- rh - 22.5
        }
        ## set up page with the right number of rows
        dev.hold()
        on.exit(dev.flush())
        pushViewport(vpTree(viewport(width=unit(pw, "mm"), y=1, just="top",
                                     height=unit(ph, "mm"), name="page",
                                     clip="off"),
                            do.call(vpList, vps)))
        row.last <- 0
        for (i in m) {

            seekViewport(LETTERS[i])
            ## working code goes here - e.g., skelplot!
            grid.draw(rowTree)

            ## plot x axis
            ## get this row's data
            row.first <- row.last + 1
            row.last <- min(row.first + (yrs.col - 1), n)
            skel.sub <- skel.df[row.first:row.last, ]
            skelYr <- skel.sub[["yr"]]
            skel2 <- skel.sub[["skel"]]
            end.yr <- length(skelYr)
            init.lab <- min(skelYr)
            x.labs <- seq(from=init.lab, length.out = length(ticks), by=10)
            grid.text(label = x.labs, x = ticks, y = yry,
                      default.units = "mm", just = yrjust,
                      gp = gpar(fontsize=10))
            ## plot data
            notNA <- which(!is.na(skel2))
            if (length(notNA) > 0) {
                xx <- (notNA - 1) * 2
                if (!master) {
                    y0 <- 0
                    y1 <- 2 * skel2[notNA]
                } else {
                    y0 <- 22
                    y1 <- 22 - 2 * skel2[notNA]
                }
                grid.segments(x0 = xx, x1 = xx, y0 = y0, y1 = y1,
                              default.units = "mm",
                              gp = gpar(col = "black", lwd = 2,
                              lineend = "square", linejoin = "round"))
            }
        }
        ## end arrow
        end.mm <- (end.yr - 1) * 2
        grid.lines(x=unit(c(end.mm, end.mm), "mm"), y=unit(c(rh, 0), "mm"),
                   gp = gpar(lwd = 2, lineend = "square", linejoin = "round"))
        grid.polygon(x = c(end.mm, end.mm, end.mm + 2), y = yy1,
                     gp = gpar(fill = "black", lineend = "square",
                     linejoin = "round"), default.units = "mm")
        ## start arrow and sample id
        seekViewport(LETTERS[1])
        start.mm <- pad.length * 2
        grid.lines(x=unit(c(start.mm, start.mm), "mm"),
                   y=unit(c(rh, 0), "mm"),
                   gp = gpar(lwd = 2, lineend = "square", linejoin = "round"))
        fontsize.sname <-
            if (nchar(sname2, type = "width") > 6) {
                9
            } else {
                10
            }
        grid.polygon(x = c(start.mm, start.mm, start.mm - 2),
                     y = yy1, default.units = "mm",
                     gp=gpar(fill = "black", lineend = "square",
                     linejoin = "round"))
        grid.text(label = sname2, x = start.mm - 1, y = yy2,
                  just = sjust, rot = 90, default.units = "mm",
                  gp = gpar(fontsize=fontsize.sname))
        popViewport()
        for (i in seq(from = 2, by = 1, length.out = n.rows - 1)) {
            seekViewport(LETTERS[i])
            popViewport()
        }
        popViewport()
    }
    if (dat.out) {
        skel.df
    }
}

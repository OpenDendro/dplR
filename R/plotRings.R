plotRings <- function(year, trwN, trwS = NA_real_,
                      trwE = NA_real_, trwW = NA_real_, 
                      animation = FALSE, sys.sleep = 0.2, 
                      year.labels = TRUE, 
                      d2pith = NA,
                      col.rings = "grey", col.outring = "black", 
                      x.rings = "none", col.x.rings = "red",
                      species.name = NA,
                      saveGIF=FALSE, fname="GIF_plotRings.gif") {
  
  ## Creating a data.frame
  TRW <- data.frame(row.names = year, trwN = trwN, 
                    trwS = trwS, 
                    trwE = trwE,
                    trwW = trwW)
  
  TRW <- TRW[as.logical((rowSums(is.na(TRW))-length(TRW))),] # It is to remove rows with NAs across all rows
  
  # trw means
  TRW$trw.means <- rowMeans(TRW, na.rm = TRUE)
  
  # Distance to pith (d2pith)
  # Add d2pith values,  
  # This code find the index position of the first non-NA value in a 
  # column:  which.min(is.na(TRW$trwE))
  # This code check the NA values of d2pith. If there are NA values 
  # this code do nothing, else sum the individual d2pith values to the 
  # first ring. 
  if(!is.na(mean(d2pith, na.rm = TRUE))) {  
    TRW.d2pith <- TRW[,1:4]
    if(!is.na(d2pith[1])) {
      TRW.d2pith$trwN[which.min(is.na(TRW.d2pith$trwN))] <- TRW.d2pith$trwN[which.min(is.na(TRW.d2pith$trwN))]+d2pith[1] }
    if(!is.na(d2pith[2])) {
      TRW.d2pith$trwS[which.min(is.na(TRW.d2pith$trwS))] <- TRW.d2pith$trwS[which.min(is.na(TRW.d2pith$trwS))]+d2pith[2] }
    if(!is.na(d2pith[3])) {
      TRW.d2pith$trwE[which.min(is.na(TRW.d2pith$trwE))] <- TRW.d2pith$trwE[which.min(is.na(TRW.d2pith$trwE))]+d2pith[3] }
    if(!is.na(d2pith[4])) {
      TRW.d2pith$trwW[which.min(is.na(TRW.d2pith$trwW))] <- TRW.d2pith$trwW[which.min(is.na(TRW.d2pith$trwW))]+d2pith[4] }
    # add d2pith to the first ring of the trw.means
    TRW$trw.means[1] <- rowMeans(TRW.d2pith[1,], na.rm = TRUE)  
  }
  
  # Accumulative trw.means
  TRW$trw.acc <- cumsum(TRW$trw.means)
  
  # Eccentricity
  y <- TRW$trwN - TRW$trwS   # eccentricity2
  y[is.na(y)] <- 0
  if(exists("y")==TRUE) TRW$N_S <- y  # add to the TRW data.frame
  x <- TRW$trwE - TRW$trwW   # eccentricity1
  x[is.na(x)] <- 0
  if(exists("x")==TRUE) TRW$E_W <- x  # add to the TRW data.frame
  z <- TRW$trw.acc   # accumulative rings
  
 
  # Getting and coloring the narrow and wider rings
   q2 <- as.numeric(quantile(TRW[,5])[2]) # quantile 25% of trw.means
          col.narrow.rings <- ifelse(TRW[,5] <= q2, col.x.rings, col.rings) 
   q4 <- as.numeric(quantile(TRW[,5])[4]) # quantile 75% of trw.means
          col.wider.rings <- ifelse(TRW[,5] >= q4, col.x.rings, col.rings) 
  

  
  ## AREA calculation: pi(radius)^2    ||  pi(z)^2
  
  # Acummulative BAI (inside out)
  TRW$bai.acc <- pi*(TRW$trw.acc)^2  
  # Individual BAI (inside out)
  TRW$bai.ind  <-c(TRW$bai.acc[1], TRW$bai.acc[2:nrow(TRW)] - TRW$bai.acc[1:nrow(TRW)-1]) 
  
 # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # #
  
  ## Plotting  
    if (animation == TRUE) {
    
    # With animation
    for (i in 1:length(x)) {
      # Rings
      par(mar=c(1,4,1,1)+0.1)
      cols <-  c(rep(col.rings, i-1), col.outring) 
      narrow.cols <- c(col.narrow.rings[1:i-1], col.outring) # colors when is selected "narrow.rings"
      wider.cols <- c(col.wider.rings[1:i-1], col.outring) # colors when is selected "wider.rings"
     
      max.acc <- max(z, na.rm = TRUE) * 2.5
      symbols(y = y[1:i], x = if(length(x) > 0) y[1:i] else x[1:i],
               circles=z[1:i], inches=FALSE, xlim = c(-max.acc, max.acc), ylim = c(-max.acc, max.acc), 
               xlab='', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))),
               line=1.5,adj=0.5, side=3, cex=1.5), 
               sub=if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
               line=0.5,adj=0.5, side=3, cex=1), 
               fg=  if(x.rings == "narrow.rings") narrow.cols 
                 else if(x.rings == "wider.rings") wider.cols 
                 else if(x.rings == "none") cols) 
      
      # year labels
      if(year.labels == TRUE) legend('topright', legend=year[i], box.lty=0, inset = 0.01, cex=2)
      
      Sys.sleep(sys.sleep)
    }
  }  
  
 # Without animation
  else {
    par(mar=c(1,4,1,1)+0.1)
    cols <- c(rep(col.rings, length(x)-1), col.outring)
    narrow.cols <- c(col.narrow.rings[1:length(x)-1], col.outring) # colors when is selected "narrow.rings"
    wider.cols <- c(col.wider.rings[1:length(x)-1], col.outring) # colors when is selected "wider.rings"
    rings.lwd <- c(rep(1, length(x)), 3)
    
    max.acc <- max(z, na.rm = TRUE) * 2.5
    symbols( y = y, x = if(length(x) > 0) y else x,
             circles=z, inches=FALSE, xlim = c(-max.acc, max.acc), ylim = c(-max.acc, max.acc), 
             xlab='', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))), line=1.5,adj=0.5, 
             side=3, cex=1.5), sub= if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
             line=0.5,adj=0.5, side=3, cex=1), 
             fg=  if(x.rings == "narrow.rings") narrow.cols 
             else if(x.rings == "wider.rings") wider.cols 
             else if(x.rings == "none") cols)

    # year labels
    if(year.labels == TRUE) legend('topright', legend=paste(range(year)[1], "-", range(year)[2]), box.lty=0, inset = 0.01, cex=1.2)
      }
  
  # saveGIF
  
 if (saveGIF == TRUE) {
   
   saveGIF({
   par (bg="white")
   
   # With animation
   for (i in 1:length(x)) {
     # Rings
     par(mar=c(1,4,1,1)+0.1,cex=1.5)
     cols <-  c(rep(col.rings, i-1), col.outring) 
     narrow.cols <- c(col.narrow.rings[1:i-1], col.outring) # colors when is selected "narrow.rings"
     wider.cols <- c(col.wider.rings[1:i-1], col.outring) # colors when is selected "wider.rings"
     
     max.acc <- max(z, na.rm = TRUE) * 2.5
     symbols(y = y[1:i], x = if(length(x) > 0) y[1:i] else x[1:i],
             circles=z[1:i], inches=FALSE, xlim = c(-max.acc, max.acc), ylim = c(-max.acc, max.acc), 
             xlab='', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))),
                                                    line=1.5,adj=0.5, side=3, cex=1.5), 
             sub=if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
                                      line=0.5,adj=0.5, side=3, cex=1), 
             fg=  if(x.rings == "narrow.rings") narrow.cols 
             else if(x.rings == "wider.rings") wider.cols 
             else if(x.rings == "none") cols) 
     
     # year labels
     if(year.labels == TRUE) legend('topright', legend=year[i], box.lty=0, inset = 0.01, cex=2)
   }
  }, movie.name = fname, interval = sys.sleep, nmax = 10, ani.width = 1000, 
   ani.height = 1000)  
}
 
  # Without saving the GIF
 else {
   par(mar=c(1,4,1,1)+0.1)
   cols <- c(rep(col.rings, length(x)-1), col.outring)
   narrow.cols <- c(col.narrow.rings[1:length(x)-1], col.outring) # colors when is selected "narrow.rings"
   wider.cols <- c(col.wider.rings[1:length(x)-1], col.outring) # colors when is selected "wider.rings"
   rings.lwd <- c(rep(1, length(x)), 3)
   
   max.acc <- max(z, na.rm = TRUE) * 2.5
   symbols( y = y, x = if(length(x) > 0) y else x,
            circles=z, inches=FALSE, xlim = c(-max.acc, max.acc), ylim = c(-max.acc, max.acc), 
            xlab='', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))), line=1.5,adj=0.5, 
                                                   side=3, cex=1.5), sub= if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
                                                                                               line=0.5,adj=0.5, side=3, cex=1), 
            fg=  if(x.rings == "narrow.rings") narrow.cols 
            else if(x.rings == "wider.rings") wider.cols 
            else if(x.rings == "none") cols)
   
   # year labels
   if(year.labels == TRUE) legend('topright', legend=paste(range(year)[1], "-", range(year)[2]), box.lty=0, inset = 0.01, cex=1.2)
 }
 
   
  ## Print Report:  
  print("Output data:")
  # print(TRW)  # all data.frame
  # print Radii lenght [mm/100]
  if(sum(TRW$trwN, na.rm = TRUE) > 0) print(paste("Length Radius N:  ", round(sum(TRW$trwN, na.rm = TRUE), digits=2 ), sep = " ", "mm/100"))
  if(sum(TRW$trwS, na.rm = TRUE) > 0) print(paste("Length Radius S:  ", round(sum(TRW$trwS, na.rm = TRUE), digits=2 ), sep = " ", "mm/100"))
  if(sum(TRW$trwE, na.rm = TRUE) > 0) print(paste("Length Radius E:  ", round(sum(TRW$trwE, na.rm = TRUE), digits=2 ), sep = " ", "mm/100"))
  if(sum(TRW$trwW, na.rm = TRUE) > 0) print(paste("Length Radius W:  ", round(sum(TRW$trwW, na.rm = TRUE), digits=2 ), sep = " ", "mm/100"))
  if(sum(TRW$trw.means, na.rm = TRUE) > 0) print(paste("Disc diameter:  ", round(sum(TRW$trw.means, na.rm = TRUE)*2, digits = 2), sep = " ", "mm/100"))
  if(sum(TRW$bai.ind, na.rm = TRUE) > 0) print(paste("Basal Area of the disc:  ", round(sum(TRW$bai.ind, na.rm = TRUE)/10^6, digits = 2), sep = " ", "mm2"))
  
  
  TRW  
}


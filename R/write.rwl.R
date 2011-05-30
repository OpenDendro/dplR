`write.rwl` <-
function(rwl.df, fname, header=NULL, append=FALSE, prec=0.01)
{
  if(!(prec == 0.01 | prec == 0.001)) stop('prec must eq 0.01 or 0.001')
  if(append) {
    if(!file.exists(fname)) stop("fname does not exist, can\'t append")
    if(length(header)>0) stop("bad idea to append with header")
  }
  if(length(header)>0){
    if(!is.list(header)) stop("header must be a list")
    header.names = c("site.id","site.name","spp.code","state.country","spp",
      "elev","lat","long","first.yr","last.yr","lead.invs","comp.date")
    if(!all(header.names %in% names(header))) {
      stop(paste("header must be a list with names",
      paste(header.names,collapse = ", ")))
    }
    # Record #1: 1-6 Site ID, 10-61 Site Name, 62-65 Species Code, optional ID#s
    # Record #2: 1-6 Site ID, 10-22 State/Country, 23-40 Species, 41-45
    # Elevation, 48-57 Lat-Long, 68-76 1st & last Year
    # Note: lat-lons are in degrees and minutes, ddmm or dddmm
    # Record #3: 1-6 Site ID, 10-72 Lead Investigator, 73-80 comp. date
    header = lapply(header,as.character)
    site.id = header$site.id[1]
    site.name = header$site.name[1]
    spp.code = header$spp.code[1]
    state.country = header$state.country[1]
    spp = header$spp[1]
    elev = header$elev[1]
    lat = header$lat[1]
    long = header$long[1]
    first.yr = header$first.yr[1]
    last.yr = header$last.yr[1]
    lead.invs = header$lead.invs[1]
    comp.date = header$comp.date[1]

    site.id = ifelse(nchar(site.id > 6), substr(site.id, 1, 6),site.id)
    site.id = ifelse(nchar(site.id < 6), encodeString(site.id, width = 6),site.id)
    site.name = ifelse(nchar(site.name) > 52, substr(site.name, 1, 52),site.name)
    site.name = ifelse(nchar(site.name) < 52, encodeString(site.name, width = 52),site.name)
    spp.code = ifelse(nchar(spp.code) > 4, substr(spp.code, 1, 4),spp.code)
    spp.code = ifelse(nchar(spp.code) < 4, encodeString(spp.code, width = 4),spp.code)
    state.country = ifelse(nchar(state.country) > 13, substr(state.country, 1, 13),
      state.country)
    state.country = ifelse(nchar(state.country) < 13, encodeString(state.country, width = 13),state.country)
    spp = ifelse(nchar(spp > 18), substr(spp, 1, 18),spp)
    spp = ifelse(nchar(spp < 18), encodeString(spp, width = 18),spp)
    elev = ifelse(nchar(elev > 5), substr(elev, 1, 5),elev)
    elev = ifelse(nchar(elev < 5), encodeString(elev, width = 5),elev)
    lat.long = ifelse(nchar(long) > 5, paste(lat,long,sep=""),
     paste(lat,long,sep=" "))
    lat.long = ifelse(nchar(lat.long > 10), substr(lat.long, 1, 10),lat.long)
    lat.long = ifelse(nchar(lat.long < 10), encodeString(lat.long, width = 10),lat.long)
    yrs = paste(first.yr,last.yr,sep=" ")
    yrs = ifelse(nchar(yrs > 9), substr(yrs, 1, 9),yrs)
    yrs = ifelse(nchar(yrs < 9), encodeString(yrs, width = 9),yrs)
    lead.invs = ifelse(nchar(lead.invs) > 63, substr(lead.invs, 1, 63),lead.invs)
    lead.invs = ifelse(nchar(lead.invs) < 63, encodeString(lead.invs, width = 63),lead.invs)
    comp.date = ifelse(nchar(comp.date) > 8, substr(comp.date, 1, 8),comp.date)
    comp.date = ifelse(nchar(comp.date) < 8, encodeString(comp.date, width = 8),comp.date)

    hdr1 = paste(site.id, "   ", site.name, spp.code, sep="")
    hdr2 = paste(site.id, "   ", state.country,spp," ",elev,"  ",
      lat.long, "          ", yrs, sep="")
    hdr3 = paste(site.id, "   ", lead.invs, comp.date, sep="")
    hdr = c(hdr1,hdr2,hdr3)
  }

  # Loop through series and write each one
  nseries <- ncol(rwl.df)
  yrs.all = as.numeric(rownames(rwl.df))

  # Sort years using increasing order, reorder rwl.df accordingly
  yrs.order = sort.list(yrs.all)
  yrs.all = yrs.all[yrs.order]
  rwl.df = rwl.df[yrs.order,]

  rwl.out = character()
  na.str = ifelse(prec == 0.01, 9.99, -9.999)
  missing.str = ifelse(prec == 0.01, -9.99, 0)
  prec.rproc = ifelse(prec == 0.01, 100, 1000) # reciprocal of precision
  for(l in 1:nseries) {
    series = rwl.df[,l]
    yrs = yrs.all[!is.na(series)]
    series = series[!is.na(series)]

    series = c(series,na.str)
    yrs = c(yrs,max(yrs)+1)

    min.year = min(yrs)
    max.year = max(yrs)
    decades.vec = yrs%/%10 * 10
    # Output for completely missing decades can be disabled by using
    # the alternate definition of the "decades" list
    decades = seq(min(decades.vec),max(decades.vec),10)
#    decades = unique(decades.vec)
    n.decades = length(decades)

    # 1-6
    rwl.df.name = colnames(rwl.df)[l]
    rwl.df.width = nchar(rwl.df.name)
    # Pad to six
    # If rwl.df.width > 6, truncate
    rwl.df.name = ifelse(rwl.df.width > 6, substr(rwl.df.name, 1, 6),rwl.df.name)
    # Pad to nchar 4 (no leading zero)
    rwl.df.name = ifelse(rwl.df.width < 6,
      formatC(rwl.df.name, wid = 6, format = "f"),rwl.df.name)

    dec.str <- character(n.decades)
    for(i in 1:n.decades){
      # 8-12 decade column (up to 4 numbers and a minus sign from long series)
      dec = decades[i]
      dec.yrs = yrs[decades.vec%in%dec]
      dec.rwl = series[decades.vec%in%dec]

      # Find negative values and mark as missing data, but
      # allow the negative "end of series" marker when prec == 0.001
      neg.match = dec.rwl < 0
      if (prec == 0.001 && i == n.decades)
        neg.match[length(neg.match)] = FALSE 
      dec.rwl[neg.match] = missing.str
      
      # Find missing data.
      if (n.decades==1)
        all.years = dec.yrs[1]:dec.yrs[length(dec.yrs)]
      else if (i==1)
        all.years = dec.yrs[1]:(dec+9)
      else if (i==n.decades)
        all.years = dec:dec.yrs[length(dec.yrs)]
      else
        all.years = dec:(dec+9)
      missing.years = setdiff(all.years,dec.yrs)
      # Mark missing data.
      if (length(missing.years) > 0){
        dec.yrs = c(dec.yrs,missing.years)
        dec.rwl = c(dec.rwl,rep(missing.str,times=length(missing.years)))
        dec.order = sort.list(dec.yrs)
        dec.yrs = dec.yrs[dec.order]
        dec.rwl = dec.rwl[dec.order]
      }

      # Pad to nchar 5 (no leading zero)
      dec.year1 = formatC(dec.yrs[1], dig = 0, wid = 5, format = "f")

      # Convert millimeters to the desired precision
      dec.rwl = round(dec.rwl * prec.rproc, 3)

      # Find and correct illegal uses of the stop marker
      if (prec == 0.01){
        end.match = dec.rwl == 999
        if (i==n.decades)
          end.match[length(end.match)] = FALSE
        dec.rwl[end.match] = 1000
      }

      # Pad to nchar 6 (no leading zero)
      dec.rwl = formatC(dec.rwl, dig = 0, wid = 6, format = "f")

      dec.str[i] = paste(rwl.df.name," ",dec.year1,paste(dec.rwl,sep = "",
        collapse = ""),sep="")
    }
    rwl.out = c(rwl.out,dec.str)
  }
  if(length(header)>0) rwl.out = c(hdr,rwl.out)
  cat(rwl.out , file = fname, sep = "\n", append=append)
}

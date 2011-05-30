# Internal variable.
# Create a list of R expressions. format.tucson[[i]], when evaluated
# (later), returns a string of i concatenated ring widths (6
# characters each).  By having the code here, some time is saved if
# write.tucson.R is called multiple times. Downside: this slows down the
# loading of the library a tiny bit. The list of expressions could
# easily be written open. While these for loops are slower, they are
# arguably a cleaner solution, because one sees the systematic
# structure of the formatting expressions.
format.tucson = list()
for (i in 1:10){
  format.str =
    paste("sprintf(\"", paste(rep("%6.0f", i),collapse=""), "\"", sep="")
  for (j in 1:i){
    format.str = paste(format.str, ",dec.rwl[", j, "]", sep="")
  }
  format.tucson[[i]] = parse(text=paste(format.str, ")", sep=""))
}

# Internal function.
# Increment the given number (vector) x by one in the given base.
# Well, kind of: we count up to and including base (not base-1), and
# the smallest digit is one. Basically, we have a shift of one because
# of array indices starting from 1 instead of 0.  In case another
# digit is needed in the front, vector x grows.
count.base <- function(x, base){
  n.x = length(x)
  pos = n.x
  x[pos] = x[pos] + 1
  while (x[pos] == base + 1){
    x[pos] = 1
    if (pos == 1){
      temp = vector(mode="integer", length=n.x+1)
      temp[2:(n.x+1)] = x
      pos = 2
      x = temp
    }
    x[pos-1] = x[pos-1] + 1
    pos = pos - 1
  }
  x
}

# Internal function.
# Compose a new name by attaching a suffix, which may partially
# replace the original name depending on the limit imposed on the
# length of names.
compose.name <- function(orig.name, alphabet, idx, limit){
  idx.length = length(idx)
  if (idx.length > limit){
    new.name = ""
  } else{
    last.part = paste(alphabet[idx], collapse="")
    first.part = substr(orig.name, 1, limit-idx.length)
    new.name = paste(first.part, last.part, sep="")
  }
  new.name
}

# Internal function.
# Fix names so that they are unique and no longer than the given
# length.  A reasonable effort will be done in the search for a set of
# unique names, although some stones will be left unturned. The
# approach should be good enough for all but the most pathological
# cases.
fix.names <- function(x, limit,mapping.fname,mapping.append){
  x.cut = substr(x, 1, limit)
  unique.cut = unique(x.cut)
  n.unique = length(unique.cut)
  n.x = length(x)
  # Check if there are duplicate names after truncation.
  # No duplicates => nothing to do beyond this point, except return the result.
  if (n.unique == n.x){
    y = x.cut
  } else {
    warning("Duplicate names present. Renaming series.")
    if(nchar(mapping.fname)>0){
      write.map = TRUE
      if(mapping.append) {
        if(!file.exists(mapping.fname)){
          stop("mapping.fname does not exist, can\'t append")
        } else{
          map.file = file(mapping.fname, "a")
        }
      } else{
        map.file = file(mapping.fname, "w")
      }
    } else{
      write.map = FALSE
    }

    y = character(length=n.x)
    alphanumeric = c(0:9, LETTERS, letters)
    n.an = length(alphanumeric)
    # First pass: Keep already unique names
    for (i in 1:n.unique){
      idx.this = which(x.cut %in% unique.cut[i])
      n.this = length(idx.this)
      if (n.this == 1)
        y[idx.this] = x.cut[idx.this]
    }

    x.cut2 = substr(x.cut, 1, limit-1)
    x.cut2[y != ""] = NA
    unique.cut2 = unique(x.cut2) # may contain NA
    n.unique2 = length(unique.cut2)
    # Second pass (exclude names that were set in the first pass):
    # Make rest of the names unique
    for (i in 1:n.unique2){
      this.substr = unique.cut2[i]
      if (is.na(this.substr)) # skip NA
        next
      idx.this = which(x.cut2 %in% this.substr)
      n.this = length(idx.this)
      suffix.count = 0
      for (j in 1:n.this){
        still.looking = TRUE
        while (still.looking){
          suffix.count = count.base(suffix.count, n.an)
          proposed =
            compose.name(unique.cut2[i],alphanumeric,suffix.count,limit)
          if (nchar(proposed)==0){
            warning("Could not remap a name. Some series will be missing.")
            still.looking = FALSE
            proposed = paste(unique.cut2[i], "F", sep="") # F for Fail...
          } else if (!any(y %in% proposed)){
            still.looking = FALSE
          }
        }
        y[idx.this[j]] = proposed
        if (write.map)
          cat(x[idx.this[j]], "\t", proposed, "\n", file=map.file, sep = "")
      }
    }
    if(write.map)
      close(map.file)
  }
  y
}

# Exportable function
`write.tucson` <-
function(rwl.df, fname, header=NULL, append=FALSE, prec=0.01, mapping.fname="", mapping.append=FALSE, long.names=FALSE)
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

    site.id = ifelse(nchar(site.id) > 6, substr(site.id, 1, 6),site.id)
    site.id = ifelse(nchar(site.id) < 6, encodeString(site.id, width = 6),site.id)
    site.name = ifelse(nchar(site.name) > 52, substr(site.name, 1, 52),site.name)
    site.name = ifelse(nchar(site.name) < 52, encodeString(site.name, width = 52),site.name)
    spp.code = ifelse(nchar(spp.code) > 4, substr(spp.code, 1, 4),spp.code)
    spp.code = ifelse(nchar(spp.code) < 4, encodeString(spp.code, width = 4),spp.code)
    state.country = ifelse(nchar(state.country) > 13, substr(state.country, 1, 13),
      state.country)
    state.country = ifelse(nchar(state.country) < 13, encodeString(state.country, width = 13),state.country)
    spp = ifelse(nchar(spp) > 18, substr(spp, 1, 18),spp)
    spp = ifelse(nchar(spp) < 18, encodeString(spp, width = 18),spp)
    elev = ifelse(nchar(elev) > 5, substr(elev, 1, 5),elev)
    elev = ifelse(nchar(elev) < 5, encodeString(elev, width = 5),elev)
    lat.long = ifelse(nchar(long) > 5, paste(lat,long,sep=""),
     paste(lat,long,sep=" "))
    lat.long = ifelse(nchar(lat.long) > 10, substr(lat.long, 1, 10),lat.long)
    lat.long = ifelse(nchar(lat.long) < 10, encodeString(lat.long, width = 10),lat.long)
    yrs = paste(first.yr,last.yr,sep=" ")
    yrs = ifelse(nchar(yrs) > 9, substr(yrs, 1, 9),yrs)
    yrs = ifelse(nchar(yrs) < 9, encodeString(yrs, width = 9),yrs)
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
  col.names = colnames(rwl.df)
  if(length(grep("^([a-z]|[0-9])+$", col.names, ignore.case = TRUE)) != nseries)
    stop("Series names must only contain alphanumeric characters")

  # Sort years using increasing order, reorder rwl.df accordingly
  yrs.order = sort.list(yrs.all)
  yrs.all = yrs.all[yrs.order]
  rwl.df = as.data.frame(rwl.df[yrs.order,])

  first.year = yrs.all[1]
  last.year = yrs.all[length(yrs.all)]
  long.years = FALSE
  if (first.year < -999){
    long.years = TRUE
    if (first.year < -9999)
      stop("Years earlier than -9999 not supported")
  }
  if (last.year > 9999){
    long.years = TRUE
    if (last.year > 99999)
      stop("Hello, user from the future. Year > 99999 not possible.")
  }

  ## The basic name.width is 7.
  name.width = 7

  ## If we set exploit.short to TRUE:
  ## In the absence of long year numbers, it is possible to use a name
  ## that is one character longer.
  ## use.space adjusts the following:
  ## Do we use an extra space between the name and the decade
  ## (reduces maximum length of name by one)?

  ## Different interpretations exist...
  ## Setting long.names to FALSE will produce the same behavior
  ## as in old versions of the function.
  ## We offer the user only one bit of customization (i.e. two options),
  ## at this time anyway. Maybe the original idea of two customization
  ## options was too fine-grained.
  if(long.names){ # http://www.cybis.se/wiki/index.php?title=.rwl on 2010-04-21
    exploit.short = TRUE  # limit is
    use.space = FALSE     # 7 or 8 characters
  } else{ # http://www.ncdc.noaa.gov/paleo/treeinfo.html on 2010-04-21
    exploit.short = FALSE # limit is
    use.space = TRUE      # 6 characters
  }

  if (exploit.short && !long.years)
    name.width = name.width + 1
  if (use.space){
    name.width = name.width - 1
    opt.space = " "
  } else{
    opt.space = ""
  }
  name.width = as.integer(name.width)
  year.width = as.integer(12-name.width-nchar(opt.space)) # year ends at col 12
  
  col.names = fix.names(col.names, name.width, mapping.fname, mapping.append)
  
  if(append)
    rwl.out = file(fname, "a")
  else
    rwl.out = file(fname, "w")
  if(length(header)>0)
    cat(hdr, "\n", file=rwl.out, sep="")
  na.str = ifelse(prec == 0.01, 9.99, -9.999)
  missing.str = ifelse(prec == 0.01, -9.99, 0)
  prec.rproc = ifelse(prec == 0.01, 100, 1000) # reciprocal of precision
  format.year = sprintf("%%%d.0f", year.width)

  for(l in 1:nseries) {
    series = rwl.df[,l]
    idx = !is.na(series)
    yrs = yrs.all[idx]
    series = series[idx]

    series = c(series,na.str)
    yrs = c(yrs,max(yrs)+1)

    min.year = min(yrs)
    max.year = max(yrs)
    decades.vec = yrs%/%10 * 10
    # Output for completely missing decades can be disabled by using
    # the alternate definition of the "decades" list
    decades = seq(from=min(decades.vec), to=max(decades.vec), by=10)
#    decades = unique(decades.vec)
    n.decades = length(decades)

    # 1--name.width
    rwl.df.name = col.names[l]
    rwl.df.width = nchar(rwl.df.name)
    # Pad to name.width
    rwl.df.name = ifelse(rwl.df.width < name.width,
      format(rwl.df.name, width=name.width, justify="right"),rwl.df.name)

    for(i in 1:n.decades){
      # up to 4 numbers and a minus sign from long series
      dec = decades[i]
      dec.idx = decades.vec%in%dec
      dec.yrs = yrs[dec.idx]
      dec.rwl = series[dec.idx]

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
      # Mark missing data.
      if (length(all.years) > length(dec.yrs)){
        missing.years = setdiff(all.years,dec.yrs)
        dec.yrs = c(dec.yrs,missing.years)
        dec.rwl = c(dec.rwl,rep(missing.str,times=length(missing.years)))
        dec.order = sort.list(dec.yrs)
        dec.yrs = dec.yrs[dec.order]
        dec.rwl = dec.rwl[dec.order]
      }

      # Pad to year.width (no leading zero)
      dec.year1 = sprintf(format.year, dec.yrs[1])

      # Convert millimeters to the desired precision
      dec.rwl = round(dec.rwl * prec.rproc)

      # Find and correct illegal uses of the stop marker
      if (prec == 0.01){
        end.match = dec.rwl == 999
        if (i==n.decades)
          end.match[length(end.match)] = FALSE
        dec.rwl[end.match] = 1000
      }

      # Pad to nchar 6 (no leading zero)
      dec.rwl=eval(format.tucson[[length(dec.rwl)]])

      dec.str = paste(rwl.df.name,opt.space,dec.year1,dec.rwl,sep="")
      cat(dec.str, "\n", file = rwl.out, sep="")
    }
  }
  close(rwl.out)
}

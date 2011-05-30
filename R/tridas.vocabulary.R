### Function for checking TRiDaS vocabulary.
tridas.vocabulary <-
    function(category = c("dating type", "measuring method", "shape",
             "location type", "variable", "unit", "remark",
             "dating suffix", "presence / absence",
             "complex presence / absence", "certainty"),
             idx = NA, term = NA, match.exact = FALSE){
        category <- match.arg(category)
        res <-
            switch(category,
                   "dating type" = c("absolute", "dated with uncertainty",
                   "relative", "radiocarbon"),
                   "measuring method" = c("measuring platform",
                   "hand lens and graticule", "onscreen measuring",
                   "visual estimate"),
                   "shape" = c("whole section", "half section",
                   "third section", "quarter section",
                   "wedge where radius is smaller than circumference",
                   "wedge where radius equals the circumference",
                   "wedge where radius is bigger than the circumference",
                   "beam straightened on one side",
                   "squared beam from whole section",
                   "squared beam from half section",
                   "squared beam from quarter section",
                   "plank cut on one side", "radial plank through pith",
                   "radial plank up to pith",
                   paste("tangential plank not including pith with",
                         "breadth larger than a quarter section", sep=" "),
                   paste("plank not including pith with",
                         "breadth smaller than a quarter section", sep= " "),
                   "small part of section", "part of undetermined section",
                   "unknown", "other"),
                   "location type" = c("growth location",
                   "location of use (static)", "location of use (mobile)",
                   "current location", "manufacture location",
                   "find location"),
                   "variable" = c("ring width", "earlywood width",
                   "latewood width", "ring density", "earlywood density",
                   "latewood density", "maximum density", "latewood percent",
                   "vessel size"),
                   "unit" = c("micrometres", "1/100th millimetres",
                   "1/50th millimetres", "1/20th millimetres",
                   "1/10th millimetres", "millimetres", "centimetres",
                   "metres"),
                   "remark" = c("fire damage", "frost damage", "crack",
                   "false ring(s)", "compression wood", "tension wood",
                   "traumatic ducts", "unspecified injury", "single pinned",
                   "double pinned", "triple pinned", "missing ring",
                   "radius shift up", "radius shift down", "moon ring(s)",
                   "diffuse latewood", "density fluctuation",
                   "wide late wood", "wide early wood"),
                   "dating suffix" = c("AD", "BC", "BP"),
                   ## (complex) presence / absence options are in
                   ## order of "increasing information"
                   "presence / absence" = c("unknown", "absent", "present"),
                   "complex presence / absence" = c("unknown",
                   "not applicable", "absent", "incomplete", "complete"),
                   "certainty" = c("unknown", "exact", "approximately",
                   "after", "before")
                   )
        n.term <- length(term)
        if(!is.na(idx[1]))
            res[idx]
        else if(n.term > 1 || !is.na(term)){
            if(match.exact)
                any(res == term)
            else{
                res2 <- character(n.term)
                for(k in 1:n.term)
                    res2[k] <- match.arg(term[k], res)
                res2
            }
        }
        else
            res
    }

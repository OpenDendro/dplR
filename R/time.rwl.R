time.rwl <- function(x,...){
  as.numeric(rownames(x))
}

time.crn <- function(x,...){
  as.numeric(rownames(x))
}


# creating the method for `time.rwl<-` and `time.crn<-`
`time<-` <- function (x, value) {
  UseMethod("time<-")
}

`time<-.rwl` <- function(x, value){
  row.names(x) <- value
  x
}

`time<-.crn` <- function(x, value){
  row.names(x) <- value
  x
}


# Gaussian smoother that is often needed
SM_WINDOW = 5
smoothed<- function(dfx, w= SM_WINDOW){
  dfx$cases <- smoother::smth(
    dfx$cases, window = w, tails = TRUE)
  dfx
}



# creates time series in the correct format 
make_cases <- function (data_set){
  df <- data.frame(date=as.Date(data_set[,1]), cases= as.numeric(data_set[,2]))
  df <- df[order(df$date),]
  return(df)
}

# Not always robust - so not yet in myLib.
require(myLib)

pretty_date <- function(..., start= as.Date("2020-11-01"),
                        end= Sys.Date() + 15, ccloc=2, 
                        xat=NULL){
  if (is.null(xat)) xat= seq(start, by="4 week",
                             length.out= 24)
  xrange= c(start, end)
  f<- "%d-%m"
  xlab="2020 - 2021"
  pretty_plot(..., ccloc=ccloc, xlim= xrange, xlab=xlab, xat=xat, dformat=f)
}

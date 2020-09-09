#' transform table output
#'
#'
function(df, columnname, ){
num <- which(nchar(df$Standstill)==4, arr.ind=TRUE) ##taking all the lines where a standstill date is entered as principal lines
l <- length(num)  # l tells us how many "principal" lines there are
l2 <- nrow(df) # l2 tells us how many lines there are in total
##now a loop to copy the lines to each other
for (k in 1:l){
  ## the following feature solves the problem of the last lines at the end
  if (k<l){
    num2 <- num[k+1]-num[k]-1
  }else {
    num2 <- l2-num[k]
  }
  if(num2>0){
    ##here a loop is used to copy one line after the other
    for (j in 1:num2){
      df[num[k],] <- paste(df[num[k],], df[num[k]+j,], sep=" ")
    }
  } else {}
}
}

#' transform table output
#'
#' When, for example, exporting a table from a pdf, then content belonging to one line is often scattered across several lines.
#' This function copies lines that belong together in one and deletes the rest.
#'
#' @param df is the dataframe to be transformed
#' @param pl is a vector containing the numbers of the "principle lines". Can for example be obtained by a formula like the following: which(is.na(df[,1]), arr.ind=TRUE)
#' @return returns the transformed dataframe
#' @export


t_pdf_tbl <- function(df, pl){
l = length(pl)  # l tells us how many "principal" lines there are
lt = nrow(df) # lt tells us how many lines there are in total
##now a loop to copy the lines to each other
for (k in 1:l){
  ## the following feature solves the problem of the last lines at the end
  if (k<l){
    num = pl[k+1]-pl[k]-1
  }else {
    num = lt-pl[k]
  }
  if(num>0){
    ##here a loop is used to copy one line after the other separated by a space
    for (j in 1:num){
      df[pl[k],] = paste(df[pl[k],], df[pl[k]+j,], sep=" ")
    }
  } else {}
}
df<- df[pl,]
return(df)
}



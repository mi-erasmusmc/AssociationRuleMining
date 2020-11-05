getARMdata <- function(data){
  data = data
  x = as(split(data[,"covariateLabel"], data[,"rowId"]), "transactions")
  return(x)
}


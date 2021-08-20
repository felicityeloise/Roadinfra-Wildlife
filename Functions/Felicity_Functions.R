tidy.df <- function(data.set){
  # data.set is the user entry dataset to tidy up
  data.set <- droplevels(data.set)
  row.names(data.set) <- 1:nrow(data.set)
  return(data.set)
} # Close tidy.df

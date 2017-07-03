tidy.stability <- function(dataframe){
  ## Figure out which vegetation columns had only "F" for forb and were misunderstood as logical by the import
  offenders <- lapply(names(dataframe) %>% setNames(names(dataframe)), function(X) {
    class(dataframe[, X]) %>% grepl(pattern = "logical")
  }) %>% .[grepl(x = names(.), pattern = "^Veg")] %>% .[unlist(.)] %>% names()
  bad.columns <- dataframe[, offenders]
  ## This replaces all the F values with "F" values
  bad.columns[!bad.columns] <- "F"
  ## Replace the columns in the original input with the corrected ones
  dataframe[, offenders] <- bad.columns

  ## Get the header information to bind to data subsets in the lapply() later
  header <- dataframe %>% select(SiteID, SiteKey, PlotID, PlotKey, FormDate, Observer, Recorder, DataEntry, DataErrorChecking, BoxNum)

  ## Make sure that instead of 1:6 we have 1:18 for the Line[n] columns
  lines <- dataframe[, grepl(x = names(dataframe), pattern = "Line[1-6]")]
  lines <- data.frame(
    lines$Line1, lines$Line1, lines$Line1,
    lines$Line2, lines$Line2, lines$Line2,
    lines$Line3, lines$Line3, lines$Line3,
    lines$Line4, lines$Line4, lines$Line4,
    lines$Line5, lines$Line5, lines$Line5,
    lines$Line6, lines$Line6, lines$Line6
  )
  names(lines) <- paste0("Line", 1:18)
  dataframe <- cbind(lines, dataframe[, !grepl(x = names(dataframe), pattern = "Line[1-6]")])

  ## Get each subset of columns ending in 1 through 18, strip the numerical endings, and bind them into an output
  output <- lapply(1:18, function(X) {
    cbind(header,
          dataframe[, grep(x = names(dataframe), pattern = paste0("[a-z]", X, "$"))]
    ) %>% setNames(names(.) %>% stringr::str_replace(., pattern = paste0(X, "$"), replacement = ""))}
  ) %>% dplyr::bind_rows()

  return(output)
}



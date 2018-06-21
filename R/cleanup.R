tidy.stability <- function(dataframe){
  # Inelegant, but this cuts down to only records where there were non-NA values in the Line variables, which should cut out empty records
  dataframe.trim <- dplyr::filter(dataframe,
                                  !is.na(Line1),
                                  !is.na(Line2),
                                  !is.na(Line3),
                                  !is.na(Line4),
                                  !is.na(Line5),
                                  !is.na(Line6))

  # Sometimes there will only be "F" records in a Veg variable, which causes it to be interpreted as logical at some point earlier
  # This forces all the Veg variables into character strings, which will coerce FALSE into "FALSE"
  dataframe <- dplyr::mutate_at(dataframe, dplyr::vars(dplyr::starts_with("Veg")), as.character)
  # The only place where "FALSE" will be a as a string will be in Veg variables coerced from (incorrectly!) logical values above
  dataframe <- dplyr::mutate_at(dataframe, dplyr::vars(dplyr::starts_with("Veg")), function(string){gsub(string, pattern = "FALSE", replacement = "F")})

  ## Make sure that instead of 1:6 we have 1:18 for the Line[n] columns
  # First, get just the Line variables as a data frame
  lines <- dataframe.trim[, grepl(x = names(dataframe.trim), pattern = "^Line[1-6]$")]
  # Then remake that data frame repeating each of the original variables three times
  # This is because each of the Line variables corresponded to THREE of the other variables, thankfully in order (i.e. Line1 described Veg1:Veg3)
  lines <- data.frame(
    lines$Line1, lines$Line1, lines$Line1,
    lines$Line2, lines$Line2, lines$Line2,
    lines$Line3, lines$Line3, lines$Line3,
    lines$Line4, lines$Line4, lines$Line4,
    lines$Line5, lines$Line5, lines$Line5,
    lines$Line6, lines$Line6, lines$Line6
  )
  # Set the names of those variables to Line1:Line18
  names(lines) <- paste0("Line", 1:18)
  # Replace the original Line1:Line6 with the new Line1:Line18
  dataframe.trim <- cbind(lines, dataframe[, !grepl(x = names(dataframe.trim), pattern = "Line[1-6]")])

  ## Get each subset of columns ending in 1 through 18, strip the numerical endings, and bind them into an output
  df.list <- lapply(1:18,
                 function(X, dataframe){
                   # Create a regex that'll catch the variables for the current number
                   variable.regex <- paste0("^(", paste0(c("Line", "Pos", "Veg", "Rating"), X, collapse = "|"), ")$")

                   # Get all the header variables that are present using dplyr::matches() so that if one is missing it just skips it
                   # Plus get all the variables that match the regex we just made
                   current.df <- dplyr::select(dataframe,
                                               dplyr::matches("^(SiteID|SiteKey|PlotID|PlotKey|FormDate|Observer|Recorder|DataEntry|DataErrorChecking|BoxNum)$"),
                                               dplyr::matches(variable.regex))

                   # Strip the number from the variable names
                   names(current.df) <- gsub(names(current.df), pattern = paste0(X, "$"), replacement = "")

                   # Retun the data frame
                   return(current.df)
                   }, dataframe = dataframe.trim)

  output <- dplyr::bind_rows(df.list)

  return(output)
}



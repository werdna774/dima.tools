#' Reading in data from multiple DIMAs
#'
#' @description This will read in data from one or more DIMAs according to the SQL queries requested and includes default SQL queries for canopy gap, line-point intercept, soil stability, and species inventory data. The output is either a list of lists of query results named with the source filename[s] or a list of query results combined from all sources named with the query name[s].
#' @param data.path A string specifying the folder path containing the DIMA[s].
#' @param dima.list An optional character vector of one or more filenames of DIMAs to read data from in \code{data.path}. If not specified, all DIMAs in the folder will be read from.
#' @param all Logical. If \code{TRUE} then the contents of all tables in the database will be read in. Defaults to \code{FALSE}.
#' @param gap Logical. If \code{TRUE} then canopy gap data will be read in with a default SQL query. Defaults to \code{TRUE}.
#' @param lpi Logical. If \code{TRUE} then line-point intercept data will be read in with a default SQL query. Defaults to \code{TRUE}.
#' @param species Logical. If \code{TRUE} then species data will be read in with a default SQL query. Defaults to \code{TRUE}.
#' @param species.inventory Logical. If \code{TRUE} then species inventory data will be read in with a default SQL query. Defaults to \code{TRUE}.
#' @param stability Logical. If \code{TRUE} then soil stability data will be read in with a default SQL query. Defaults to \code{TRUE}.
#' @param stability.tidy Logical. \code{TRUE} then soil stability data will be reformatted from the format in DIMA into a tidy data frame. Defaults to \code{TRUE}.
#' @param custom.query An optional named character vector of one or more SQL queries. Value names should follow the same pattern as "gap", "lpi", "species.inventory".
#' @param combine Logical. If \code{TRUE} then the output will be a named list of data frames, one for each SQL query. The data frames will contain all of the relevant data from all of the DIMAs read from. If \code{FALSE} then the output will be a named list of per-DIMA named lists of query result data frames. Defaults to \code{TRUE}.
#' @export
read.dima <- function(data.path,
                      dima.list = NULL,
                      all.tables = FALSE,
                      gap = TRUE,
                      lpi = TRUE,
                      species = TRUE,
                      species.inventory = TRUE,
                      stability = TRUE,
                      stability.tidy = TRUE,
                      custom.query = NULL,
                      combine = TRUE
                      ){
  if (is.null(dima.list)) {
    dima.list <- list.files(path = data.path, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$")
  } else {
    if (length(dima.list[grepl(x = dima.list, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$")]) != length(dima.list)) {
      stop("Valid file extension required for all DIMAs in argument dima.list")
    }
    if (length(dima.list[dima.list %in% list.files(path = data.path, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$")]) != length(dima.list)) {
      stop(paste0("Unable to find the following DIMAs in the provided data path: ",
                  paste(dima.list[!(dima.list %in% list.files(path = data.path, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$"))], collapse = ", ")))
    }
  }

  queries <- list()
  queries$lpi <- "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.SiteKey, joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, joinSitePlotLine.PlotKey, tblLPIHeader.Observer, tblLPIHeader.Recorder, tblLPIHeader.DataEntry, tblLPIHeader.DataErrorChecking, tblLPIHeader.FormDate, joinSitePlotLine.LineID, tblLPIDetail.PointLoc, tblLPIDetail.TopCanopy, tblLPIDetail.Lower1, tblLPIDetail.Lower2, tblLPIDetail.Lower3, tblLPIDetail.Lower4, tblLPIDetail.Lower5, tblLPIDetail.Lower6, tblLPIDetail.Lower7, tblLPIDetail.SoilSurface, tblLPIDetail.HeightWoody, tblLPIDetail.HeightHerbaceous, tblLPIDetail.SpeciesWoody, tblLPIDetail.SpeciesHerbaceous, tblLPIDetail.ShrubShape, tblLPIDetail.ChkboxTop, tblLPIDetail.ChkboxLower1, tblLPIDetail.ChkboxLower2, tblLPIDetail.ChkboxLower3, tblLPIDetail.ChkboxLower4, tblLPIDetail.ChkboxLower5, tblLPIDetail.ChkboxLower6, tblLPIDetail.ChkboxLower7, tblLPIDetail.ChkboxSoil, tblLPIDetail.ChkboxWoody, tblLPIDetail.ChkboxHerbaceous, tblLPIHeader.CheckboxLabel
  FROM joinSitePlotLine INNER JOIN (tblLPIHeader LEFT JOIN tblLPIDetail ON tblLPIHeader.RecKey = tblLPIDetail.RecKey) ON joinSitePlotLine.LineKey = tblLPIHeader.LineKey;"
  queries$gap <- "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.SiteKey, joinSitePlotLine.PlotID, joinSitePlotLine.PlotKey, tblGapHeader.FormDate, tblGapHeader.Observer, tblGapHeader.Recorder, tblGapHeader.DataEntry, tblGapHeader.DataErrorChecking, tblGapHeader.LineLengthAmount, joinSitePlotLine.LineID, tblGapDetail.GapStart, tblGapDetail.GapEnd, tblGapDetail.Gap, tblGapHeader.Perennials, tblGapHeader.AnnualGrasses, tblGapHeader.AnnualForbs, tblGapHeader.Other
  FROM joinSitePlotLine INNER JOIN (tblGapHeader INNER JOIN tblGapDetail ON tblGapHeader.RecKey = tblGapDetail.RecKey) ON joinSitePlotLine.LineKey = tblGapHeader.LineKey;"
  queries$stability <- "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.SiteKey, joinSitePlotLine.PlotID, joinSitePlotLine.PlotKey, joinSitePlotLine.LineID, tblSoilStabHeader.FormDate, tblSoilStabHeader.Observer, tblSoilStabHeader.Recorder, tblSoilStabHeader.DataEntry, tblSoilStabHeader.DataErrorChecking, tblSoilStabDetail.BoxNum, tblSoilStabDetail.Line1, tblSoilStabDetail.Line2, tblSoilStabDetail.Line3, tblSoilStabDetail.Line4, tblSoilStabDetail.Line5, tblSoilStabDetail.Line6, tblSoilStabDetail.Pos1, tblSoilStabDetail.Pos2, tblSoilStabDetail.Pos3, tblSoilStabDetail.Pos4, tblSoilStabDetail.Pos5, tblSoilStabDetail.Pos6, tblSoilStabDetail.Pos7, tblSoilStabDetail.Pos8, tblSoilStabDetail.Pos9, tblSoilStabDetail.Pos10, tblSoilStabDetail.Pos11, tblSoilStabDetail.Pos12, tblSoilStabDetail.Pos13, tblSoilStabDetail.Pos14, tblSoilStabDetail.Pos15, tblSoilStabDetail.Pos16, tblSoilStabDetail.Pos17, tblSoilStabDetail.Pos18, tblSoilStabDetail.Veg1, tblSoilStabDetail.Veg2, tblSoilStabDetail.Veg3, tblSoilStabDetail.Veg4, tblSoilStabDetail.Veg5, tblSoilStabDetail.Veg6, tblSoilStabDetail.Veg7, tblSoilStabDetail.Veg8, tblSoilStabDetail.Veg9, tblSoilStabDetail.Veg10, tblSoilStabDetail.Veg11, tblSoilStabDetail.Veg12, tblSoilStabDetail.Veg13, tblSoilStabDetail.Veg14, tblSoilStabDetail.Veg15, tblSoilStabDetail.Veg16, tblSoilStabDetail.Veg17, tblSoilStabDetail.Veg18, tblSoilStabDetail.Rating1, tblSoilStabDetail.Rating2, tblSoilStabDetail.Rating3, tblSoilStabDetail.Rating4, tblSoilStabDetail.Rating5, tblSoilStabDetail.Rating6, tblSoilStabDetail.Rating7, tblSoilStabDetail.Rating8, tblSoilStabDetail.Rating9, tblSoilStabDetail.Rating10, tblSoilStabDetail.Rating11, tblSoilStabDetail.Rating12, tblSoilStabDetail.Rating13, tblSoilStabDetail.Rating14, tblSoilStabDetail.Rating15, tblSoilStabDetail.Rating16, tblSoilStabDetail.Rating17, tblSoilStabDetail.Rating18, tblSoilStabDetail.Hydro1, tblSoilStabDetail.Hydro2, tblSoilStabDetail.Hydro3, tblSoilStabDetail.Hydro4, tblSoilStabDetail.Hydro5, tblSoilStabDetail.Hydro6, tblSoilStabDetail.Hydro7, tblSoilStabDetail.Hydro8, tblSoilStabDetail.Hydro9, tblSoilStabDetail.Hydro10, tblSoilStabDetail.Hydro11, tblSoilStabDetail.Hydro12, tblSoilStabDetail.Hydro13, tblSoilStabDetail.Hydro14, tblSoilStabDetail.Hydro15, tblSoilStabDetail.Hydro16, tblSoilStabDetail.Hydro17, tblSoilStabDetail.Hydro18
FROM (tblSoilStabHeader LEFT JOIN tblSoilStabDetail ON tblSoilStabHeader.RecKey = tblSoilStabDetail.RecKey) INNER JOIN joinSitePlotLine ON tblSoilStabHeader.PlotKey = joinSitePlotLine.PlotKey;"
  queries$species <- "SELECT tblSpecies.SpeciesCode, tblSpecies.ScientificName, tblSpeciesGrowthHabit.GrowthHabit, tblSpeciesGrowthHabit.GrowthHabitSub, tblSpecies.Duration, tblSpecies.Invasive, tblSpeciesGroups.GroupName
FROM (tblSpecies INNER JOIN tblSpeciesGrowthHabit ON tblSpecies.GrowthHabitCode = tblSpeciesGrowthHabit.Code) INNER JOIN tblSpeciesGroups ON tblSpecies.Group = tblSpeciesGroups.RecKey;"
  queries$species.inventory <- "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.SiteKey, joinSitePlotLine.PlotID, joinSitePlotLine.PlotKey, tblSpecRichHeader.FormDate, tblSpecRichHeader.Observer, tblSpecRichHeader.Recorder, tblSpecRichHeader.DataEntry, tblSpecRichHeader.DataErrorChecking, tblSpecRichDetail.SpeciesCount, tblSpecRichDetail.SpeciesList
FROM joinSitePlotLine INNER JOIN (tblSpecRichHeader LEFT JOIN tblSpecRichDetail ON tblSpecRichHeader.RecKey = tblSpecRichDetail.RecKey) ON joinSitePlotLine.LineKey = tblSpecRichHeader.LineKey;"

  queries.requested <- queries[c("gap", "lpi", "species", "species.inventory", "stability")[c(gap, lpi, species, species.inventory, stability)]]
  if (!is.null(custom.query)) {
    if (is.null(names(custom.query)) | length(names(custom.query)[!grepl(x = names(custom.query), pattern = "^$")]) != length(names(custom.query))) {
      stop("The custom.query vector must have a name for each value.")
    }
    queries.requested <- c(queries.requested, custom.query)
  }

  data <- list()
  ## This loops because you can only have one channel open at a time, which means there's no use for a lapply()
  for (dima.name in dima.list) {
    data.current <- extract.table(data.path = data.path, dima = dima.name, query = queries.requested)
    if (stability.tidy & ("stability" %in% names(data.current))) {
      data.current$stability <- tidy.stability(data.current$stability)
    }
    ## Add the query results to the list data
    data <- c(data, dima.name = data.current)
  }

  ## If requested, combine all like data from all like queries
  if (combine) {
    output <- lapply(names(queries.requested),
                     function(X) dplyr::bind_rows(data[grepl(x = names(data), pattern = paste0(X, "$"))])) %>%
      setNames(names(queries.requested))
  } else {
    output <- data
  }
  return(output)
}

#' Reading in data from DIMA
#'
#' @description This will read in data from a DIMA according to the SQL query or queries provided. The output is a named list of query results.
#' @param data.path A string specifying the folder path containing the DIMA[s].
#' @param dima A string specifying the filename with extension of the DIMA to read data from in \code{data.path}.
#' @param query An named character vector of one or more SQL queries. Value names should follow the same pattern as "gap", "lpi", "species.inventory".
#' @export
extract.table <- function(data.path, dima, query){
  if (is.null(names(query)) | length(names(query)[!grepl(x = names(query), pattern = "^$")]) != length(names(query))) {
    stop("The query vector must have a name for each value, even if there is only one.")
  }
  if (!grepl(x = dima, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$")) {
    stop("Valid file extension required for the argument dima.")
  }
  if (!(dima %in% list.files(path = data.path, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$"))) {
    stop("Unable to find the specified DIMA in the provided data path")
  }

  ## Use the appropriate function from RODBC:: based on 32- versus 64-bit installs of R
  if (R.Version()$arch == "x86_64") {
    dima.channel <- RODBC::odbcConnectAccess2007(paste(data.path, dima, sep = "/"))
  } else if (R.Version()$arch == "i386") {
    dima.channel <- RODBC::odbcConnectAccess(paste(data.path, dima, sep = "/"))
  }
  ## Apply the SQL queries to the DIMA
  data.current <- lapply(query, FUN = RODBC::sqlQuery, channel = dima.channel, stringsAsFactors = FALSE)
  RODBC::odbcClose(channel = dima.channel)
  return(data.current)
}

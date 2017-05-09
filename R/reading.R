#' Reading in data from multiple DIMAs
#' 
#' @description This will read in data from one or more DIMAs according to the SQL queries requested and includes default SQL queries for canopy gap, line-point intercept, soil stability, and species inventory data. The output is either a list of lists of query results named with the source filename[s] or a list of query results combined from all sources named with the query name[s].
#' @param data.path A string specifying the folder path containing the DIMA[s].
#' @param dima.list An optional character vector of one or more filenames of DIMAs to read data from in \code{data.path}. If not specified, all DIMAs in the folder will be read from.
#' @param gap Logical. If \code{T} then canopy gap data will be read in with a default SQL query. Defaults to \code{T}.
#' @param lpi Logical. If \code{T} then line-point intercept data will be read in with a default SQL query. Defaults to \code{T}.
#' @param species.inventory Logical. If \code{T} then species inventory data will be read in with a default SQL query. Defaults to \code{T}.
#' @param stability Logical. If \code{T} then soil stability data will be read in with a default SQL query. Defaults to \code{T}.
#' @param custom.query An optional named character vector of one or more SQL queries. Value names should follow the same pattern as "gap", "lpi", "species.inventory".
#' @param combine Logical. If \code{T} then the output will be a named list of data frames, one for each SQL query. The data frames will contain all of the relevant data from all of the DIMAs read from. If \code{F} then the output will be a named list of per-DIMA named lists of query result data frames. Defaults to \code{T}.
#' @export
read.dima <- function(data.path,
                      dima.list = NULL,
                      gap = T,
                      lpi = T,
                      species.inventory = T,
                      stability = T,
                      custom.query = NULL,
                      combine = T
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
  queries$lpi <- "SELECT joinSitePlotLine.SiteName, joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, tblLPIHeader.Observer, joinSitePlotLine.LineID, tblLPIDetail.PointLoc, tblLPIDetail.TopCanopy, tblLPIDetail.Lower1, tblLPIDetail.Lower2, tblLPIDetail.Lower3, tblLPIDetail.Lower4, tblLPIDetail.Lower5, tblLPIDetail.Lower6, tblLPIDetail.Lower7, tblLPIDetail.SoilSurface, tblLPIDetail.HeightWoody, tblLPIDetail.HeightHerbaceous, tblLPIDetail.SpeciesWoody, tblLPIDetail.SpeciesHerbaceous
FROM joinSitePlotLine INNER JOIN (tblLPIHeader LEFT JOIN tblLPIDetail ON tblLPIHeader.RecKey = tblLPIDetail.RecKey) ON joinSitePlotLine.LineKey = tblLPIHeader.LineKey;"
  queries$gap <- "SELECT joinSitePlotLine.SiteID, joinSitePlotLine.PlotID, joinSitePlotLine.LineID, tblGapHeader.Observer, tblGapDetail.GapStart, tblGapDetail.GapEnd, tblGapDetail.Gap
  FROM joinSitePlotLine INNER JOIN (tblGapHeader INNER JOIN tblGapDetail ON tblGapHeader.RecKey = tblGapDetail.RecKey) ON joinSitePlotLine.LineKey = tblGapHeader.LineKey;"
  
  queries.requested <- queries[c("gap", "lpi", "stability", "species.inventory")[c(gap, lpi, stability, species.inventory)]]
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
  if (length(dima.list[grepl(x = dima.list, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$")]) != length(dima.list)) {
    stop("Valid file extension required for the argument dima.")
  }
  if (length(dima.list[dima.list %in% list.files(path = data.path, pattern = "\\.(MDB)|(mdb)|(accdb)|(ACCDB)$")]) != length(dima.list)) {
    stop("Unable to find the specified DIMA in the provided data path")
  }

  ## Use the appropriate function from RODBC:: based on 32- versus 64-bit installs of R
  if (R.Version()$arch == "x86_64") {
    dima.channel <- RODBC::odbcConnectAccess2007(paste(data.path, dima.name, sep = "/"))
  } else if (R.Version()$arch == "i386") {
    dima.channel <- RODBC::odbcConnectAccess(paste(data.path, dima.name, sep = "/"))
  }
  ## Apply the SQL queries to the DIMA
  data.current <- lapply(query, FUN = RODBC::sqlQuery, channel = dima.channel, stringsAsFactors = F)
  RODBC::odbcClose(channel = dima.channel)
  return(data.current)
}

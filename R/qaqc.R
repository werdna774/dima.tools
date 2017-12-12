#' @export
check.plot <- function(site.table,
                       plot.table,
                       line.table,
                       header.table.lpi,
                       header.table.gap,
                       header.table.richness,
                       header.table.soilstability,
                       header.table.soilpit,
                       expected.linecount = 3){

  ## The metadata lookup table
  sites.plots.lines.meta <- merge(x = merge(x = dplyr::select(site.table, -DateModified),
                                            y = dplyr::select(plot.table, -DateModified),
                                            by = "SiteKey"),
                                  y = dplyr::select(line.table, -DateModified),
                                  by = "PlotKey")

  sites.plots.lines <- dplyr::select(.data = sites.plots.lines.meta,
                                     dplyr::ends_with(match = "key"),
                                     dplyr::ends_with(match = "id"),
                                     -dplyr::starts_with(match = "esd"))

  linecounts <- dplyr::summarize(.data = dplyr::group_by(.data = sites.plots.lines,
                                                         SiteID,
                                                         PlotID),
                                 line.counts = n())
  errors <- list()
  # Make the error data frames for the linecount discrepancies
  errors$too.few.lines <- dplyr::distinct(sites.plots.lines[, c("SiteID", "PlotID")])
  errors$too.few.lines$LineID <- NA
  errors$too.few.lines$error <- paste0("Fewer than the expected ", expected.linecount, " lines on plot")
  errors$too.few.lines <- errors$too.few.lines[errors$too.few.lines$PlotID %in% linecounts$PlotID[linecounts$line.counts < expected.linecount],]
  errors$too.many.lines <- dplyr::distinct(sites.plots.lines[, c("SiteID", "PlotID")])
  errors$too.many.lines$LineID <- NA
  errors$too.many.lines$error <- paste0("More than the expected", expected.linecount, " lines on plot")
  errors$too.many.lines <- errors$too.many.lines[errors$too.many.lines$PlotID %in% linecounts$PlotID[linecounts$line.counts > expected.linecount],]
  # Which line keys don't show up in any LPI headers?
  errors$missing.lpi <- sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.lpi$error <- "No LPI form on record for the line"
  errors$missing.lpi <- errors$missing.lpi[!(sites.plots.lines$LineKey %in% header.table.lpi$LineKey),]

  # Which line keys don't show up in any canopy gap headers?
  errors$missing.gap <- sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.gap$error <- "No canopy gap form on record for the line"
  errors$missing.gap <- errors$missing.gap[!(sites.plots.lines$LineKey %in% header.table.gap$LineKey),]

  # Which plot keys don't show up in any soil stability headers?
  errors$missing.soilstab <- sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.soilstab$LineID <- NA
  errors$missing.soilstab$error <- "No soil stability form on record for the plot"
  errors$missing.soilstab <- dplyr::distinct(errors$missing.soilstab[!(sites.plots.lines$PlotKey %in% header.table.soilstability$PlotKey),])

  # Now to flip it around!
  # Find only the line keys that show up in species richness headers, then find only the plot keys that don't have and line keys in that set
  errors$missing.richness <- sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.richness$LineID <- NA
  errors$missing.richness$error <- "No species richness form on record for the plot"
  errors$missing.richness <- dplyr::distinct(errors$missing.richness[!(sites.plots.lines$PlotKey %in% sites.plots.lines[(sites.plots.lines$LineKey %in% header.table.richness$LineKey), "PlotKey"]),])

  ########
  # PLOT DESCRIPTION CHECKS
  # This checks for NAs because is.numeric() evaluated the vector as a unit and not each value in the vector
  # So, find the rows with VALID values for a given variable then find all the sites.plots.lines rows that DO NOT have plot keys matching one of them
  errors$missing.state <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.state$LineID <- NA
  errors$missing.state$error <- "The plot is not assigned to a state"
  errors$missing.state <- dplyr::distinct(errors$missing.state[!(sites.plots.lines$PlotKey %in% plot.table$PlotKey[!is.na(plot.table$State)]),])
  errors$missing.county <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.county$LineID <- NA
  errors$missing.county$error <- "The plot is not assigned to a county"
  errors$missing.county <- unique(errors$missing.county[!(sites.plots.lines$PlotKey %in% plot.table$PlotKey[!is.na(plot.table$County)]),])
  errors$missing.directions <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.directions$LineID <- NA
  errors$missing.directions$error <- "The plot does not have navigation directions"
  errors$missing.directions <- dplyr::distinct(errors$missing.directions[!(sites.plots.lines$PlotKey %in% plot.table$PlotKey[!is.na(plot.table$Directions)]),])
  errors$missing.slope <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.slope$LineID <- NA
  errors$missing.slope$error <- "The plot does not have a valid slope"
  errors$missing.slope <- dplyr::distinct(errors$missing.slope[!(sites.plots.lines$PlotKey %in% plot.table$PlotKey[!is.na(plot.table$Slope)]),])
  errors$missing.aspect <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.aspect$LineID <- NA
  errors$missing.aspect$error <- "The plot does not have a valid aspect"
  errors$missing.aspect <- dplyr::distinct(errors$missing.aspect[!(sites.plots.lines$PlotKey %in% plot.table$PlotKey[!is.na(plot.table$Aspect)]),])
  errors$missing.slopeshape <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.slopeshape$LineID <- NA
  errors$missing.slopeshape$error <- "The plot does not have a valid slope shape"
  errors$missing.slopeshape <- dplyr::distinct(errors$missing.slopeshape[!(sites.plots.lines$PlotKey %in% plot.table$PlotKey[!is.na(plot.table$ESD_SlopeShape)]),])
  errors$missing.recentweather <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.recentweather$LineID <- NA
  errors$missing.recentweather$error <- "The plot is errors$missing information regarding recent weather"
  errors$missing.recentweather <- dplyr::distinct(errors$missing.recentweather[!(sites.plots.lines$PlotKey %in% plot.table$PlotKey[!is.na(plot.table$RecentWeatherPast12) | !is.na(plot.table$RecentWeatherPrevious12)]),])

  # This one is flipped. Find all the tblPlots entries where LandscapeType is NA or the LandscapeType needs a secondary value and LandscapeTypeSecondary is "" or NA. Then return all the sites.plots.lines that match those plot keys
  errors$missing.landform <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.landform$LineID <- NA
  errors$missing.landform$error <- "The plot has either an invalid or incomplete landform assignment"
  errors$missing.landform <- dplyr::distinct(errors$missing.landform[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[is.na(plot.table$LandscapeType) | (grepl(plot.table$LandscapeType, pattern = "\\*$") & (plot.table$LandscapeTypeSecondary %in% c("", NA)))]),])

  # Also flipped. Find where the datum was UTM without a zone provided and then get all the sites.plots.lines rows with those plot keys
  errors$missing.zone <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.zone$LineID <- NA
  errors$missing.zone$error <- "The plot coordinates are given as being in UTM but the plot is missing a valid zone"
  errors$missing.zone <- dplyr::distinct(errors$missing.zone[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[plot.table$Datum == "UTM" & plot.table$Zone %in% c("", NA)]),])

  errors$missing.coords <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.coords$LineID <- NA
  errors$missing.coords$error <- "The plot coordinates are missing or invalid"
  errors$missing.coords <- dplyr::distinct(errors$missing.coords[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[plot.table$Easting == 0 | plot.table$Northing == 0]),])
  errors$missing.elev <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.elev$LineID <- NA
  errors$missing.elev$error <- "The plot elevation is missing or invalid"
  errors$missing.elev <- dplyr::distinct(errors$missing.elev[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[plot.table$Elevation == 0]),])
  errors$missing.elev.units <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.elev.units$LineID <- NA
  errors$missing.elev.units$error <- "The plot elevation units are missing or invalid"
  errors$missing.elev.units <- dplyr::distinct(errors$missing.elev.units[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[plot.table$ElevationType == 0]),])

  ########
  # LINE DESCRIPTION CHECKS
  errors$missing.coords.start <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.coords.start$LineID <- NA
  errors$missing.coords.start$error <- "One or more of the starting coordinates for a line is invalid"
  errors$missing.coords.start <- dplyr::distinct(errors$missing.coords.start[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[any(line.table$EastingStart == 0, line.table$NorthingStart == 0, line.table$LongitudeStart == 0, line.table$LatitudeStart == 0)]),])
  errors$missing.coords.end <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.coords.end$LineID <- NA
  errors$missing.coords.end$error <- "One or more of the ending coordinates for a line is invalid"
  errors$missing.coords.end <- dplyr::distinct(errors$missing.coords.end[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[any(line.table$EastingEnd == 0, line.table$NorthingEnd == 0, line.table$LongitudeEnd == 0, line.table$LatitudeEnd == 0)]),])
  errors$missing.elev.start <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.elev.start$LineID <- NA
  errors$missing.elev.start$error <- "One or more of the starting elevations for a line is invalid (if the elevation was 0, then this may be a false positive)"
  errors$missing.elev.start <- dplyr::distinct(errors$missing.elev.start[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[line.table$ElevationStart == 0 | line.table$ElevationStart == 0]),])
  errors$missing.elev.end <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.elev.end$LineID <- NA
  errors$missing.elev.end$error <- "One or more of the ending elevations for a line is invalid (if the elevation was 0, then this may be a false positive)"
  errors$missing.elev.end <- dplyr::distinct(errors$missing.elev.end[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[line.table$ElevationStart == 0 | line.table$ElevationStart == 0]),])
  errors$missing.elev.units <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$missing.elev.units$LineID <- NA
  errors$missing.elev.units$error <- "One or more lines is missing valid elevation units"
  errors$missing.elev.units <- dplyr::distinct(sites.plots.lines[(sites.plots.lines$PlotKey %in% line.table$PlotKey[line.table$ElevationType == 0]),])
  errors$invalid.azimuth <-  sites.plots.lines[, c("SiteID", "PlotID", "LineID")]
  errors$invalid.azimuth$LineID <- NA
  errors$invalid.azimuth$error <- "One or more lines is missing a valid azimuth"
  errors$invalid.azimuth <- dplyr::distinct(errors$invalid.azimuth[(sites.plots.lines$PlotKey %in% plot.table$PlotKey[any(line.table$Azimuth < 0, line.table$Azimuth > 360)]),])

  output <- dplyr::bind_rows(errors)

  return(dplyr::distinct(output))
}

#' @export
check.header <- function(header.table){
  # Is there a date?
  invalid.formdate <- is.na(lubridate::as_date(header.table$FormDate))
  # An observer?
  invalid.observer <- is.na(header.table$Observer) | !is.character(header.table$Observer)
  # A recorder?
  invalid.recorder <- is.na(header.table$Recorder) | !is.character(header.table$Recorder)
  # Someone error checking?
  invalid.dataerrorchecking <- is.na(header.table$DataErrorChecking) | !is.character(header.table$DataErrorChecking)
  # Someone for data entry if it wasn't at least the same day the data were collected?
  invalid.entry <- (is.na(header.table$DataEntry) | !is.character(header.table$DataEntry)) & (format(lubridate::as_date(header.table$FormDate), "%Y%m%d") != format(lubridate::as_date(header.table$DateModified), "%Y%m%d"))

  # Make the error data frames
  dataframes <- list()
  if (any(invalid.formdate)){
    dataframes$formdate <- as.data.frame(header.table[invalid.formdate, c("FormDate", "LineKey")])
    dataframes$formdate$error <- "Missing a valid form date"
    names(dataframes$formdate) <- c("FormDate", "LineKey", "error")
  }
  if (any(invalid.observer)){
    dataframes$observer <- as.data.frame(header.table[invalid.observer, c("FormDate", "LineKey")])
    dataframes$observer$error <- "Missing a valid observer name"
    names(dataframes$observer) <- c("FormDate", "LineKey", "error")
  }
  if (any(invalid.recorder)){
    dataframes$recorder <- as.data.frame(header.table[invalid.recorder, c("FormDate", "LineKey")])
    dataframes$recorder$error <- "Missing a valid recorder name"
    names(dataframes$recorder) <- c("FormDate", "LineKey", "error")
  }
  if (any(invalid.dataerrorchecking)){
    dataframes$datacheck <- as.data.frame(header.table[invalid.dataerrorchecking, c("FormDate", "LineKey")])
    dataframes$datacheck$error <- "Missing a valid data error checker name"
    names(dataframes$datacheck) <- c("FormDate", "LineKey", "error")
  }
  if (any(invalid.entry)){
    dataframes$entry <- as.data.frame(header.table[invalid.entry, c("FormDate", "LineKey")])
    dataframes$entry$error <- "Missing a valid data entry name and the 'last modified' date is a different day than the form date"
    names(dataframes$entry) <- c("FormDate", "LineKey", "error")
  }

  # Combine the error data frames
  if (length(dataframes) > 1) {
    output <- dplyr::bind_rows(dataframes)
  } else if (length(dataframes) == 1) {
    output <- dataframes[[1]]
  } else {
    output <- NULL
  }

  return(output)
}

#' @export
check.richness <- function(detail.table,
                           header.table,
                           sites.plots.lines,
                           all.species){
  # Combine metadata with the details
  richness <- merge(x = merge(x = sites.plots.lines[c("SiteID", "PlotID", "LineID", "LineKey")],
                              y = header.table[, c("LineKey", "RecKey", "FormDate")]),
                    y = detail.table)
  # Compare the species lists to the contents of all.species. This does it by turning the data frame into a list of data frame based on combinations of plot and form date
  invalid.species.list <- lapply(X = split(richness, f = interaction(richness$LineKey, richness$FormDate), drop = TRUE),
                                 FUN = function(X,
                                                all.species){
                                   # Call X something intelligible
                                   current.df <- X
                                   message(paste(current.df$PlotID, current.df$FormDate))
                                   # Get a vector of the species in the current data
                                   species <- strsplit(current.df$SpeciesList, split = ";")[[1]]
                                   # Slice that vector to only codes that aren't valid unknowns or in all.species
                                   invalid.species <- species[!(species %in% all.species$Symbol) & !grepl(species, pattern = "^(AF|AG|PF|PG|SH|TR)[0-9]{2,3}$")]
                                   if (length(invalid.species) > 0) {
                                     # Assemble the output data frame
                                     output <- data.frame("SiteID" = rep.int(unique(current.df$SiteID), times = length(invalid.species)),
                                                          "PlotID" = rep.int(unique(current.df$PlotID), times = length(invalid.species)),
                                                          "LineID" = rep.int(unique(current.df$LineID), times = length(invalid.species)),
                                                          # "FormDate" = as.Date(rep.int(unique(current.df$FormDate), times = length(invalid.species))),
                                                          "error" = invalid.species,
                                                          stringsAsFactors = FALSE)
                                     output <- dplyr::mutate(error  = paste(error, "is not a valid unknown code or found in the USDA PLANTS database."))
                                     output$FormDate <- current.df$FormDate
                                   } else {
                                     output <- NULL
                                   }

                                   return(output[, c("SiteID", "PlotID", "LineID", "FormDate", "error")])
                                 },
                                 all.species = all.species)

  # Turn that list into a dataframe, which might be just grabbing the sole value in it or combining multuple values
  if (length(invalid.species.list) == 1) {
    output <- invalid.species.list[[1]]
  } else if (length(invalid.species.list) > 1) {
    output <- dplyr::bind_rows(invalid.species.list)
  } else {
    output <- NULL
  }

  invalid.header <- check.header(header.table)
  if (!is.null(invalid.header)) {
    invalid.header <- merge(x = invalid.header,
                            y = sites.plots.lines[, c("SiteID", "PlotID", "LineID", "LineKey")],
                            by = "LineKey")
    invalid.header <- invalid.header[,c("SiteID", "PlotID", "LineID", "FormDate", "error")]
    output <- rbind(invalid.header, output)
  }

  return(output)
}

#' @export
check.lpi <- function(detail.table,
                      header.table,
                      sites.plots.lines,
                      all.species) {
  # We'll create vectors to flag where bad species occur. This is so we can loop over however many canopy columns there are and combine the results
  valid.lowercanopy.unknowns <- rep.int(FALSE, times = nrow(detail.table))
  valid.lowercanopy.species <- rep.int(FALSE, times = nrow(detail.table))
  valid.lowercanopy.code <- rep.int(FALSE, times = nrow(detail.table))
  # We don't know how old the DIMA will be, so it's not clear how many lower codes there'll be to evaluate
  lower.variables <- names(dima.tables$tblLPIDetail)[grepl(names(dima.tables$tblLPIDetail), pattern = "^Lower")]
  # Here are the codes for surface and lower canopy
  surface.codes <- c("S", "R", "W", "D", "LC", "M", "CY", "EL", "GR", "CB", "ST", "BY", "BR")
  lower.canopy.codes <- c("L", "HL", "WL", "NL", "DS", "VL", "GR", "CB", "ST")
  # For the canopy layers
  # A loop isn't too inefficient when you expect no more than seven passes
  for (layer in lower.variables) {
    # Check to see if there's a valid unknown code
    valid.lowercanopy.unknowns <- (valid.lowercanopy.unknowns | grepl(detail.table[[layer]], pattern = "^(AF|AG|PF|PG|SH|TR)[0-9]{2,3}$"))
    # Check to see if there's a valid species code
    valid.lowercanopy.species <- (valid.lowercanopy.species | detail.table[[layer]] %in% all.species$Symbol)
    valid.lowercanopy.code <- (valid.lowercanopy.species | detail.table[[layer]] %in% lower.canopy.codes)
  }
  # Vectors to check the first hit and surface hit
  valid.topcanopy <- grepl(detail.table$TopCanopy, pattern = "^(AF|AG|PF|PG|SH|TR)[0-9]{2,3}$") | detail.table$TopCanopy %in% all.species$Symbol | detail.table$TopCanopy %in% c("None")
  valid.surface <- !grepl(detail.table$SoilSurface, pattern = "^(AF|AG|PF|PG|SH|TR)[0-9]{2,3}$") & !(detail.table$SoilSurface %in% all.species$Symbol) & !(detail.table$SoilSurface %in% surface.codes)

  # Get the entries where something was wrong and add an appropriate error. Slicing after adding the error means never worrying about empty data frames
  invalid.top <- detail.table[, c("RecKey", "PointLoc")]
  invalid.top$error <- "Invalid value in the top canopy slot"
  invalid.top <- invalid.top[!valid.topcanopy,]
  invalid.lower <- detail.table[, c("RecKey", "PointLoc")]
  invalid.lower$error <- "Invalid value in a lower canopy slot"
  invalid.lower <- invalid.lower[!valid.lowercanopy.code & !valid.lowercanopy.species & !valid.lowercanopy.unknowns,]
  invalid.surface <- detail.table[, c("RecKey", "PointLoc")]
  invalid.surface$error <- "Invalid value in the surface slot"
  invalid.surface <- invalid.surface[!valid.surface,]

  # Combine all the errors
  invalid.lpi <- rbind(invalid.top, invalid.lower, invalid.surface)

  # Add the site, plot, and line identities
  output <- merge(x = merge(x = invalid.lpi,
                            y = header.table[, c("FormDate", "LineKey", "RecKey")],
                            all.x = TRUE),
                  y = sites.plots.lines,
                  all.x = TRUE)
  output <- output[, c("SiteID", "PlotID", "LineID", "FormDate", "PointLoc", "error")]

  invalid.header <- check.header(header = header.table)
  if (!is.null(invalid.header)) {
    invalid.header <- merge(x = invalid.header,
                            y = sites.plots.lines[, c("SiteID", "PlotID", "LineID", "LineKey")],
                            by = "LineKey")
    invalid.header$PointLoc <- NA
    invalid.header <- invalid.header[, c("SiteID", "PlotID", "LineID", "FormDate", "PointLoc", "error")]
    output <- rbind(invalid.header, output)
  }

  return(output)
}

######## ######## ######## ######## ######## ######## ######## ########
## MAKE SURE THAT LPI SPECIES SHOW UP IN THE CORRESPONDING RICHNESS! ##
######## ######## ######## ######## ######## ######## ######## ########

#' @export
check.gap <- function(detail.table,
                      header.table,
                      sites.plots.lines.meta){
  ### This is going to be per-form because there's not an easy way in the form to specify which records. Consider that a to-do
  # Figure out which records belong to which lines on which plots
  gap.meta <- merge(x = merge(x = detail.table,
                              y = header.table,
                              all.x = TRUE),
                    y = sites.plots.lines.meta[, c("SiteID", "PlotID", "LineID", "LineKey")],
                    all.x = TRUE)
  # Convert the line lengths into the same units as the gaps
  gap.meta$LineLengthAmount[gap.meta$Measure %in% c(1)] <- gap.meta$LineLengthAmount[gap.meta$Measure %in% c(1)] * 100
  gap.meta$LineLengthAmount[gap.meta$Measure %in% c(2)] <- gap.meta$LineLengthAmount[gap.meta$Measure %in% c(2)] * 12

  # Group the data frame for summarization
  gap.meta.group <- dplyr::group_by(.data = gap.meta,
                                    SiteID, PlotID, LineID, FormDate)

  # DIMA variable names have changed for some inexplicable reason, so we force them here
  if ("Perennials" %in% names(gap.meta)){
    names(gap.meta)[names(gap.meta) == "Perennials"] <- "PerennialsCanopy"
  }
  if ("AnnualGrasses" %in% names(gap.meta)){
    names(gap.meta)[names(gap.meta) == "AnnualGrasses"] <- "AnnualGrassesCanopy"
  }
  if ("AnnualForbs" %in% names(gap.meta)){
    names(gap.meta)[names(gap.meta) == "AnnualForbs"] <- "AnnualForbsCanopy"
  }
  if ("Other" %in% names(gap.meta)){
    names(gap.meta)[names(gap.meta) == "Other"] <- "OtherCanopy"
  }

  # Summarize!
  group.meta.invalids <- dplyr::ungroup(dplyr::summarize(.data = gap.meta.group,
                                                         # Are there valid units?
                                                         invalid.units = any(!(Measure %in% c(1, 2))),
                                                         # Are canopy gap data even included?
                                                         not.canopy = any(!(GapData %in% c(1, 2))),
                                                         # Are all the entries numeric?
                                                         invalid.entry.type = any(!is.numeric(GapStart) | !is.numeric(GapEnd)),
                                                         # Are the correct boxes ticked for canopies?
                                                         invalid.canopy = any(!(PerennialsCanopy %in% 1) & !(AnnualGrassesCanopy %in% 1) & !(AnnualForbsCanopy %in% 1) & !(OtherCanopy %in% 0)),
                                                         # Are the calculated gap sizes correct?
                                                         invalid.size = any(abs(GapStart - GapEnd) != Gap),
                                                         # Are all the start values larger than their end values, or vice versa?
                                                         invalid.directions = !(all((GapEnd - GapStart) == abs(GapEnd - GapStart)) | all((GapEnd - GapStart) != abs(GapEnd - GapStart))),
                                                         # Are all the start and end values within the span of the line?
                                                         invalid.distance = any((GapStart > LineLengthAmount | GapEnd > LineLengthAmount) | (GapStart < 0 | GapEnd < 0)),
                                                         # Are there canopy gaps?,
                                                         no.gaps.present = all(NoCanopyGaps %in% 0)
  ))

  # Okay, so there's going to be weirdness with NAs when there weren't gaps
  group.meta.invalids$invalid.size[group.meta.invalids$invalid.size %in% c(NA) & group.meta.invalids$no.gaps.present] <- FALSE
  group.meta.invalids$invalid.directions[group.meta.invalids$invalid.directions %in% c(NA) & group.meta.invalids$no.gaps.present] <- FALSE
  group.meta.invalids$invalid.distance[group.meta.invalids$invalid.distance %in% c(NA) & group.meta.invalids$no.gaps.present] <- FALSE

  # This adds the error then pares down so that we don't struggle to add the variable error to empty data frames
  invalid.units <- group.meta.invalids[, c("SiteID", "PlotID", "LineID", "FormDate")]
  invalid.units$error <- "Units missing"
  invalid.units <- invalid.units[group.meta.invalids$invalid.units,]
  invalid.not.canopy <- group.meta.invalids[, c("SiteID", "PlotID", "LineID", "FormDate")]
  invalid.not.canopy$error <- "Form settings do not include canopy gaps"
  invalid.not.canopy <- invalid.not.canopy[group.meta.invalids$not.canopy,]
  invalid.entry.type <- group.meta.invalids[, c("SiteID", "PlotID", "LineID", "FormDate")]
  invalid.entry.type$error <- "Not all gap start and end values are numeric"
  invalid.entry.type <- invalid.entry.type[group.meta.invalids$invalid.entry.type & !group.meta.invalids$no.gaps.present,]
  invalid.canopy <- group.meta.invalids[, c("SiteID", "PlotID", "LineID", "FormDate")]
  invalid.canopy$error <- "Selection of plants that stop a gap either includes 'Other' or does not include 'Perennials', 'Annual Forbs', and 'Annual Grasses'"
  invalid.canopy <- invalid.canopy[group.meta.invalids$invalid.canopy,]
  invalid.size <- group.meta.invalids[, c("SiteID", "PlotID", "LineID", "FormDate")]
  invalid.size$error <- "One or more of the gap size values does not match the value of |gap start - gap end|"
  invalid.size <- invalid.size[group.meta.invalids$invalid.size & !group.meta.invalids$no.gaps.present,]
  invalid.direction <- group.meta.invalids[, c("SiteID", "PlotID", "LineID", "FormDate")]
  invalid.direction$error <- "One or more gaps is recorded in the opposite direction of the others (i.e. start > end vs end > start)"
  invalid.direction <- invalid.direction[group.meta.invalids$invalid.directions,]
  invalid.distance <- group.meta.invalids[, c("SiteID", "PlotID", "LineID", "FormDate")]
  invalid.distance$error <- "One or more gap start or end values is less than 0 or greater than the line length"
  invalid.distance <- invalid.distance[group.meta.invalids$invalid.distance,]

  invalid.header <- check.header(header = header.table)
  if (!is.null(invalid.header)) {
    invalid.header <- merge(x = invalid.header,
                            y = sites.plots.lines.meta[, c("SiteID", "PlotID", "LineID", "LineKey")],
                            by = "LineKey")
    invalid.header <- invalid.header[,c("SiteID", "PlotID", "LineID", "FormDate", "error")]
  }

  output <- rbind(invalid.header,
                  invalid.units,
                  invalid.not.canopy,
                  invalid.canopy,
                  invalid.entry.type,
                  invalid.size,
                  invalid.direction,
                  invalid.distance)

  return(output)

}

#' @export
check.dima <- function(dima.tables = NULL,
                       dima.filepath = NULL,
                       dima.filename = NULL,
                       all.species,
                       expected.linecount = 3,
                       write = TRUE){

  if (is.null(dima.tables)) {
    if (is.null(dima.filepath) & !is.null(dima.filename)) {
      stop("If providing a DIMA filename, you have to provide a filepath for that DIMA as well.")
    } else if (!is.null(dima.filepath) & is.null(dima.filename)) {
      stop("If providing a DIMA filepath, you have to provide a filename for the DIMA you want to check")
    } else if (is.null(dima.filepath) & is.null(dima.filename)) {
      stop("You must either provide a valid DIMA filepath and filename or the output from read.dima() as the argument dima.tables")
    } else {
      dima.tables <- read.dima(data.path = dima.filepath,
                               dima.list = dima.filename,
                               all.tables = TRUE)
    }
  } else {
    message("Using the provided DIMA tables")
  }

  critical.tables <- c("tblSites", "tblPlots", "tblLines",
                       "tblLPIDetail", "tblLPIHeader",
                       "tblGapDetail", "tblGapHeader",
                       "tblSoilStabDetail", "tblSoilStabHeader",
                       "tblSpecRichDetail", "tblSpecRichHeader",
                       "tblSoilPits")

  if (any(!(critical.tables %in% names(dima.tables)))){
    stop(paste("The following critical dataframes are missing from dima.tables:", paste(critical.tables[!(critical.tables %in% names(dima.tables))], collapse = ", ")))
  }
  ## The metadata lookup table
  sites.plots.lines.meta <- merge(x = merge(x = dplyr::select(dima.tables$tblSites, -DateModified),
                                            y = dplyr::select(dima.tables$tblPlots, -DateModified),
                                            by = "SiteKey"),
                                  y = dplyr::select(dima.tables$tblLines, -DateModified),
                                  by = "PlotKey")

  sites.plots.lines <- dplyr::select(.data = sites.plots.lines.meta,
                                     dplyr::ends_with(match = "key"),
                                     dplyr::ends_with(match = "id"),
                                     -dplyr::starts_with(match = "esd"))

  errors <- list()

  errors[["plots"]] <- check.plot(site.table = dima.tables$tblSites,
                             plot.table = dima.tables$tblPlots,
                             line.table = dima.tables$tblLines,
                             header.table.lpi = dima.tables$tblLPIHeader,
                             header.table.gap = dima.tables$tblGapHeader,
                             header.table.richness = dima.tables$tblSpecRichHeader,
                             header.table.soilstability = dima.tables$tblSoilStabHeader,
                             header.table.soilpit = dima.tables$tblSoilPits)


  errors[["richness"]] <- check.richness(detail.table = dima.tables$tblSpecRichDetail,
                                    header.table = dima.tables$tblSpecRichHeader,
                                    all.species = all.species,
                                    sites.plots.lines = sites.plots.lines)


  errors[["lpi"]] <- check.lpi(detail.table = dima.tables$tblLPIDetail,
                          header.table = dima.tables$tblLPIHeader,
                          all.species = all.species,
                          sites.plots.lines = sites.plots.lines)


  errors[["gap"]] <- check.gap(detail.table = dima.tables$tblGapDetail,
                          header.table = dima.tables$tblGapHeader,
                          sites.plots.lines.meta = sites.plots.lines.meta)

  if (write) {
    if (is.null(dima.filepath)) {
      message("The results will not be written due to a lack of a valid filepath.")
    } else {
      lapply(X = names(errors),
             FUN = function(X, errors, filepath){
               write.csv(errors[[X]], paste0(filepath, "/", paste0("errors_", X)))
             }, errors = errors, filepath = dima.filepath)
    }
  }
  return(errors)
}

# All the species in PLANTS
# all.species <- read.csv("C:/Users/Nelson/Documents/Projects/dima.tools/inst/defaults/species.csv", stringsAsFactors = FALSE)
#
# # A list of the error data frames. There's not a good way to write them to Excel right now
# errors <- check.dima(dima.filepath = "C:/Users/Nelson/Desktop",
#                      dima.filename = "CRC_AIMTraining2016_COMBINED.mdb",
#                      all.species = all.species)




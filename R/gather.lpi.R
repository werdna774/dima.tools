#' Gather LPI data into tall/long data frames
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dima.tables A list of data frames. Recommended to use the output from\code{read.dima()}. Must contain data frames called tblLPIHeader and tblLPIDetail which have the same fields as the tables in DIMA with the same names. If \code{meta} is \code{TRUE} then it must also include data frames called tblSites, tblPlots, and tblLines which have the same fields as the tables in DIMA with the same names. If \code{species.characteristics} is \code{TRUE} then it must also include data frames called tblSpecies and tblSpeciesGrowthHabit (and optionally tblSpeciesGroups) which have the same fields as the tables in DIMA with the same names.
#' @param meta Logical. If \code{TRUE} then the site, plot, and line names and keys will be added to the output data frames from \code{dima.list} using the data frames called tblSites, tblPlots, and tblLines. Defaults to \code{TRUE}
#' @param species.characteristics Logical. If \code{TRUE} then the available species information will be added from \code{dima.list} using the data frames tblSpecies, tblSpeciesGrowthHabit, and, if available, tblSpeciesGroups. Defaults to \code{TRUE}.
#' @return A list of two data frames: one containing the data from the LPI pin intercepts and one containing the data from the height methd done alongside pin drops.
#' @export
gather.lpi <- function(dima.tables,
                       meta = TRUE,
                       species.characteristics = TRUE){
  if (meta) {
    ## Merge the site, plot, and line details with the LPI data
    lpi <- metamerge(dima.tables = dima.tables, "LPI")
  } else {
    lpi <- merge(dima.tables$tblLPIHeader, dima.tables$tblLPIDetail)
  }


  ## Strip out most of the variables because they're not relevant
  lpi.restricted <- dplyr::select(.data = lpi,
                                  DateModified,
                                  dplyr::starts_with("site", ignore.case = TRUE),
                                  dplyr::starts_with("plot", ignore.case = TRUE),
                                  dplyr::starts_with("line", ignore.case = TRUE),
                                  PointLoc,
                                  PointNbr,
                                  dplyr::contains("Top"),
                                  dplyr::contains("Lower"),
                                  dplyr::contains("Surface"),
                                  dplyr::ends_with("Woody"),
                                  dplyr::ends_with("Herbaceous"),
                                  dplyr::starts_with("Chkbox"),
                                  -dplyr::starts_with("avg"),
                                  -dplyr::contains("option", ignore.case = TRUE))

  ## Make a tall data frame with the site-plot-line-point identifiers and the hit codes by layer
  lpi.hits.tall <- tidyr::gather(data = dplyr::select(.data = lpi.restricted,
                                                      DateModified,
                                                      dplyr::starts_with("site", ignore.case = TRUE),
                                                      dplyr::starts_with("plot", ignore.case = TRUE),
                                                      dplyr::starts_with("line", ignore.case = TRUE),
                                                      PointLoc,
                                                      PointNbr,
                                                      TopCanopy,
                                                      dplyr::matches("^Lower[1-7]$"),
                                                      SoilSurface),
                                 key = "layer",
                                 value = "code",
                                 -(SiteKey:PointNbr))

  ## Make a tall data frame with the site-plot-line-point identifiers and the checkbox status by layer
  lpi.chkbox.tall <- tidyr::gather(data = dplyr::select(.data = lpi.restricted,
                                                        DateModified,
                                                        dplyr::starts_with("site", ignore.case = TRUE),
                                                        dplyr::starts_with("plot", ignore.case = TRUE),
                                                        dplyr::starts_with("line", ignore.case = TRUE),
                                                        PointLoc,
                                                        PointNbr,
                                                        dplyr::starts_with("Chkbox"),
                                                        -dplyr::matches("Woody$|Herbaceous$")),
                                   key = "layer",
                                   value = "checkbox",
                                   -(SiteKey:PointNbr))

  ## Make the names in the layer variable match
  lpi.chkbox.tall$layer <- stringr::str_replace_all(string = lpi.chkbox.tall$layer,
                                                    pattern = "^Chkbox",
                                                    replacement = "")

  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Top"] <- "TopCanopy"
  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Soil"] <- "SoilSurface"

  ## Make a tall data frame with the site-plot-line-point identifiers and the heights by layer
  lpi.layerheight.tall <- tidyr::gather(data = dplyr::select(.data = lpi.restricted,
                                                             dplyr::starts_with("site", ignore.case = TRUE),
                                                             dplyr::starts_with("plot", ignore.case = TRUE),
                                                             dplyr::starts_with("line", ignore.case = TRUE),
                                                             PointLoc,
                                                             PointNbr,
                                                             dplyr::starts_with("Height"),
                                                             -dplyr::matches("Woody$|Herbaceous$")),
                                        key = "layer",
                                        value = "layer.height",
                                        -(SiteKey:PointNbr))

  ## Make the names in the layer variable match
  lpi.layerheight.tall$layer <- stringr::str_replace_all(string = lpi.layerheight.tall$layer,
                                                         pattern = "^Height",
                                                         replacement = "")

  lpi.layerheight.tall$layer[lpi.layerheight.tall$layer == "Top"] <- "TopCanopy"
  lpi.layerheight.tall$layer[lpi.layerheight.tall$layer == "Surface"] <- "SoilSurface"

  ## Merge the three tall data frames
  lpi.tall <- dplyr::distinct(merge(x = merge(x = lpi.hits.tall,
                                              y = lpi.chkbox.tall,
                                              all = TRUE),
                                    y = lpi.layerheight.tall,
                                    all = TRUE))

  ## Make an AIM height data frame
  lpi.habitheight.tall <- dplyr::select(.data = lpi.restricted,
                                        DateModified,
                                        dplyr::starts_with("site", ignore.case = TRUE),
                                        dplyr::starts_with("plot", ignore.case = TRUE),
                                        dplyr::starts_with("line", ignore.case = TRUE),
                                        PointLoc,
                                        PointNbr,
                                        dplyr::matches("Woody$|Herbaceous$"))

  ## If we're adding species
  if (species.characteristics) {
    ## Make a species characteristics data frame
    species <- merge(x = dima.tables$tblSpecies,
                     y = dima.tables$tblSpeciesGrowthHabit,
                     by.x = "GrowthHabitCode",
                     by.y = "Code",
                     all.x = TRUE)
    ## If this is a recent enough version of DIMA, bring in the group information
    if (class(dima.tables$tblSpeciesGroups) == "data.frame") {
      species <- merge(x = species,
                       y = dima.tables$tblSpeciesGroups,
                       by.x = "Group",
                       by.y = "RecKey",
                       all.x = TRUE)
    }

    ## Add species information
    lpi.tall <- merge(x = lpi.tall,
                      y = species,
                      by.x = "code",
                      by.y = "SpeciesCode",
                      all.x = TRUE)

    ## Do the same for the habitat height data, but woody then herbaceous
    lpi.habitheight.tall <- merge(x = lpi.habitheight.tall,
                                  y = species,
                                  by.x = "SpeciesWoody",
                                  by.y = "SpeciesCode",
                                  all.x = TRUE)

    ## Because we need to rename these to end in Woody and Herbaceous to match what they go with
    names(lpi.habitheight.tall)[names(lpi.habitheight.tall) %in% names(species)] <- paste0(names(lpi.habitheight.tall)[names(lpi.habitheight.tall) %in% names(species)], "Woody")

    lpi.habitheight.tall <- merge(x = lpi.habitheight.tall,
                                  y = species,
                                  by.x = "SpeciesHerbaceous",
                                  by.y = "SpeciesCode",
                                  all.x = TRUE)

    names(lpi.habitheight.tall)[names(lpi.habitheight.tall) %in% names(species)] <- paste0(names(lpi.habitheight.tall)[names(lpi.habitheight.tall) %in% names(species)], "Herbaceous")
  }

  ## Output the list
  output <- list("layers" = lpi.tall, "heights" = lpi.habitheight.tall)
}

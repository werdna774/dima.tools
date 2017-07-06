species.qc <- function(dima.tables){
  ## Get tall data frames
  lpi.tall <- gather.lpi(dima.tables = dima.tables,
                         meta = FALSE,
                         species.characteristics = FALSE)

  ## Get all the codes
  codes.used <- c(lpi.tall$layers$codes, lpi.tall$heights$SpeciesHerbaceous, lpi.tall$heights$SpeciesWoody)

  ## Make a species characteristics data frame
  species <- merge(x = dima.tables$tblSpecies,
                   y = dima.tables$tblSpeciesGrowthHabit,
                   by.x = "GrowthHabitCode",
                   by.y = "Code",
                   all.x = TRUE)
  ## If this is a recent enough version of DIMA, bring in the group information
  if (names(dima.tables$tblSpeciesGroups) != "dima.name.tblSpeciesGroups") {
    species <- merge(x = species,
                     y = dima.tables$tblSpeciesGroups,
                     by.x = "Group",
                     by.y = "RecKey",
                     all.x = TRUE)
  }

  ## Make a geometry data frame with the site and plot information (just so we can use it later to bind to calculated values)
  ## the selects() are just to get only the important fields
  sites.plots <- merge(x = dplyr::select(dima.tables$tblSites, SiteKey, SiteID, SiteName),
                       y = dplyr::select(dima.tables$tblPlots, SiteKey, PlotID, State),
                       by = "SiteKey")

  species.used <- dplyr::filter(species, SpeciesCode %in% codes.used)

  species.wrong <- species.used[!(species.used$Duration %in% c("Perennial", "Annual")) | !(species.used$GrowthHabit %in% c("Woody", "Non-woody")) | !(species.used$GrowthHabitSub %in% c("Tree", "Shrub", "Sub-Shrub", "Succulent", "Forb/herb", "Graminoid", "Sedge")),]

  return(species.wrong)
}

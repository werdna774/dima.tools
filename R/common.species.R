common.species <- function(dima.tables){
  ## Make a species characteristics data frame
  species <- merge(x = dima.tables$tblSpecies,
                   y = dima.tables$tblSpeciesGrowthHabit,
                   by.x = "GrowthHabitCode",
                   by.y = "Code",
                   all.x = TRUE)

  species.uniques <- dplyr::select(species, SpeciesCode, Duration, GrowthHabit, GrowthHabitSub) %>%
    dplyr::distinct() %>% dplyr::group_by(SpeciesCode, SpeciesCode, Duration, GrowthHabit, GrowthHabitSub) %>%
    dplyr::summarize(count = n())

  ## Get a list of three data frames, each one with the code and the most common value for that characteristic
  species.common.list <- lapply(X = c("Duration", "GrowthHabit", "GrowthHabitSub"),
                                FUN = function(X, species.df){
                                  counts <- dplyr::group_by_(species.df, "SpeciesCode", X) %>%
                                    dplyr::summarize(count = n())
                                  # Artificially drop the NA counts to 0 so we prefer non-NA values
                                  counts$count[is.na(counts[[X]])] <- 0
                                  counts %>% dplyr::arrange(desc(count)) %>%
                                    dplyr::ungroup() %>% dplyr::group_by(SpeciesCode) %>%
                                    summarize_(paste0("first(", X,")"))
                                }, species.df = species)

  ## Turn the list into a data frame and drop the ones with NA in all three characteristics
  species.common.df <- cbind(species.common.list[[1]],
                             species.common.list[[2]][, 2],
                             species.common.list[[3]][, 2]) %>%
    setNames(c("SpeciesCode", "Duration", "GrowthHabit", "GrowthHabitSub")) %>%
    .[!(is.na(.$Duration) & is.na(.$GrowthHabit) & is.na(.$GrowthHabitSub)),]

  return(list(species.frequent = species.common.df,
              species.uniques = species.uniques))
}

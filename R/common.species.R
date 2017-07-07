#' Find the most common species characteristics assigned in DIMAs
#' @description Compare two or more DIMAs against each other to find out what the variability in species characteristic assignment is like and what the most common assignments are. This pulls in data from DIMAs without combining them because species are expected to appear more than once and combined tables will wreak havoc with some \code{merge()} steps. The data frame \code{uniques} contains one entry per combination of species code, growth habit, growth sub habit, and duration and a count of how many databases that combination occurred in. The data frame \code{frequent} contains one entry for each species code that had at least one of those characteristics assigned and the most frequently occurring value for each characteristic. These are considered separately though, so if ARTRW8 shows up in three DIMAs as non-woody perennial forb, a woody perennial shrub, and a woody annual shrub then the entry in the output would call it a woody perennial shrub.
#' @param data.path A string. The path to the folder containing the DIMA[s].
#' @param dima.list Optional character vector. Two or more filenames of DIMAs to read data from in \code{data.path}. If not specified, all DIMAs in the folder will be read from.
#' @return A list with containing data frames \code{frequent} and \code{unique}.
#' @export
common.species <- function(data.path,
                           dima.list){
  if (length(dima.list) < 2) {
    stop("At least two DIMAs are required for this to work.")
  }

  ## Read in the tables from the DIMAs
  dima.tables <- read.dima(data.path = data.path,
                           dima.list = dima.list,
                           combine = FALSE,
                           custom.query = c("tblSpecies" = "SELECT * FROM tblSpecies",
                                            "tblSpeciesGrowthHabit" = "SELECT * FROM tblSpeciesGrowthHabit"))

  ## Make a species characteristics data frame
  species <- lapply(dima.tables, FUN = function(X){
    merge(x = X$tblSpecies,
          y = X$tblSpeciesGrowthHabit,
          by.x = "GrowthHabitCode",
          by.y = "Code",
          all.x = TRUE) %>%
      dplyr::select(SpeciesCode, Duration, GrowthHabit, GrowthHabitSub)
  }) %>% dplyr::bind_rows()


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

  return(list(frequent = species.common.df,
              uniques = species.uniques))
}

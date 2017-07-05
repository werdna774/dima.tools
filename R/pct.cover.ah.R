#' Calculating percent cover (any hit) from LPI data.
#' @description Calculate the percent cover by plot for variables or combinations of variables. Percent cover will be calculated for every combination of the variables requested, so if the variables are \code{GrowthHabitSub} and \code{Duration} then the output will contain fields like \code{Graminoid.Perennial}, \code{Graminoid.Annual}, \code{Shrub.Perennial}, etc. whereas using just the variable \code{code} will produce one column per species code. Any number of grouping variables can be used. Because these are calculated as cover from anywhere in the canopy column, values for a plot will virtually always sum to > 100% due to multiple layers of vegetation occuring at pin drops.
#' @param lpi.tall A tall/long-format data frame. Use the data frame \code{"layers"} from the \code{gather.lpi()} output.
#' @param ... One or more bare variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @export
pct.cover.ah <- function(lpi.tall, ...){
  ## Get a list of the variables the user wants to group by.
  grouping.variables <- rlang::quos(...)

  summary <- lpi.tall %>%
    dplyr::group_by(SiteKey, SiteID, SiteName, PlotKey, PlotID, LineID, PointNbr,
                    !!!grouping.variables) %>%
    ## Here's the breakdown of the gnarly parts:
    # Because this is a tall format, we want just presence/absence for the grouping at a given point
    # so we'll write in 1 if any of the layers within that grouping has a non-NA and non-"" value
    dplyr::summarize(present = if(any(!is.na(code) & code != "")){1} else {0}) %>%
    tidyr::unite(grouping, !!!grouping.variables, sep = ".") %>%
    dplyr::ungroup() %>% dplyr::group_by(SiteKey, SiteID, SiteName, PlotKey, PlotID, grouping) %>%
    # Within a plot, find the sum of all the "presents" then divide by the number of possible hits, which
    # we'll calculate by taking the highest point number in each line and finding the sum so that variable
    # numbers of lines and line lengths are handled correctly.
    # Sorting the values then taking the last [number of lines on the plot] values is the trick to not over/underestimating!
    dplyr::summarize(percent = 100*sum(present, na.rm = TRUE)/sum(tail(sort(PointNbr), length(unique(LineID))))) %>%
    ## Make the data table wide
    tidyr::spread(key = grouping, value = percent) %>%
    ## Remove the empty groupings, that is the ones where all the grouping variable values were NA
    dplyr::select(-dplyr::matches("^[NA.]{0,100}NA$")) %>%
    ## Replace the NA values with 0s because they represent 0% cover for that grouping
    tidyr::replace_na(replace = setNames(as.list(rep.int(0,
                                                         times = length(unique(names(.)[!(names(.) %in% c("SiteKey", "SiteID", "SiteName", "PlotKey", "PlotID"))])))),
                                         unique(names(.)[!(names(.) %in% c("SiteKey", "SiteID", "SiteName", "PlotKey", "PlotID"))])))
  return(summary)
}

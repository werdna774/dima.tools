#' Calculate the vegetation height
#' @description
#' @param lpi.tall A tall/long-format data frame. Use the data frame \code{"height"} from the \code{gather.lpi()} output.
#' @param ... One or more bare variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param by.year Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.
#' @param by.line Logical. If \code{TRUE} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @param omit.zero Logical. If \code{TRUE} the results omit height measurements of \code {0}
#' @export


mean.height<-function(lpi.tall,
                      by.line = FALSE,
                      tall = FALSE,
                      by.year=FALSE,
                      by.max=FALSE,
                      omit.zero=FALSE,
                      ...){
  ## Get a list of the variables the user wants to group by.
  grouping.variables <- rlang::quos(...)

  # For grouping by line
  if (by.line) {
      level <- rlang::quos(SiteKey, SiteID, SiteName, PlotKey, PlotID, LineKey, LineID)
    } else {
      level <- rlang::quos(SiteKey, SiteID, SiteName, PlotKey, PlotID)
    }


  # For grouping by year
  if (by.year){
    height<-dplyr::mutate(height, Year = format(lubridate::as_date(FormDate), "%Y"))
    level<-rlang::quos(!!!level, Year)
  }

  # Calculate mean height by grouping variable,
  if (omit.zero){
    summary<-lpi.tall %>% dplyr::filter(!is.na(Height),Species!="None") %>%
    dplyr::group_by(!!!level,!!!grouping.variables) %>%
    dplyr::summarize(mean.height=mean(Height)) %>%
    tidyr::unite(indicator, !!!grouping.variables, sep = ".")
  } else {
    summary<-lpi.tall %>% dplyr::filter(!is.na(Height)) %>%
      dplyr::group_by(!!!level,!!!grouping.variables) %>%
      dplyr::summarize(mean.height=mean(Height))%>%
      tidyr::unite(indicator, !!!grouping.variables, sep = ".")
  }

  # Convert to to wide format if necessary
  if (!tall){
    summary <- tidyr::spread(summary, key = indicator, value = mean.height, fill = 0)

  }
  return(summary)
}


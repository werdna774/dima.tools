#Percent Ground Cover
###Need to update this language
{#' @description Calculate the percent cover of ground cover indicators by plot for variables or combinations of variables. Percent cover will be calculated for every combination of the variables requested, so if the variables are \code{GrowthHabitSub} and \code{Duration} then the output will contain fields like \code{Graminoid.Perennial}, \code{Graminoid.Annual}, \code{Shrub.Perennial}, etc. whereas using just the variable \code{code} will produce one column per species code. Any number of grouping variables can be used. Because these are calculated as cover from anywhere in the canopy column, values for a plot will virtually always sum to > 100% due to multiple layers of vegetation occuring at pin drops. Any groupings where all the variable values were \code{NA} will be dropped.
  #' @param lpi.tall A tall/long-format data frame. Use the data frame \code{"layers"} from the \code{gather.lpi()} output.
  #' @param ... One or more bare variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
  #' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
  #' @param hit Character string. If \code{"any"} then percent cover will be calculated using any hit in the canopy column (so a single pin drop record may be counted more than once if it had hits that corresponded to different groups). If \code{"first"} then only the first canopy hit at a pin drop will be used to calculate cover. Defaults to \code{"any"}.
  #' @export
  #' }

##test data--delete once complete
  data.path<-"C:/Users/samccord/Documents/DIMA"
dima.tables<-read.dima(data.path, all.tables = T, combine=T)
lpi.tall<-gather.lpi(dima.data, meta=TRUE, species.characteristics = FALSE)
lpi.tall<-lpi.tall$layers
pct.ground.cover<-function(lpi.tall, tall=FALSE, by.year=FALSE, by.line=TRUE){
  soil.surface.codes<-c("AG","BR","BY","CB","CM","CY","D","EL","FG","GR","LC","LM","M","PC","R","S","ST","WA")
  lower.canopy.codes<-c("L", "WL", "WA", "VL", "AM", "DN", "ER", "HT", "NL", "NT")
  #calculate between plant cover
  summary.1st<-pct.cover(lpi.tall, tall=tall, hit="first",by.year=by.year, by.line=by.line, code) #calculate between plant
  summary.any<-pct.cover(lpi.tall, tall=tall, hit="any",by.year=by.year, by.line=by.line, code) #all ground surface hits




}



lpi <- metamerge(dima.tables =dima.data, "LPI", minimum = TRUE)

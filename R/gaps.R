gaps <- function(dima.tables,
                 by.line = FALSE,
                 tall = FALSE) {

  gap.data <- metamerge(dima.tables = dima.tables,
                        form = "Gap",
                        minimum = TRUE)

  ## Group for the correct set of variables
  if (by.line){
    gap.intermediate <- gap.data %>% dplyr::filter(!is.na(Gap)) %>% dplyr::group_by(SiteKey, SiteID, PlotKey, PlotID, LineKey, LineID, FormDate)
  } else {
    gap.intermediate <- gap.data %>% dplyr::filter(!is.na(Gap)) %>% dplyr::group_by(SiteKey, SiteID, PlotKey, PlotID, FormDate)
  }

  ## Convert the line lengths to the same units as the gaps
  gap.intermediate$LineLengthAmount[gap.intermediate$Measure == 1] <- 100 * gap.intermediate$LineLengthAmount[gap.intermediate$Measure == 1]
  gap.intermediate$LineLengthAmount[gap.intermediate$Measure == 2] <- 12 * gap.intermediate$LineLengthAmount[gap.intermediate$Measure == 2]

  ## Calculate indicators
  gap.summary <- gap.intermediate %>%
    dplyr::summarize(n.20to24 = length(Gap[(Gap >= 20 & Gap <= 24)]),
                     n.26to50 = length(Gap[(Gap >= 25 & Gap <= 50)]),
                     n.51to100 = length(Gap[(Gap > 50 & Gap <= 100)]),
                     n.101to200 = length(Gap[(Gap > 100 & Gap <= 200)]),
                     n.201plus = length(Gap[(Gap > 200)]),
                     length.20to24 = sum(Gap[(Gap >= 20 & Gap <= 24)]),
                     length.25to50 = sum(Gap[(Gap >= 25 & Gap <= 50)]),
                     length.51to100 = sum(Gap[(Gap > 50 & Gap <= 100)]),
                     length.101to200 = sum(Gap[(Gap > 100 & Gap <= 200)]),
                     length.201plus = sum(Gap[(Gap > 200)]),
                     length.total = tail(sort(LineLengthAmount), length(unique(LineKey)))) %>%
    dplyr::mutate(pct.20to24 = 100*length.20to24/length.total,
                  pct.25to50 = 100*length.25to50/length.total,
                  pct.51to100 = 100*length.51to100/length.total,
                  pct.101to200 = 100*length.101to200/length.total,
                  pct.201plus = 100*length.201plus/length.total)


  if (tall) {
    gap.summary <- tidyr::gather(gap.summary,
                                 key = "indicator",
                                 value = "value",
                                 -(SiteKey:FormDate))
  }

  ##Notes for Nelson: This code assumes metric--not sure if we want to load in the tblHeader info so that we know if it is English or Metric
  return(gap.summary)
}

#' 
#' @title Drop levels of sex, maturity state, and shell condition that have been aggregated in a model data dataframe
#' 
#' @description Function to drop the levels of sex, maturity state, and shell condition that have been aggregated
#' in a canonical model data dataframe returned by, e.g., [rCompTCMs::extractMDFR.Data.FleetTimeSeriesABs()].
#' 
#' @param tbl - the dataframe to drop aggregated levels from
#' 
#' @return an object of same class as tbl with aggregated levels dropped, leaving only the original unaggregated levels
#' 
#' @details The model data dataframe should be in canconical format as returned by, e.g.,  [rCompTCMs::extractMDFR.Data.FleetTimeSeriesABs()].
#' The original levels in the input data to the associated TCSAM02 model are returned.
#' 
#' @importFrom dplyr filter
#' 
#' @md
#' 
#' @export
#' 
dropAggregatedDataLevels<-function(tbl){
  for (x_ in c("male","female")) if (nrow(tbl %>% dplyr::filter(x==x_))) tbl %<>% dplyr::filter(x!="all sex")
  for (m_ in c("immature","mature")) if (nrow(tbl %>% dplyr::filter(m==m_))) tbl %<>% dplyr::filter(m!="all maturity")
  for (s_ in c("new shell","old shell")) if (nrow(tbl %>% dplyr::filter(s==s_))) tbl %<>% dplyr::filter(s!="all shell")
  return(tbl)
}

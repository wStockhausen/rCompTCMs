#' 
#' @title Compare management quantities among model scenarios
#' 
#' @description Function to compare management quantities among model scenarios.
#' 
#'@param objs - single model resLst object, or named list of them
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a list of ggplot2 plot objects
#'
#'@details Uses [rTCSAM02::getMDFR.ManagementQuantities()] to get the management quantities calculated
#'in each model case. Uses [computeDiffs()] to compute differences with base case.
#' 
#' @importFrom cowplot get_legend plot_grid
#' @importFrom dplyr filter mutate 
#' @importFrom stringr str_replace
#' @importFrom tibble tribble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches 
#' 
#' @importFrom rTCSAM02 getMDFR.ManagementQuantities
#' 
#' @import ggplot2
#' 
#' @md
#' 
#'@export
#'
modelComparisons.ManagementQuantities(objs,
                                      verbose=FALSE){
  plots = list();
  cap1<-paste0("\n  \nFigure &&figno. Comparison of management quantities among model scenarios.  \n  \n");
  cap2<-paste0("\n  \nFigure &&figno. Comparison of differences in management quantities among model scenarios.  \n  \n")
  
  mdfr = rTCSAM02::getMDFR.ManagementQuantities(objs,verbose=verbose);
  
  #--plot values
  categories = tibble::tribble(~category,~label,
                               "recruitment", "Millions",
                               "biomass",     "1,000's t",
                               "fishing rate",expression(yr^{-1}),
                               "catch",       "1,000's t")
  ps=list();
  for (ic in 1:nrow(categories)){
    dfr = mdfr %>% dplyr::filter(category==categories$category[ic]);
    p = ggplot(dfr,aes(x=type,y=val,colour=case,fill=case))+
                    geom_col(position="dodge") + 
                    facet_grid(.~category,scales="free") +
                    scale_colour_viridis_d(aesthetics=c("colour","fill"))+
                    labs(y=categories$label[[ic]]);
    if (ic==1) lgnd = cowplot::get_legend(p);
    p = p + theme(panel.background=element_rect(colour="black",fill="white"),
                  legend.position="none",
                  axis.title.x=element_blank());
    ps[[ic]] = p;
    rm(dfr,p)
  }
  pg1 = cowplot::plot_grid(plotlist=ps,nrow=2,byrow=TRUE)
  pg2 = cowplot::plot_grid(pg1,lgnd,nrow=1,rel_widths=c(4,1))
  plots[[cap1]] = pg2;
  rm(categories,ps,lgnd,pg1,pg2)
  
  #--calculate % differences relative to the base case,
  #--pivot back to long form,
  #--revert "case" names to original names
  mdfr1 = computeDiffs(mdfr,
                       base=1,
                       cast="process+fleet+category+type",
                       type="percent") %>%
           tidyr::pivot_longer(cols=tidyselect::matches("^.*-"),names_to="case",values_to="val") %>%
           dplyr::mutate(case=stringr::str_replace(case,pattern="^.*-",""));
  
  #--plot % differences relative to the base case
  categories = tibble::tribble(~category,~label,
                               "recruitment", "% difference from base",
                               "biomass",     "% difference from base",
                               "fishing rate","% difference from base",
                               "catch",       "% difference from base")
  ps=list();
  for (ic in 1:nrow(categories)){
    dfr = mdfr1 %>% dplyr::filter(category==categories$category[ic]);
    p = ggplot(dfr,aes(x=type,y=val,colour=case,fill=case))+
                    geom_col(position="dodge") + 
                    facet_grid(.~category,scales="free") +
                    scale_colour_viridis_d(aesthetics=c("colour","fill"))+
                    labs(y=categories$label[[ic]]);
    if (ic==1) lgnd = cowplot::get_legend(p);
    p = p + theme(panel.background=element_rect(colour="black",fill="white"),
                  legend.position="none",
                  axis.title.x=element_blank());
    print(p)
    ps[[ic]] = p;
  }
  pg1 = cowplot::plot_grid(plotlist=ps,nrow=2,byrow=TRUE)
  pg2 = cowplot::plot_grid(pg1,lgnd,nrow=1,rel_widths=c(4,1))
  plots[[cap2]] = pg2;
  
  return(plots);
}
  
  
  
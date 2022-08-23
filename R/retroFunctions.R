#'
#'@title Get a melted dataframe of model estimates from a list of retrospective model results
#'
#'@description This function extracts a melted dataframe from a list of retrospective model results.
#'
#'@param modsRetro - named list of resLst objects, with names corresponding to assessment years representing peels
#'@param FUN - rCompTCMs function used to extract the desired estimates (i.e., an extractMDFR function)
#'@param yadj - adjustment to extract correct max year in results, relative to \code{assessment year-1}
#'@param ... - additional arguments passed to FUN
#'
#'@return dataframe
#'
#'@details Results are extracted using \code{FUN} for a retrospective series of tcsam02 model runs.
#'
#'@note The names of the resLst objects should correspond to the *assessment year* they represent.
#'\code{yadj} should be 0 (the default) for results that end in the "max year" of the assessment,
#'which is one less than the assessment year (e.g., recruitment, mature biomass). For results that end
#'with the year of the assessment (e.g., survey biomass, numbers-at size), \code{yadj} should be 1.
#'
#'@import magrittr
#'@import dplyr
#'
#'@export
#'
retroGetMDFR<-function(modsRetro,
                       FUN,
                       yadj=0,
                       ...){
  if (...length()>0){
    dfrp<-FUN(modsRetro,...);
  } else {
    dfrp<-FUN(modsRetro);
  }
  tmp = dfrp %>% dplyr::mutate(group=as.numeric(as.character(case))) %>%
                 dplyr::filter(as.numeric(as.character(y))<group+yadj) %>%
                 dplyr::select(!group);
  # if (is.factor(dfrp$y)){
  #   lvls<-levels(dfrp$y);
  #   dfrp$y<-as.numeric(as.character(dfrp$y));
  #   dfrp$y<-dfrp$y+yadj;
  #   dfrp<-dfrp[dfrp$y<=(as.numeric(as.character(dfrp$case))),];
  #   dfrp$y<-factor(dfrp$y,levels=lvls);
  # } else if (mode(dfrp$y)=="numeric"){
  #   dfrp$y<-dfrp$y+yadj;
  #   dfrp<-dfrp[dfrp$y<=(as.numeric(as.character(dfrp$case))),];
  # } else if (mode(dfrp$y)=="character"){
  #   dfrp$y<-as.numeric(dfrp$y);
  #   dfrp$y<-dfrp$y+yadj;
  #   dfrp<-dfrp[dfrp$y<=(as.numeric(as.character(dfrp$case))),];
  #   dfrp$y<-as.character(dfrp$y);
  # }
  return(tmp);
}

#'
#'@title Calculate Mohn's rho from a dataframe of retrospective estimates
#'
#'@description This function calculate Mohn's rho from a dataframe of retrospective estimates,
#'and optionally produces a plot.
#'
#'@param dfr - dataframe of retrospective estimates for analysis
#'@param ylab - y axis label for plot
#'@param yadj - adjustment to \code{assessment year-1} for max year in dataframe
#'@param showPlot - flag (T/F) to show plot immediately
#'
#'@return Returns a list with rho, a dataframe of values, and a ggplot2 object, invisibly
#'
#'@details Mohn's rho quantifies the relative bias exhibited by estimates of some
#'quantity from a set of retrospective model runs.
#'
#'@import dplyr
#'@import ggplot2
#'@import magrittr
#'@importFrom reshape2 dcast
#'@importFrom wtsPlots getStdTheme
#'
#'@export
#'
retroCalcMohnsRho<-function(dfr,ylab="value",yadj=0,showPlot=FALSE){
  dfr$case<-as.character(dfr$case);
  peels<-as.character(sort(unique(as.numeric(dfr$case)),decreasing=TRUE));
  np<-length(peels);
  bsyrc<-peels[1];
  bsyrn<-as.numeric(bsyrc);
  dfrp = dfr %>% dplyr::filter(y>=(as.numeric(peels[np])-1+yadj)) %>%  #--peels refer to assessment years
                 dplyr::select(case,y,val);
  dfrpp = dfrp %>% dplyr::filter(case==peels[1]) %>%
                   dplyr::select(base_case=case,y,base_val=val) %>%
                   dplyr::inner_join(dfrp %>% dplyr::filter(case!=peels[1]),by="y") %>%
                   dplyr::mutate(relbias=(val-base_val)/base_val) %>%
                   dplyr::select(base=base_case,peel=case,y,retro_val=val,base_val,relbias) %>%
                   dplyr::arrange(base,peel,y);
  rho = mean(dfrpp$relbias);
  trho<-prettyNum(rho,digits=3);

  p<-ggplot(data=dfrp,mapping=aes_string(x="y",y="val",colour="case")) +
       geom_line() +
       geom_point(data=(dfrp %>% dplyr::filter(y==as.numeric(case)-1+yadj)),size=2) +
       labs(x="peel",y=ylab,colour="peel",caption=paste0("Mohn's rho = ",trho)) +
       lims(y=c(0,NA)) +
       wtsPlots::getStdTheme();
  if (showPlot) print(p);

  return(invisible(list(rho=rho,dfr=dfrpp,plot=p)));
}

#'
#'@title Get a melted dataframe of model estimates from a list of retrospective model results
#'
#'@description This function extracts a melted dataframe from a list of retrospective model results.
#'
#'@param modsRetro - named list of resLst objects, with names corresponding to retrospective peels
#'@param FUN - rCompTCMs function used to extract the desired estimates (i.e., an extractMDFR function)
#'@param yadj - adjustment to make to model years
#'@param ... - additional arguments passed to FUN
#'
#'@return dataframe
#'
#'@details Results are extracted using \code{FUN} for a retrospective series of tcsam02 model runs.
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
  if (is.factor(dfrp$y)){
    lvls<-levels(dfrp$y);
    dfrp$y<-as.numeric(as.character(dfrp$y));
    dfrp$y<-dfrp$y+yadj;
    dfrp<-dfrp[dfrp$y<=(as.numeric(as.character(dfrp$case))),];
    dfrp$y<-factor(dfrp$y,levels=lvls);
  } else if (mode(dfrp$y)=="numeric"){
    dfrp$y<-dfrp$y+yadj;
    dfrp<-dfrp[dfrp$y<=(as.numeric(as.character(dfrp$case))),];
  } else if (mode(dfrp$y)=="character"){
    dfrp$y<-as.numeric(dfrp$y);
    dfrp$y<-dfrp$y+yadj;
    dfrp<-dfrp[dfrp$y<=(as.numeric(as.character(dfrp$case))),];
    dfrp$y<-as.character(dfrp$y);
  }
  return(dfrp);
}

#'
#'@title Calculate Mohn's rho from a dataframe of retrospective estimates
#'
#'@description This function calculate Mohn's rho from a dataframe of retrospective estimates,
#'and optionally produces a plot.
#'
#'@param dfr - dataframe of retrospective estimates for analysis
#'@param ylab - y axis label for plot
#'@param showPlot - flag (T/F) to show plot immmediately
#'
#'@return Returns a list with rho, a dataframe of values, and a ggplot2 object, invisibly
#'
#'@details Mohn's rho quantifies the relative bias exhibited by estimates of some
#'quantity from a set of retrospective model runs.
#'
#'@import ggplot2
#'@import magrittr
#'@importFrom reshape2 dcast
#'
#'@export
#'
retroCalcMohnsRho<-function(dfr,ylab="value",showPlot=FALSE){
  dfr$case<-as.character(dfr$case);
  peels<-as.character(sort(unique(as.numeric(dfr$case)),decreasing=TRUE));
  np<-length(peels);
  bsyrc<-peels[1];
  bsyrn<-as.numeric(bsyrc);
  dfr<-dfr %>% subset(y>=as.numeric(peels[np]));
  dfrp<-reshape2::dcast(dfr,y~case,value.var="val",fun.aggregate=sum,na.rm=FALSE);
  dfrp<-dfrp %>% subset(y>(bsyrn-np));
  mat<-as.matrix(dfrp[,2:ncol(dfrp)]);
  mat<-mat[,peels];#columns now starts with base year
  rownames(mat)<-as.character(dfrp$y);

  dfrRB<-NULL;
  for (p in 2:np){
    peel<-peels[p];
    base<-mat[peel,bsyrc];
    retro<-mat[peel,peel];
    rb<-(retro-base)/base;
    dfrRB<-rbind(dfrRB,
                 data.frame(peel=peel,base=base,retro=retro,relbias=rb));
  }
  rho<-mean(dfrRB$relbias);
  trho<-prettyNum(rho,digits=3);

  p<-ggplot(data=dfr,mapping=aes_string(x="y",y="val",colour="case"));
  p<-p+geom_line();
  p<-p+geom_point(data=dfr %>% subset(y==as.numeric(case)),size=2);
  p<-p+labs(x="peel",y=ylab,colour="peel",caption=paste0("Mohn's rho = ",trho));
  p<-p+lims(y=c(0,NA));
  if (showPlot) print(p);

  return(invisible(list(rho=rho,dfr=dfrRB,plot=p)));
}

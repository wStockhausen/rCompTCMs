#'
#'@title Plot sd_report time series for recruitment and SSB from several model
#'
#'@description This function plots sd_report recruitment and SSB time series from several model runs.
#'
#'@param objs - list of resLst objects
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details Results are extracted using [rTCSAM02::getMDFR.SdRep.RecAndSSB()] for tcsam02 model runs.
#'
#' @import magrittr
#' @import ggplot2
#'
#' @importFrom rTCSAM02 getMDFR.SdRep.RecAndSSB
#'
#'@export
#'
#'@md
#'
compareResults.sdRep.RecAndSSB<-function(objs,
                                         ci=0.95,
                                         verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::compareResults.sdRep.RecAndSSB().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-rTCSAM02::getMDFR.SdRep.RecAndSSB(objs,ci=ci,verbose=verbose)
    mx_year=max(mdfr$y,na.rm=TRUE);

    var_="lnRec";
    dfr = mdfr %>% dplyr::filter(variable==var_) %>%
                   dplyr::mutate(Rec=exp(est),
                                 lci=exp(lci),
                                 uci=exp(uci));
    p1 = ggplot(dfr,aes(x=y,y=Rec,ymin=lci,ymax=uci,colour=case,fill=case)) +
           geom_ribbon(colour=NA,alpha=0.2) + geom_line() +
           labs(x="Year",y="Recruitment (millions)") +
           wtsPlots::getStdTheme() +
           theme(legend.position=c(0.99,0.99),
                 legend.justification=c(1,1));
    p2 = p1 %+% (dfr %>% dplyr::filter(y>mx_year-20));
    pg1 = cowplot::plot_grid(p1 + theme(axis.title.x=element_blank()),
                             p2 + theme(legend.position="none"),
                             ncol=1);

    var_="SSB";
    dfr = mdfr %>% dplyr::filter(variable==var_);
    p1 = ggplot(dfr,aes(x=y,y=est,ymin=lci,ymax=uci,colour=case,fill=case)) +
           geom_ribbon(colour=NA,alpha=0.2) + geom_line() +
           labs(x="Year",y="Mature Biomass at Mating (1000's t)") +
           facet_grid(.~x) +
           scale_y_continuous(limits=c(0,NA),oob=scales::squish) +
           wtsPlots::getStdTheme() ;
    lg = cowplot::get_legend(p1);
    p2 = p1 %+% (dfr %>% dplyr::filter(y>mx_year-20));
    pg2 = cowplot::plot_grid(p1 + theme(legend.position="none")+ theme(axis.title.x=element_blank()),
                             p2 + theme(legend.position="none"),
                             ncol=1);
    pg3 = cowplot::plot_grid(pg2,lg,nrow=1,rel_widths=c(7,1));

    if (verbose) cat("finished rCompTCMs::compareResults.sdRep.RecAndSSB().\n");
    return(list(rec=pg1,ssb=pg2))
}

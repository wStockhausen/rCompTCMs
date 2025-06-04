#'
#'@title Plot sd_report time series for recruitment and SSB from several model
#'
#'@description This function plots sd_report recruitment and SSB time series from several model runs.
#'
#' @param objs - list of resLst objects
#' @param colour_scale - ggplot2 colour scale to substitute for default (if not NULL)
#' @param fill_scale - ggplot2 fill scale to substitute for default (if not NULL)
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@return list with elements
#'\itemize{
#'\item {rec - cowplot object with arranged recruitment plots}
#'\item {ssb - cowplot object with arranged ssb plots}
#'\item {plts$rec - list of ggplot2 objects}
#'\item {plts$ssb - list off ssb ggplot2 objects}
#'}
#'
#'@details Results are extracted using [rTCSAM02::getMDFR.SdRep.RecAndSSB()] for tcsam02 model runs.
#'
#' @import cowplot
#' @import dplyr
#' @import ggplot2
#' @import wtsPlots
#'
#' @importFrom rTCSAM02 getMDFR.SdRep.RecAndSSB
#'
#'@export
#'
#'@md
#'
compareResults.sdRep.RecAndSSB<-function(objs=NULL,
                                         mdfr = NULL,
                                         ci=0.95,
                                         colour_scale=NULL,
                                         fill_scale=NULL,
                                         verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::compareResults.sdRep.RecAndSSB().\n");
    options(stringsAsFactors=FALSE);


    if (is.null(mdfr)){
        cases<-names(objs);
        mdfr<-rTCSAM02::getMDFR.SdRep.RecAndSSB(objs,ci=ci,verbose=verbose)
    } else {
        cases<-unique(mdfr$cases);
    }
    mx_year=max(mdfr$y,na.rm=TRUE);

    var_="lnRec";
    dfr = mdfr |> dplyr::filter(variable==var_) |>
                   dplyr::mutate(Rec=exp(est),
                                 lci=exp(lci),
                                 uci=exp(uci));
    p1r = ggplot(dfr,aes(x=y,y=Rec,ymin=lci,ymax=uci,colour=case,fill=case)) +
           geom_ribbon(colour=NA,alpha=0.2) + geom_line() +
           labs(x="Year",y="Recruitment (millions)") +
           wtsPlots::getStdTheme() +
           theme(legend.position=c(0.99,0.99),
                 legend.justification=c(1,1));
    if (!is.null(colour_scale)) p1r = p1r + colour_scale;
    if (!is.null(fill_scale))   p1r = p1r + fill_scale;
    lg = cowplot::get_legend(p1r);
    p2r = p1r %+% (dfr |> dplyr::filter(y>mx_year-20));
    pg1r = cowplot::plot_grid(p1r + theme(legend.position="none") + theme(axis.title.x=element_blank()),
                             p2r + theme(legend.position="none"),
                             ncol=1);
    pgR = cowplot::plot_grid(pg1r,lg,nrow=1,rel_widths=c(7,1));

    var_="SSB";
    dfr = mdfr |> dplyr::filter(variable==var_);
    p1b = ggplot(dfr,aes(x=y,y=est,ymin=lci,ymax=uci,colour=case,fill=case)) +
           geom_ribbon(colour=NA,alpha=0.2) + geom_line() +
           labs(x="Year",y="Mature Biomass at Mating (1000's t)") +
           facet_grid(.~x) +
           scale_y_continuous(limits=c(0,NA),oob=scales::squish) +
           wtsPlots::getStdTheme() ;
    if (!is.null(colour_scale)) p1b = p1b + colour_scale;
    if (!is.null(fill_scale))   p1b = p1b + fill_scale;
    lg = cowplot::get_legend(p1b);
    p2b = p1b %+% (dfr |> dplyr::filter(y>mx_year-20));
    pg2b = cowplot::plot_grid(p1b + theme(legend.position="none")+ theme(axis.title.x=element_blank()),
                             p2b + theme(legend.position="none"),
                             ncol=1);
    pgS = cowplot::plot_grid(pg2b,lg,nrow=1,rel_widths=c(7,1));

    if (verbose) cat("finished rCompTCMs::compareResults.sdRep.RecAndSSB().\n");
    return(list(rec=pgR,ssb=pgS,plts=list(rec=list(p1r,p2r),ssb=list(p1b,p2b))));
}

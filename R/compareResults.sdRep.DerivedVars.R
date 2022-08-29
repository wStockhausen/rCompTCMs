#'
#'@title Plot sd_report time series for recruitment and SSB from several model
#'
#'@description This function plots sd_report recruitment and SSB time series from several model runs.
#'
#'@param objs - list of resLst objects
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return list with two ggplot2 objects
#'
#'@details Results are extracted using [rTCSAM02::getMDFR.SdRep.DerivedVars()] for tcsam02 model runs.
#' The plot objects can be re-faceted on \code{variable} as desired
#' (e.g., \code{p[[1]]+facet_wrap(~variable,ncol=2,scales="free")}).
#'
#' @import magrittr
#' @import ggplot2
#' @import cowplot
#'
#' @importFrom rTCSAM02 getMDFR.SdRep.DerivedVars
#'
#'@export
#'@md
#'
compareResults.sdRep.DerivedVars<-function(objs,
                                           ci=0.95,
                                           verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::compareResults.sdRep.DerivedVars().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-rTCSAM02::getMDFR.SdRep.DerivedVars(objs,ci=ci,verbose=verbose)

    #--calculate distributions
    p = tibble::tibble(p=seq(from=(1-ci)/2,to=1-(1-ci)/2,by=0.01));

    vars = c("AvgRec","Bmsy","Fmsy","MSY"); # "CurB"   "PrjB"   "OFL"
    dfr = mdfr %>% dplyr::filter(variable %in% vars);
    qdfr = dfr %>% dplyr::full_join(p,by=character()) %>%
                   dplyr::mutate(q=qnorm(p,mean=est,sd=`std.dev`),
                                 d=dnorm(q,mean=est,sd=`std.dev`))
    p1 = ggplot() +
           geom_ribbon(data=qdfr,mapping=aes(x=q,ymax=d,fill=case),ymin=0,colour=NA,alpha=0.2) +
           geom_vline(data=dfr,mapping=aes(xintercept=est,colour=case)) +
           labs(x="value",y="probability") +
           facet_wrap(~variable,nrow=1,scales="free") +
           scale_y_continuous(limits=c(0,NA),oob=scales::squish) +
           wtsPlots::getStdTheme();

    vars = c("CurB","PrjB","OFL");
    dfr = mdfr %>% dplyr::filter(variable %in% vars);
    qdfr = dfr %>% dplyr::full_join(p,by=character()) %>%
                   dplyr::mutate(q=qnorm(p,mean=est,sd=`std.dev`),
                                 d=dnorm(q,mean=est,sd=`std.dev`))
    p2 = ggplot() +
           geom_ribbon(data=qdfr,mapping=aes(x=q,ymax=d,fill=case),ymin=0,colour=NA,alpha=0.2) +
           geom_vline(data=dfr,mapping=aes(xintercept=est,colour=case)) +
           labs(x="value",y="probability") +
           facet_wrap(~variable,nrow=1,scales="free") +
           scale_y_continuous(limits=c(0,NA),oob=scales::squish) +
           wtsPlots::getStdTheme();

    if (verbose) cat("finished rCompTCMs::compareResults.sdRep.DerivedVars().\n");
    return(list(p1,p2))
}

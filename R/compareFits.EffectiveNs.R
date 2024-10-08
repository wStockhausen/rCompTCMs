#'
#'@title Function to compare effective Ns from size compositions by year among several models
#'
#'@description This function compares effective Ns from size compositions by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param mdfr - dataframe
#'@param fleets - names of fleets to include (or "all" or NULL to include all)
#'@param fleet.type - 'survey','fishery'
#'@param category - 'total', 'discard', 'retained', or 'index'
#'@param years - vector of years to show, or 'all' to show all years
#'@param dodge - width to dodge overlapping series
#'@param facet_wrap - ggplot2 formula to produce figure with wrapped facets
#'@param facet_grid - ggplot2 formula to produce figure with gridded facets
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return lists ofggplot2 objects, nested by fishery
#'
#'@details Effective N's > 1.0e10 are treated as NA's.
#'
#'@import ggplot2
#'@import wtsPlots
#'
#'@export
#'
compareFits.EffectiveNs<-function(objs=NULL,
                                  mdfr=NULL,
                                  fleets="all",
                                  fleet.type=c("survey","fishery"),
                                  category=c('index','total','discard','retained'),
                                  years='all',
                                  dodge=0.2,
                                  facet_wrap=NULL,
                                  facet_grid="x+m+s~.",
                                  showPlot=FALSE,
                                  pdf=NULL,
                                  verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareFits.EffectiveNs().\n");
    options(stringsAsFactors=FALSE);

    std_theme = wtsPlots::getStdTheme();

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (fleet.type[1]=='survey') category<-'index';

    if (is.null(mdfr)) {
        mdfr<-rCompTCMs::extractMDFR.Fits.EffectiveNs(objs,
                                                      fleets=fleets,
                                                      fleet.type=fleet.type[1],
                                                      category=category[1],
                                                      verbose=verbose);
        cases<-names(objs);
    } else {
        cases<-unique(mdfr$cases);
    }

    if (is.numeric(years)) mdfr <- mdfr[as.numeric(mdfr$y) %in% years,];

    #----------------------------------
    #effective Ns
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    for (f in uF){
        if (verbose) cat("Plotting fleet",f,"\n")
        mdfrp = mdfr |> dplyr::filter(fleet==f) |> dplyr::mutate(val=ifelse(val>1.0e10,NA,val));
        p<-wtsPlots::plotMDFR.XY(mdfrp,x='y',value.var='val',agg.formula=NULL,
                                 facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                                 xlab='year',ylab='Effective N',units='',lnscale=FALSE,
                                 title=f,
                                 colour='case',guideTitleColour='',
                                 shape='type',guideTitleShape='',
                                 linetype='type',guideTitleLineType='',
                                 showPlot=FALSE) +
            std_theme;
        if (showPlot||!is.null(pdf)) print(p);
        cap<-paste0("\n  \nFigure &&figno. Effective N's for ",f," ",category," catch size compositions.  \n  \n")
        plots[[f]]<-p;
    }#uF

    if (verbose) cat("rCompTCMs::compareFits.EffectiveNs: Done!\n");
    return(plots)
}

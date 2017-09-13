#'
#'@title Function to compare effective Ns from size compositions by year among several models
#'
#'@description This function compares effective Ns from size compositions by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param fleet.type - 'survey','fishery'
#'@param category - 'total','discard','retained','discard mortality', or 'index'
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
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareFits.EffectiveNs<-function(objs=NULL,
                                  mdfr=NULL,
                                  fleet.type=c("survey","fishery"),
                                  category=c('index','captured','discarded','retained','discard mortality'),
                                  years='all',
                                  dodge=0.2,
                                  facet_wrap=NULL,
                                  facet_grid="x~m+s",
                                  showPlot=FALSE,
                                  pdf=NULL,
                                  verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareFits.EffectiveNs().\n");
    options(stringsAsFactors=FALSE);


    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    if (fleet.type[1]=='survey') category<-'index';

    if (is.null(mdfr)) {
        mdfr<-rCompTCMs::extractMDFR.Fits.EffectiveNs(objs,
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
        mdfrp<-mdfr[mdfr$fleet==f,];
        p<-wtsPlots::plotMDFR.XY(mdfrp,x='y',value.var='val',agg.formula=NULL,
                                 facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                                 xlab='year',ylab='Effective N',units='',lnscale=FALSE,
                                 title=f,
                                 colour='case',guideTitleColour='',
                                 shape='type',guideTitleShape='',
                                 linetype='type',guideTitleLineType='',
                                 showPlot=FALSE);
        if (showPlot||!is.null(pdf)) print(p);
        cap<-paste0("\n  \nFigure &&figno. Effective N's for ",f," ",category," catch size compositions.  \n  \n")
        plots[[f]]<-p;
    }#uF

    if (verbose) cat("rCompTCMs::compareFits.EffectiveNs: Done!\n");
    return(plots)
}

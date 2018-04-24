#'
#'@title Function to compare survey capture probability functions (Q_vyxmsz) by year among several models
#'
#'@description This function compares survey capture probability functions (Q_vyxmsz) by year
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param cast - formula to exclude factors from "averaging" over
#'@param years - vector of years to show, or 'all' to show all years
#'@param dodge - width to dodge overlapping series
#'@param mxy - max number of years per page
#'@param facet_wrap - ggplot2 formula to produce figure with wrapped facets
#'@param facet_grid - ggplot2 formula to produce figure with gridded facets
#'@param pdf - creates pdf, if not NULL
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Surveys.CaptureProbs<-function(objs,
                                         cast='y+x',
                                         years='all',
                                         dodge=0.2,
                                         mxy=15,
                                         facet_wrap=NULL,
                                         facet_grid="y~x",
                                         pdf=NULL,
                                         showPlot=FALSE,
                                         verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Surveys.CaptureProbs().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Surveys.CaptureProbs(obj,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (is.numeric(years)) mdfr <- mdfr[as.numeric(mdfr$y) %in% years,];

    #----------------------------------
    #capture probability functions
    #----------------------------------
    plots<-list();
    uF<-unique(mdfr$fleet);
    for (f in uF){
        if (verbose) cat("Plotting fleet",f,"\n")
        mdfrp<-mdfr[mdfr$fleet==f,];
        uY<-unique(mdfrp$y);
        mxpg<-ceiling(length(uY)/mxy);
        for (pg in 1:mxpg){
            mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
            p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                           xlab='size (mm CW)',ylab='capture probability',units='',lnscale=FALSE,
                           title=f,
                           colour='case',guideTitleColor='',
                           shape='case',guideTitleShape='',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            plots[[paste0("Capture probabilities for ",f,". ",pg," of ",mxpg,".")]]<-p;
        }#pg
    }#uF

    # for (f in uF){
    #     if (verbose) cat("Plotting fleet",f,"\n")
    #     mdfrp<-mdfr[mdfr$fleet==f,];
    #     uY<-unique(mdfrp$y);
    #     for (pg in 1:ceiling(length(uY)/mxy)){
    #         mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
    #         p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
    #                        facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
    #                        xlab='size (mm CW)',ylab='capture probability',units='',lnscale=FALSE,
    #                        title=f,
    #                        colour='case',guideTitleColor='',
    #                        shape='case',guideTitleShape='',
    #                        showPlot=FALSE);
    #         if (showPlot||!is.null(pdf)) print(p);
    #         plots[[paste0("Female:male ratio of capture probabilities for ",f,". ",pg," of ",mxpg,".")]]<-p;
    #     }#pg
    # }#uF

    if (verbose) cat("rCompTCMs::compareResults.Surveys.CaptureProbs(): Done!\n");
    return(plots)
}

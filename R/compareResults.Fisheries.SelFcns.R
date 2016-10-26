#'
#'@title Function to compare fishery selectivity functions by year among several models
#'
#'@description This function compares fishery selectivity functions by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param cast - formula to exclude factors from "averaging" over
#'@param dodge - width to dodge overlapping series
#'@param mxy - max number of years per page
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
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
compareResults.Fisheries.SelFcns<-function(objs,
                                         cast='x',
                                         dodge=0.2,
                                         mxy=15,
                                         showPlot=TRUE,
                                         pdf=NULL,
                                         verbose=TRUE){
    if (verbose) cat("rCompTCMs::compareResults.fisherys.SelFcns: Start.\n");
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
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL;#rTCSAM2013::getMDFR.Pop.Recruitment(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Fisheries.SelFcns(obj,cast=cast,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fisheries.SelFcns(obj,cast=cast,verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    #----------------------------------
    #selectivity functions
    #----------------------------------
    plots<-list();
    facet_wrap<-'~y'; 
    facet_grid<-NULL;
    if (!is.null(cast)&&(cast!='')) {
        mxy<-5;
        facet_wrap<-NULL;
        facet_grid<-paste0('y~',cast);
    }
    uF<-unique(mdfr$fleet);
    for (f in uF){
        if (verbose) cat("Plotting fleet",f,"\n")
        mdfrp<-mdfr[mdfr$fleet==f,];
        uY<-unique(mdfrp$y);
        for (pg in 1:ceiling(length(uY)/mxy)){
            mdfrpp<-mdfrp[mdfrp$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
            p<-plotMDFR.XY(mdfrpp,x='z',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=5,
                           xlab='size (mm CW)',ylab='Selectivity',units='',lnscale=FALSE,
                           title=f,
                           colour='case',guideTitleColor='',
                           shape='case',guideTitleShape='',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            plots[[paste(f,pg,sep=".")]]<-p;
        }#pg
    }#uF

    if (verbose) cat("rCompTCMs::compareResults.Fisheries.SelFcns: Done!\n");
    return(plots)
}

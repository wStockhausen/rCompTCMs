#'
#'@title Function to compare mature biomass estimates by year among several models
#'
#'@description This function compares mature biomass estimates by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param numRecent - number of "recent" years to plot
#'@param dodge - width to dodge overlapping series
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details uses \code{rTCSAM2013::getMDFR.PopQuantities}, 
#'\code{rsimTCSAM::getMDFR.Pop.Quantities}, \code{rsimTCSAM::getMDFR.Pop.Quantities}, and 
#'\code{plotMDFR.XY}.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.MatureBiomass<-function(objs,
                                           numRecent=15,
                                           dodge=0.2,
                                           showPlot=TRUE,
                                           pdf=NULL,
                                           verbose=TRUE){
    if (verbose) cat("rCompTCMs::compareResults.Pop.MatureBiomass: Plotting mature biomass.\n");
    
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
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.PopQuantities(obj,type="MB_yx",verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type="MB_yx",verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type="MB_yx",verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    idx<-mdfr$y>=(max(mdfr$y)-numRecent);
    
    #----------------------------------
    #mature biomass
    #----------------------------------
    plots<-list();
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=FALSE,
                   facet_grid='x~.',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$MB_yx<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=FALSE,
                   facet_grid='x~.',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$RMB_yx<-p;
    
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=TRUE,
                   facet_grid='x~.',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$lnMB_yx<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=TRUE,
                   facet_grid='x~.',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$lnRMB_yx<-p;
    
    if (verbose) cat("rCompTCMs::compareResults.Pop.MatureBiomass: Done!\n");
    return(plots)
}

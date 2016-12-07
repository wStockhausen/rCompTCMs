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
#'@details uses \code{rTCSAM2013::getMDFR.Pop.Quantities}, 
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
    if (verbose) cat("Starting rCompTCMs::compareResults.Pop.MatureBiomass().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    mdfr<-extractResults.Pop.MatureBiomass(objs,verbose=verbose);
    
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
    cap1<-"  \n  \nFigure &&figno. Estimated annual mature biomass.  \n  \n";
    plots[[cap1]]<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=FALSE,
                   facet_grid='x~.',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated recent mature biomass.  \n  \n";
    plots[[cap1]]<-p;
    
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=TRUE,
                   facet_grid='x~.',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated annual mature biomass, on ln-scale.  \n  \n";
    plots[[cap1]]<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=TRUE,
                   facet_grid='x~.',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated recent mature biomass, on ln-scale.  \n  \n";
    plots[[cap1]]<-p;
    
    if (verbose) cat("Finished rCompTCMs::compareResults.Pop.MatureBiomass()!\n");
    return(plots)
}

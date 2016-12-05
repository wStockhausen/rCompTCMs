#'
#'@title Function to compare recruitment estimates by year among several models
#'
#'@description This function compares recruitment estimates by year
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
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Recruitment<-function(objs,
                                         numRecent=15,
                                          dodge=0.2,
                                          showPlot=TRUE,
                                          pdf=NULL,
                                          verbose=TRUE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Pop.Recruitment().\n");
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
        if (inherits(obj,"tcsam2013.resLst")) {
            mdfr1<-rTCSAM2013::getMDFR.Pop.Recruitment(obj,verbose);
            mdfr1$y<-as.numeric(mdfr1$y);
            mdfr1$y<-mdfr1$y-1;#adjust to TCSAM02 sense for timing of recruitment
        }
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type="R_y",verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type="R_y",verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y)
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    idx<-mdfr$y>=(max(mdfr$y)-numRecent);
    
    #----------------------------------
    #recruitment
    #----------------------------------
    plots<-list();
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                   dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$R_y<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                   dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$RR_y<-p;
    
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$lnR_y<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$lnRR_y<-p;

    if (verbose) cat("rCompTCMs::compareResults.Pop.Recruitment: Done!\n");
    return(plots)
}

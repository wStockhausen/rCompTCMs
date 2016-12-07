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

    mdfr<-extractMDFR.Pop.Recruitment(objs,verbose=verbose);
    
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
    cap1<-"  \n  \nFigure &&figno. Estimated annual recruitment.  \n  \n";
    plots[[cap1]]<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                   dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated recent recruitment.  \n  \n";
    plots[[cap1]]<-p;
    
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated annual recruitment, on ln-scale.  \n  \n";
    plots[[cap1]]<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    cap1<-"  \n  \nFigure &&figno. Estimated recent recruitment, on ln-scale.  \n  \n";
    plots[[cap1]]<-p;

    if (verbose) cat("rCompTCMs::compareResults.Pop.Recruitment: Done!\n");
    return(plots)
}

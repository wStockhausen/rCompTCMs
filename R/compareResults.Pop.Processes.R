#'
#'@title Compare population processes from TCSAM2013 and TCSAM02 model runs
#'
#'@description Function to compare population processes from TCSAM2013 and TCSAM02 model runs.
#'
#'@param objs - list of resLst objects
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print debug info
#'
#'@return list of ggplot2 objects.
#'
#'@details Uses \code{wtsUtilities::printGGList()}. List names are captions.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Processes<-function(objs,
                                       showPlot=TRUE,
                                       pdf=NULL,
                                       width=8,
                                       height=6,
                                       verbose=FALSE){
  
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    cases<-names(objs);
    
    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    plots<-list();#output list
    figno<-1;
    
    #-------------------------------------------#
    #plot natural mortality
    #-------------------------------------------#
    p<-compareResults.Pop.NaturalMortality(objs,dodge=0.2,showPlot=FALSE,verbose=verbose);
    cap<-"  \n  \nFigure &&fno.Estimated natural mortality rates.  \n  \n";
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot prM2M
    #-------------------------------------------#
    p<-compareResults.Pop.PrM2M(objs,dodge=0.5,showPlot=FALSE,verbose=verbose);
    cap<-"  \n  \nFigure &&fno. Estimated probabilities of molt-to-maturity at size (mm CW).  \n  \n";
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot mean growth
    #-------------------------------------------#
    p<-compareResults.Pop.MeanGrowth(objs,dodge=0.5,showPlot=FALSE,verbose=verbose);
    cap<-"  \n  \nFigure &&fno. Estimated mean growth patterns.  \n  \n";
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot growth transition matrices
    #-------------------------------------------#
    p<-compareResults.Pop.GrowthMatrices(objs,showPlot=FALSE,verbose=verbose);
    cap<-paste0("  \n  \nFigure &&fno. Estimated growth transition matrices.  \n  \n");
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;

    #-------------------------------------------#
    #plot recruitment size distribution
    #-------------------------------------------#
    p<-compareResults.Pop.RecSizeDistribution(objs,dodge=0.5,showPlot=FALSE,verbose=verbose);
    cap<-paste0("  \n  \nFigure &&fno. Estimated/assumed size distribution at recruitment to the model.  \n  \n");
    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
    plots[[cap]]<-p; p<-NULL;
    
    return(invisible(plots));
}

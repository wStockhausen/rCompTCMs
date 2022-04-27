#'
#'@title Compare effort time series data by fleet among several model scenarios
#'
#'@description Function to compare effort time series data by fleet among
#'several model scenarios.
#'
#'@param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param scales - ggplot2 scales option for facet_grid
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM02::getMDFR.Data.EffortData()}.
#'
#'@return Non-nested list of ggplot2 objects, with captions as names
#'
#'@import ggplot2
#'
#'@export
#'
compareData.EffortTimeSeries<-function(objs=NULL,
                                       scales="free_y",
                                       verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareData.EffortTimeSeries().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) message("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Data.EffortData(obj,
                                                                                      verbose=verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();

    #----------------------------------
    # plot fits to biomass time series
    #----------------------------------
    if (verbose) message("Plotting",nrow(mdfr),"rows.\n")
    ylab<-"Effort (1000's potlifts)";
    cap1<-"  \n  \nFigure &&fno. Comparison of effort data among fleets.  \n  \n";

    p <- ggplot(mdfr,aes_string(x='y',y='val',colour='case',shape='case'))
    p <- p + geom_line();
    p <- p + geom_point();
    p <- p + facet_grid('fleet~.',scales=scales);
    p <- p + labs(x="year",y=ylab);

    plots[[cap1]]<-p;

    if (verbose) message("Finished rCompTCMs::compareData.EffortTimeSeries().\n");
    return(plots);
}

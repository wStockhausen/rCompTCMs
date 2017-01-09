#'
#'@title Function to plot survey catchabilities by year using ggplot2
#'
#'@description This function plots survey catchability estimates by year,
#'   sex and maturity state.
#'   
#'@param objs - list of resLst objects
#'@param years - vector of years to show, or 'all' to show all years
#'@param cast - formula to exclude factors from "averaging" over
#'@param dodge - width to dodge overlapping series
#'@param facet_grid - ggplot2 formula to produce figure with gridded facets
#'@param scales - parameter passed to ggplot2::facet_grid()
#'@param pdf - creates pdf, if not NULL
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return list of ggplot2 objects
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Surveys.Catchability<-function(objs,
                                              years='all',
                                              cast='x',
                                              dodge=0.2,
                                              facet_grid="x~.",
                                              scales="free_y",
                                              pdf=NULL,
                                              showPlot=FALSE,
                                              verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Surveys.Catchability().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    mdfr<-extractMDFR.Surveys.Catchability(objs,years=years,cast=cast,verbose=verbose);
    
    #----------------------------------
    # plot survey catchability by year
    #----------------------------------
    uF<-unique(mdfr$fleet);
    plots<-list();
    pd<-position_dodge(width=dodge);
    for (f in uF){
        if (verbose) cat("Plotting fleet",f,"\n")
        mdfrp<-mdfr[mdfr$fleet==f,];
        p <- ggplot(mdfrp,aes_string(x='y',y='val',colour='case'));
        p <- p + geom_line(position=pd);
        p <- p + geom_point(position=pd);
        if (any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
        p <- p + labs(x='year',y="survey catchability");
        p <- p + ggtitle(f);
        p <- p + facet_grid(facet_grid,scales=scales);
        p <- p + ylim(c(0,NA));
        if (showPlot) print(p);
        cap<-paste0("\n  \nFigure &&figno. Survey catchabilities for ",f,".\n   \n");
        plots[[cap]]<-p;
    }
    # p <- ggplot(mdfr,aes_string(x='y',y='val',colour='case'));
    # p <- p + geom_line(position=pd);
    # p <- p + geom_point(position=pd);
    # if (any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
    # p <- p + labs(x='year',y="survey catchability");
    # p <- p + facet_grid(facet_grid);
    # p <- p + ylim(c(0,max(1.0,1.05*max(mdfr$val,mdfr$uci,rm.na=TRUE))));
    # if (showPlot) print(p);
    # cap<-paste0("\n  \nFig. &&figno. Survey catchabilities.\n   \n");
    # plots[[cap]]<-p;

    return(plots)
}

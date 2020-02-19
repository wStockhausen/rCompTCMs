#'
#'@title Function to plot natural mortality rates by year using ggplot2
#'
#'@description This function plots natural mortality estimates by year,
#'   sex and maturity state.
#'
#'@param objs - list of resLst objects
#'@param dodge - width to dodge overlapping series
#'@param pdf - creates pdf, if not NULL
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object as list element
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.NaturalMortality<-function(objs,
                                              dodge=0.2,
                                              pdf=NULL,
                                              showPlot=FALSE,
                                              verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    mdfr<-extractMDFR.Pop.NaturalMortality(objs,verbose=verbose);

    #----------------------------------
    # plot natural mortality rates by year
    #----------------------------------
    pd<-position_dodge(width=dodge);
    p <- ggplot(mdfr,aes_string(x='y',y='val',colour='case'));
    p <- p + geom_line(position=pd);
    p <- p + geom_point(position=pd);
    if (any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
    p <- p + geom_abline(intercept=0.23,slope=0,linetype=2,colour='black')
    p <- p + labs(x='year',y="natural mortality");
    p <- p + ggtitle("Natural Mortality");
    p <- p + facet_grid(m+s~x);
    p <- p + ylim(c(0,NA));
    if (showPlot) print(p);

    plots<-list();
    cap1<-"  \n  \nFigure &&figno. Estimated natural mortality rates, by year.  \n  \n";
    plots[[cap1]]<-p;
    return(plots);
}

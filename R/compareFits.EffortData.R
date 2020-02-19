#'
#'@title Compare fits to effort data among several model runs
#'
#'@description Function to compare fits to effort data among several model runs.
#'
#'@param objs - list of resLst objects
#'@param dodge - width to dodge overlapping series
#'@param pdf - name for output pdf file
#'@param showPlot - flag to print plot to current device
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details None.
#'
#'@return ggplot object
#'
#'@import ggplot2
#'
#'@export
#'
compareFits.EffortData<-function(objs,
                                 dodge=0.2,
                                 pdf=NULL,
                                 showPlot=FALSE,
                                 verbose=FALSE){
    options(stringsAsFactors=FALSE);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        mdfr1<-NULL;
        obj<-objs[[case]];
        #if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.MeanGrowth(obj,verbose);
        #if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.MeanGrowth(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.EffortData(obj,verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }

    if (is.null(mdfr)) {
        cat("\n \nNo fits to effort data.\n \n")
        return(NULL);
    }

    mdfr$case<-factor(mdfr$case,levels=cases);

    pd<-position_dodge(width=dodge);
    pcs<-unique(mdfr$pc);
    plots<-list();
    for (pc in pcs){
        mdfrp<-mdfr[(mdfr$pc==pc),];
        fsh<-unique(mdfrp$fleet);
        #-------------------------------------------#
        #plot effort data and fits
        #-------------------------------------------#
        mdfrpo<-mdfrp[mdfrp$type == 'observed', ];
        mdfrpp<-mdfrp[mdfrp$type == 'predicted',];
        mdfrpo$y<-as.numeric(mdfrpo$y);
        mdfrpp$y<-as.numeric(mdfrpp$y);
        p <- ggplot(mdfrp,aes_string(x='y',y='val',colour='case'));
        p <- p + geom_point(data=mdfrpo,position=pd);
        p <- p + geom_line(data=mdfrpp,position=pd);
        if (any(!is.na(mdfrpo$lci))) p <- p + geom_errorbar(data=mdfrpo,aes_string(ymin='lci',ymax='uci'),position=pd);
        p <- p + labs(x='year',y="effort");
        p <- p + ggtitle(fsh);
        p <- p + facet_grid(x+m+s~.);
        if (showPlot) print(p);
        cap<-paste0("\n  \nFigure &&figno. Model fits to effort in the ",fsh,".\n   \n")
        plots[[cap]]<-p;
        #-------------------------------------------#
        #plot zscores
        #-------------------------------------------#
        mdfrpz<-mdfrp[mdfrp$type == 'zscores',];
        p <- ggplot(mdfrpz,aes_string(x='y',y='val',colour='case'));
        p <- p + geom_point(position=pd);
        p <- p + geom_abline(slope=0,linetype=2);
        p <- p + labs(x='year',y="z-scores");
        p <- p + ggtitle(fsh);
        p <- p + facet_grid(x+m+s~.);
        if (showPlot) print(p);
        cap<-paste0("Figure &&figno. \n  \nZ-scores for fits to effort in the ",fsh,".\n   \n")
        plots[[cap]]<-p;
    }#d

    #-------------------------------------------#
    #plot nll scores
    #-------------------------------------------#
    mdfrpn<-mdfr[mdfr$type == 'nlls',];
    p <- ggplot(mdfrpn,aes_string(x='fleet',y='val',fill='case'));
    p <- p + geom_col(position="dodge");
    p <- p + geom_abline(slope=0,linetype=2);
    p <- p + labs(x='fishery',y="NLLs");
    p <- p + facet_grid(x+m+s~.);
    if (showPlot) print(p);
    cap<-paste0("\n  \nFigure &&figno. Negative log-likelihood values for fits to effort in the ",fsh,".\n   \n")
    plots[[cap]]<-p;

    return(plots);
}

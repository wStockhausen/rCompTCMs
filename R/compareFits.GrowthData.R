#'
#'@title Compare fits to growth data among several model runs
#'
#'@description Function to compare fits to growth data among several model runs.
#'
#'@param objs - list of resLst objects
#'@param dodge - width to dodge overlapping series
#'@param plot1stObs - flag to include observations only from 1st model scenario
#'@param pdf - name for output pdf file
#'@param showPlot - flag to print plot to current device
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details None.
#'
#'@return ggplot object
#'
#'@import ggplot2
#'@import stringr
#'@import wtsPlots
#'
#'@export
#'
compareFits.GrowthData<-function(objs,
                                 dodge=0.2,
                                 plot1stObs=FALSE,
                                 pdf=NULL,
                                 showPlot=FALSE,
                                 verbose=FALSE){
    options(stringsAsFactors=FALSE);

    std_theme = wtsPlots::getStdTheme();

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
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.GrowthData(obj,verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }

    if (is.null(mdfr)) {
        cat("\n \nNo fits to growth data.\n \n")
        return(NULL);
    }

    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.character(mdfr$y);

    pd<-position_dodge(width=dodge);
    datasets<-unique(mdfr$category);
    plots<-list();
    for (d in datasets){
        mdfrp<-mdfr[(mdfr$category==d),];
        dp = stringr::str_replace_all(d,stringr::fixed("_")," ");
        if (nrow(mdfrp)>0){
            dcs<-unique(mdfrp$case);
            #-------------------------------------------#
            #plot growth data and fits
            #-------------------------------------------#
            if (plot1stObs) {
                mdfrpo<-mdfrp[(mdfrp$type == 'observed')&(mdfrp$case==dcs[1]), ];
            } else {
                mdfrpo<-mdfrp[mdfrp$type == 'observed', ];
            }
            mdfrpp<-mdfrp[mdfrp$type == 'predicted',];
            p <- ggplot(mdfrp,aes_string(x='z',y='val',colour='case',shape='case'));
            p <- p + geom_point(data=mdfrpo,position=pd);
            p <- p + geom_line(data=mdfrpp,position=pd);
            if (any(is.finite(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
            p <- p + geom_abline(slope=1,linetype=2);
            p <- p + labs(x='pre-molt size (mm CW)',y="post-molt size (mm CW)");
            p <- p + ggtitle(dp);
            p <- p + facet_grid(x~.);
            if (showPlot) print(p);
            cap<-paste0("\n  \nFigure &&figno. Model fits to ",dp,".\n   \n")
            plots[[cap]]<-p + std_theme;
            #-------------------------------------------#
            #plot nll scores
            #-------------------------------------------#
            mdfrpn<-mdfrp[mdfrp$type == 'nlls',];
            p <- ggplot(mdfrpn,aes_string(x='z',y='val',colour='case',shape='case'));
            p <- p + geom_point(position=pd);
            p <- p + geom_abline(slope=0,linetype=2);
            p <- p + labs(x='pre-molt size (mm CW)',y="NLLs");
            p <- p + ggtitle(dp);
            p <- p + facet_grid(x~.);
            if (showPlot) print(p);
            cap<-paste0("\n  \nFigure &&figno. Negative log-likelihood values for fits to ",dp,".\n   \n")
            plots[[cap]]<-p + std_theme;
            #-------------------------------------------#
            #plot zscores
            #-------------------------------------------#
            mdfrpz<-mdfrp[mdfrp$type == 'zscores',];
            p <- ggplot(mdfrpz,aes_string(x='z',y='val',colour='case',shape='case'));
            p <- p + geom_smooth(mapping=aes(group=case,fill=case,colour=case),alpha=0.5);
            p <- p + geom_point(position=pd);
            p <- p + geom_abline(slope=0,linetype=2);
            p <- p + labs(x='pre-molt size (mm CW)',y="z-scores");
            p <- p + ggtitle(dp);
            p <- p + facet_grid(x~.);
            if (showPlot) print(p);
            cap<-paste0("\n  \nFigure &&figno. Z-scores for fits to ",dp,".\n   \n")
            plots[[cap]]<-p + std_theme;
        }
    }#d

    return(plots);
}

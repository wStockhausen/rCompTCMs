#'
#'@title Compare Pearson's residuals or nll residuals from size comps by fleet among several model runs
#'
#'@description Function to compare Pearson's residuals or nll residuals from size comps by fleet among
#'several model runs.
#'
#'@param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param mdfr - dataframe from call to \code{extractFits.ZScores.PrNatZ}  (as alternative to objs)
#'@param fleets - names of fleets to include (or "all" or NULL to include all)
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained',or 'total')
#'@param residuals.type - residual type for tcsam02 models ('pearsons' or 'nlls')
#'@param tcsam2013.type - pearsons residuals type for tcsam2013 models ("PRs_yxmz","PRs_yxz")
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - name for output pdf file
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, \code{rTCSAM02::getMDFR.ZScores.PrNatZ()}.
#'Also uses \code{wtsUtilities::printGGList}.
#'
#'@return non-nested list of ggplot2 objects, with captions as names
#'
#'@import ggplot2
#'
#'@export
#'
compareFits.ZScores.PrNatZ<-function(objs=NULL,
                                     mdfr=NULL,
                                     fleets="all",
                                     fleet.type=c('survey','fishery'),
                                     catch.type=c('index','retained','total'),
                                     residuals.type=c('pearsons','nlls'),
                                     tcsam2013.type=c("PRs_yxmz","PRs_yxz"),
                                     showPlot=FALSE,
                                     pdf=NULL,
                                     verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::compareFits.ZScores.PrNatZ().\n");
    options(stringsAsFactors=FALSE);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (is.null(mdfr)){
        mdfr<-extractFits.ZScores.PrNatZ(objs=objs,
                                         fleets=fleets,
                                         fleet.type=fleet.type,
                                         catch.type=catch.type,
                                         residuals.type=residuals.type,
                                         tcsam2013.type=tcsam2013.type,
                                         verbose=verbose);
    }

    cases<-unique(mdfr$case);

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;

    #----------------------------------
    # plot size comp residuals by fleet
    #----------------------------------
    uFs<-unique(mdfr$fleet);
    for (uF in uFs){
        if (verbose) cat("Plotting residuals for",uF,"\n");
        mdfrp0<-mdfr[mdfr$fleet==uF,];
        uXs<-unique(mdfrp0$x);
        for (uX in uXs){
            mdfrp<-mdfrp0[mdfrp0$x==uX,];
            mx<-max(mdfrp$val,na.rm=TRUE);
            for (case in cases){
                mdfrpp<-mdfrp[mdfrp$case==case,];
                if (nrow(mdfrpp)>0){
                    p <- ggplot(data=mdfrpp,mapping=aes_string(x='y',y='z',size='val',fill='sign'));
                    p <- p + scale_size_area(max_size=10,limits=c(0,mx));
                    p <- p + geom_point(alpha=0.8,shape=21,color='black');
                    p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
                    p <- p + labs(y="size (mm CW)",x="year") + ggtitle(uF);
                    p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                                    size=guide_legend(order=1))+theme(legend.box="vertical");
                    if (length(cases)==1){
                        p <- p + facet_grid(x+m+s~.);
                    } else {
                        p <- p + facet_grid(x+m+s~case);
                    }
                    p <- p + theme(legend.box='horizontal')
                    if (residuals.type=='pearsons')
                        cap<-paste0("  \n  \nFigure &&fno. Pearson's residuals for ",uX," proportions-at-size from the ",uF," for scenario ",case,".  \n  \n");
                    if (residuals.type=='nlls')
                        cap<-paste0("  \n  \nFigure &&fno. NLL residuals for ",uX," proportions-at-size from the ",uF," for scenario ",case,".  \n  \n");
                    if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
                    plots[[cap]]<-p; p<-NULL;
                } #--case
            }#--uX
        }#--uF
    }

    if (verbose) cat("Finished rCompTCMs::compareFits.ZScores.PrNatZ().\n");
    return(plots);
}

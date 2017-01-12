#'
#'@title Compare Pearson's residuals or nll residuals from size comps by fleet among several model runs
#'
#'@description Function to compare Pearson's residuals or nll residuals from size comps by fleet among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param tcsam2013.type - pearsons residuals type for tcsam2013 models ("PRs_yxmz","PRs_yxz")
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained',or 'total')
#'@param residuals.type - residual type for tcsam02 models ('pearsons' or 'nlls')
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
                                     fleet.type=c('survey','fishery'),
                                     catch.type=c('index','retained','total'),
                                     residuals.type=c('pearsons','nlls'),
                                     tcsam2013.type=c("PRs_yxmz","PRs_yxz"),
                                     showPlot=FALSE,
                                     pdf=NULL,
                                     verbose=FALSE){
    
    if (verbose) cat("Starting rCompTCMs::compareFits.ZScores.PrNatZ().\n");
    options(stringsAsFactors=FALSE);
    
    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];
    residuals.type<-residuals.type[1];
    tcsam2013.type<-tcsam2013.type[1];
    
    if (fleet.type=='survey') catch.type<-'index';
    if ((fleet.type=='fishery')&&(catch.type=='retained')) tcsam2013.type<-'PRs.ret';
    if ((fleet.type=='fishery')&&(catch.type=='total'))    tcsam2013.type<-'PRs.tot';
    
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
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (fleet.type=='survey'){
            if (inherits(obj,"tcsam2013.resLst")) 
                mdfr1<-rTCSAM2013::getMDFR.SurveyQuantities(obj,
                                                            type=tcsam2013.type,
                                                            verbose=verbose);
            if (inherits(obj,"tcsam02.resLst"))   
                mdfr1<-rTCSAM02::getMDFR.ZScores.PrNatZ(obj,
                                                        fleet.type=fleet.type,
                                                        catch.type='index',
                                                        residuals.type=residuals.type,
                                                        verbose=verbose);
        }
        if (fleet.type=='fishery'){
            if (inherits(obj,"tcsam2013.resLst")) 
                mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,
                                                             type=tcsam2013.type,
                                                             verbose=verbose);
            if (inherits(obj,"tcsam02.resLst"))   
                mdfr1<-rTCSAM02::getMDFR.ZScores.PrNatZ(obj,
                                                        fleet.type=fleet.type,
                                                        catch.type=catch.type,
                                                        residuals.type=residuals.type,
                                                        verbose=verbose);
        }
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$z<-as.numeric(mdfr$z);
    
    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;
    
    #----------------------------------
    # plot size comp residuals from the survey 
    #----------------------------------
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$sign<-ifelse(test=mdfr$val>0,yes=">0",no="<0");
    mdfr$val <- abs(mdfr$val);
    
    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';

    uFs<-unique(mdfr$fleet);
    for (uF in uFs){
        if (verbose) cat("Plotting residuals for",uF,"\n");
        mdfrp<-mdfr[mdfr$fleet==uF,];
        p <- ggplot(data=mdfrp,mapping=aes_string(x='y',y='z',size='val',fill='sign'));
        p <- p + scale_size_area(max_size=10);
        p <- p + geom_point(alpha=0.8,shape=21,color='black');
        p <- p + geom_point(alpha=1.0,shape=21,color='black',fill=NA);
        p <- p + labs(y="size (mm CW)",x="year") + ggtitle(uF);
        p <- p + guides(fill=guide_legend(override.aes=list(alpha=1.0,size=6),order=2),
                        size=guide_legend(order=1));
        if (length(cases)==1){
            p <- p + facet_grid(x~m+s);
        } else {
            p <- p + facet_grid(case~x+m+s);
        }
        p <- p + theme(legend.box='horizontal')
        if (residuals.type=='pearsons') 
            cap<-paste0("  \n  \nFigure &&fno. Pearson's residuals for proportions-at-size from the ",uF,".  \n  \n");
        if (residuals.type=='nlls') 
            cap<-paste0("  \n  \nFigure &&fno. NLL residuals for proportions-at-size from the ",uF,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p; p<-NULL;
    }
    
    if (verbose) cat("Finished rCompTCMs::compareFits.ZScores.PrNatZ().\n");
    return(plots);
}

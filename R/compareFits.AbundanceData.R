#'
#'@title Compare fits to abundance time series by fleet among several model runs
#'
#'@description Function to compare fits to abundance time series by fleet among
#'several model runs.
#'
#'@param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param fleets - names of fleets to plot (or "all")
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained',  or 'total')
#'@param ci - confidence interval for plots
#'@param fishery.pdfType - choice for fishery CIs ("norm2","normal","lognormal")
#'@param numRecent - number of recent years to include in "recent years" plots
#'@param plot1stObs - flag (T/F) to plot observations only from the first model
#'@param scales - ggplot2 scales option for facet_grid
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param pdf - name for output pdf file
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, \code{rTCSAM02::getMDFR.Fits.FleetData()}.
#'Also uses \code{wtsUtilities::printGGList}.
#'
#'@return Non-nested list of ggplot2 objects, with captions as names, or NULL.
#'
#'@import ggplot2
#'
#'@export
#'
compareFits.AbundanceData<-function(objs=NULL,
                                    fleets="all",
                                    fleet.type=c('survey','fishery'),
                                    catch.type=c('index','retained','discard','total'),
                                    ci=0.80,
                                    fishery.pdfType=c("norm2","normal","lognormal"),
                                    numRecent=15,
                                    plot1stObs=TRUE,
                                    scales="free_y",
                                    position=position_dodge(0.3),
                                    pdf=NULL,
                                    showPlot=FALSE,
                                    verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareFits.AbundanceData().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (catch.type=='retained') type<-'bio.retm';
    if (catch.type=='discard')  type<-'bio.dscm';
    if (catch.type=='total')    type<-'bio.totm';

    mdfr<-NULL;
    if (is.data.frame(objs)) {
        mdfr<-objs;
    } else {
        mdfr<-extractMDFR.Fits.AbundanceData(objs=objs,
                                             fleets=fleets,
                                             fleet.type=fleet.type,
                                             catch.type=catch.type,
                                             ci=ci,
                                             fishery.pdfType=fishery.pdfType,
                                             verbose=verbose);
    }

    plots<-NULL;
    if (!is.null(mdfr)){
        mdfr$facets<-paste0(mdfr$x,"\n",mdfr$m,"\n",mdfr$s)

        #----------------------------------
        # define output list of plots
        #----------------------------------
        plots<-list();
        figno<-1;

        #----------------------------------
        # plot fits to abundance time series
        #----------------------------------
        if (verbose) message("Plotting ",nrow(mdfr)," rows.\n")
        ylab<-""; cap1<-"1"; cap2<-"2";
        if ((catch.type=="index")&&(fleet.type=="survey")) {
            ylab<-"Survey abundance (millions)";
            cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex survey abundance for &&fleet.  \n  \n";
            cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex survey abundance for &&fleet. Observed time period.  \n  \n";
            cap3<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex survey abundance for &&fleet. Recent time period.  \n  \n";
        }
        if ((catch.type=="index")&&(fleet.type=="fishery")) {
            ylab<-"Fishery CPUE";
            cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex index catch abundance (CPUE) for &&fleet.  \n  \n";
            cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex index catch abundance (CPUE) for &&fleet. Observed time period.  \n  \n";
            cap3<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex index catch abundance (CPUE) for &&fleet. Recent time period.  \n  \n";
        }
        if (catch.type=="retained") {
            ylab<-"Retained catch (millions)";
            cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex retained catch abundance for &&fleet.  \n  \n";
            cap2<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex retained catch abundance for &&fleet. Recent time period.  \n  \n";
            cap3<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&sex retained catch abundance for &&fleet. Observed time period.  \n  \n";
        }
        if (catch.type=="total") {
            ylab<-"Total catch (millions)";
            cap1<-"  \n  \nFigure &&figno. Comparison of observed and predicted &&sex total catch abundance for &&fleet.  \n  \n";
            cap2<-"  \n  \nFigure &&figno. Comparison of observed and predicted &&sex total catch abundance for &&fleet. Observed time period.  \n  \n";
            cap3<-"  \n  \nFigure &&figno. Comparison of observed and predicted &&sex total catch abundance for &&fleet. Recent time period.  \n  \n";
        }
        uFs<-as.character(unique(mdfr$fleet));
        for (uF in uFs){
            mdfr0<-mdfr[mdfr$fleet==uF,];
            uXs<-as.character(unique(mdfr0$x));
            for (uX in uXs){
                mdfr1<-mdfr0[mdfr0$x==uX,];
                if (nrow(mdfr1)>0){
                    if (verbose) message("uF = ",uF," uX = ",uX," nrow = ",nrow(mdfr1))
                    ps<-plotMDFR.Fits.TimeSeries(mdfr1,
                                                 numRecent=numRecent,
                                                 plot1stObs=plot1stObs,
                                                 facets='facets~.',
                                                 scales=scales,
                                                 position=position,
                                                 plotObs=TRUE,
                                                 plotMod=TRUE,
                                                 xlab='year',
                                                 ylab=ylab,
                                                 title=uF,
                                                 xlims=NULL,
                                                 ylims=NULL,
                                                 showPlot=showPlot,
                                                 verbose=verbose);
                    cp1<-gsub("&&fleet",uF,cap1,fixed=TRUE); cp1<-gsub("&&sex",uX,cp1,fixed=TRUE);
                    cp2<-gsub("&&fleet",uF,cap2,fixed=TRUE); cp2<-gsub("&&sex",uX,cp2,fixed=TRUE);
                    cp3<-gsub("&&fleet",uF,cap3,fixed=TRUE); cp3<-gsub("&&sex",uX,cp3,fixed=TRUE);
                    names(ps)<-c(cp1,cp2,cp3);
                    if (showPlot) figno<-(wtsUtilities::printGGList(ps,figno=figno))$figno;
                    plots[[cp1]]<-ps[[1]];
                    plots[[cp2]]<-ps[[2]];
                    plots[[cp3]]<-ps[[3]];
                }
            }#uXs
        }#uFs
    }

    if (verbose) message("Finished rCompTCMs::compareFits.AbundanceData().\n");
    return(plots);
}

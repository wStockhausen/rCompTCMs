#'
#'@title Compare z-scores from fitting abundance time series by fleet among several model runs
#'
#'@description Function to compare z-scores from fitting abundance time series by fleet among
#'several model runs.
#'
#'@param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param fleets - names of fleets to include (or "all")
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained', 'discard', or 'total')
#'@param pdf - name for output pdf file
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, [rTCSAM02::getMDFR.ZScores.Abundance()].
#'Also uses [wtsUtilities::printGGList()].
#'
#'@return non-nested list of ggplot2 objects, with captions as names
#'
#'@import dplyr
#'@import ggplot2
#'@import magrittr
#'
#'@md
#'
#'@export
#'
compareFits.ZScores.Abundance<-function(objs=NULL,
                                        fleets="all",
                                        fleet.type=c('survey','fishery'),
                                        catch.type=c('index','retained','discard','total'),
                                        pdf=NULL,
                                        showPlot=FALSE,
                                        verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareFits.ZScores.Abundance().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (catch.type=='retained') type<-'zscores.ret';
    if (catch.type=='discard')  type<-'zscores.dsc';
    if (catch.type=='total')    type<-'zscores.tot';

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) message("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))
            mdfr1<-rTCSAM02::getMDFR.ZScores.Abundance(obj,
                                                       fleet.type=fleet.type,
                                                       catch.type=catch.type,
                                                       verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;
    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));

    #----------------------------------
    # drop factor combinations with zeros for all years
    #----------------------------------
    tmp = mdfr %>% dplyr::group_by(case,category,process,fleet,type,pc,x,m,s,z) %>%
                   dplyr::summarize(tot=sum(abs(val),na.rm=TRUE)) %>%
                   dplyr::ungroup() %>%
                   dplyr::filter(tot>0);
    mdfr %<>% dplyr::inner_join(tmp,by=c("case","category","process","fleet","type","pc","x","m","s","z"));

    #----------------------------------
    # plot zscores from fits to time series
    #----------------------------------
    uFs<-unique(mdfr$fleet);
    #print(uFs);
    for (uF in uFs){
        #--testing uF = uFs[1];
        idx   <- (mdfr$fleet==uF);
        dfrpp <- mdfr[idx,];
        xmax  <- max(dfrpp$y,na.rm=TRUE);
        p<-plotZScores(dfrpp,x='y',y='val',
                       color='case',shape='case',legend='case',
                       facets="x~fleet",facet.scales='free_y',position='dodge',
                       ylab='z-score',title=uF,
                       showPlot=showPlot);
        cap<-paste0("  \n  \nFigure &&fno. Z-scores for ",catch.type," catch abundance in ",uF,".  \n  \n");
        if (showPlot) figno<-(wtsUtilities::printGGList(p,figno=figno,cap=cap))$figno;
        plots[[cap]]<-p+std_theme; p<-NULL;
    }

    if (verbose) message("Finished rCompTCMs::compareFits.ZScores.Abundance().\n");
    return(plots);
}

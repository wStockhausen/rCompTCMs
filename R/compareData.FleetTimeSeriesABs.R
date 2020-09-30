#'
#'@title Compare time series of abundance or biommass data by fleet among several model scenarios
#'
#'@description Function to compare abundance or biomass time series data by fleet among
#'several model scenarios.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained', 'discard' or 'total')
#'@param data.type - data type ('abundance' or 'biomass')
#'@param fleets - vector of names of fleets to plot (or 'all')
#'@param sexs - vector of sexes to plot (or 'all')
#'@param maturity_states - vector of maturity states to plot (or 'all')
#'@param shell_conditions - vector of shell conditions to plot (or 'all')
#'@param ci - confidence interval for plots
#'@param numRecent - number of years for 'recent' plot
#'@param ylims - limits for y axis (default is NULL)
#'@param facets - grid faceting formula
#'@param scales - ggplot2 scales option for facet_grid
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM02::getMDFR.Data.FleetTimeSeries()}.
#'
#'@return Non-nested list of ggplot2 objects, with captions as names
#'
#'@import ggplot2
#'
#'@export
#'
compareData.FleetTimeSeriesABs<-function(objs=NULL,
                                      fleet.type=c('survey','fishery'),
                                      catch.type=c('index','retained','discard','total'),
                                      data.type=c('abundance','biomass'),
                                      fleets="all",
                                      sexs="all",
                                      maturity_states="all",
                                      shell_conditions="all",
                                      ci=0.80,
                                      numRecent=15,
                                      ylims=NULL,
                                      facets="fleet~x",
                                      scales="free_y",
                                      verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::compareData.FleetTimeSeries().\n");
    options(stringsAsFactors=FALSE);

    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))
            mdfr1<-rTCSAM02::getMDFR.Data.FleetTimeSeries(obj,
                                                          fleet.type=fleet.type,
                                                          data.type=data.type,
                                                          catch.type=catch.type,
                                                          ci=ci,
                                                          verbose=verbose);
        if (!is.null(mdfr1)){
            if ((!is.null(fleets))&&tolower(fleets[1])!="all") mdfr1<-mdfr1[mdfr1$fleet %in% fleets,];
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    if (fleets[1]          =="all") fleets          <-unique(mdfr$f);
    if (sexs[1]            =="all") sexs            <-unique(mdfr$x);
    if (maturity_states[1] =="all") maturity_states <-unique(mdfr$m);
    if (shell_conditions[1]=="all") shell_conditions<-unique(mdfr$s);
    mdfr<-mdfr[mdfr$f %in% fleets,];
    mdfr<-mdfr[mdfr$x %in% sexs,];
    mdfr<-mdfr[mdfr$m %in% maturity_states,];
    mdfr<-mdfr[mdfr$s %in% shell_conditions,];
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();

    #----------------------------------
    # plot fits to time series
    #----------------------------------
    if (verbose) cat("Plotting",nrow(mdfr),"rows.\n")
    ylab<-""; cap1<-"1"; cap2<-"2";
    if ((catch.type=="index")&&(fleet.type=="survey")) {
        ylab<-"Survey &&type (&&data)";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed survey &&data by fleet.  \n  \n";
        cap2<-"  \n  \nFigure &&fno. Comparison of observed survey &&data by fleet. Recent time period.  \n  \n";
    }
    if ((catch.type=="index")&&(fleet.type=="fishery")) {
        ylab<-"Fishery CPUE (&&data)";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed index catch &&data (CPUE)  by fleet.  \n  \n";
        cap2<-"  \n  \nFigure &&fno. Comparison of observed index catch &&data (CPUE)  by fleet. Recent time period.  \n  \n";
    }
    if ((catch.type=="retained")) {
        ylab<-"Retained catch (&&data)";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed retained catch &&data by fleet.  \n  \n";
        cap2<-"  \n  \nFigure &&fno. Comparison of observed retained catch &&data by fleet. Recent time period.  \n  \n";
    }
    if ((catch.type=="discard")) {
        ylab<-"Discard catch (&&data)";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed discard catch &&data by fleet.  \n  \n";
        cap2<-"  \n  \nFigure &&fno. Comparison of observed discard catch &&data by fleet. Recent time period.  \n  \n";
    }
    if ((catch.type=="total")) {
        ylab<-"Total catch (&&data)";
        cap1<-"  \n  \nFigure &&figno. Comparison of observed total catch &&data by fleet.  \n  \n";
        cap2<-"  \n  \nFigure &&figno. Comparison of observed total catch &&data by fleet. Recent time period.  \n  \n";
    }

    ylab<-gsub(pattern="&&type",replacement=data.type,x=ylab,fixed=TRUE);
    if (data.type=='abundance'){
        ylab<-gsub(pattern="&&data",replacement="millions",x=ylab,fixed=TRUE);
    } else {
        ylab<-gsub(pattern="&&data",replacement="1000's t",x=ylab,fixed=TRUE);
    }
    cap1<-gsub(pattern="&&data",replacement=data.type,x=cap1,fixed=TRUE);
    cap2<-gsub(pattern="&&data",replacement=data.type,x=cap2,fixed=TRUE);

    ps<-plotMDFR.Fits.TimeSeries(mdfr,
                                 numRecent=numRecent,
                                 plot1stObs=FALSE,
                                 facets=facets,
                                 scales=scales,
                                 plotObs=TRUE,
                                 plotMod=FALSE,
                                 xlab='year',
                                 ylab=ylab,
                                 title=NULL,
                                 xlims=NULL,
                                 ylims=ylims,
                                 showPlot=FALSE);
    plots[[cap1]]<-ps[[1]];
    plots[[cap2]]<-ps[[2]];

    if (verbose) cat("Finished rCompTCMs::compareData.FleetTimeSeries().\n");
    return(plots);
}

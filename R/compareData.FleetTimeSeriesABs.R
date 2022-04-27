#'
#'@title Compare time series of abundance or biommass data by fleet among several model scenarios
#'
#'@description Function to compare abundance or biomass time series data by fleet among
#'several model scenarios.
#'
#'@param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects (or dataframe from call to [extractMDFR.Data.FleetTimeSeriesABs()])
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained', 'discard' or 'total')
#'@param data.type - data type ('abundance' or 'biomass')
#'@param fleets - vector of names of fleets to plot (or 'all')
#'@param sexs - vector of sexes to plot (or 'all')
#'@param maturity_states - vector of maturity states to plot (or 'all')
#'@param shell_conditions - vector of shell conditions to plot (or 'all')
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param ci - confidence interval for plots
#'@param numRecent - number of years for 'recent' plot
#'@param ylims - limits for y axis (default is NULL)
#'@param facets - grid faceting formula
#'@param scales - ggplot2 scales option for facet_grid
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses [extractMDFR.Data.FleetTimeSeriesABs()] to extract the data (if objs is not a dataframe).
#'Uses [plotMDFR.Fits.TimeSeries()] to make the plots, with \code{plotObs=TRUE} and \code{plotMod=FALSE}. Only time series with
#'at least one non-zero value will be plotted (to eliminate lots of values along y=0).
#'
#'@return Non-nested list of ggplot2 objects, with captions as names
#'
#'@import dplyr
#'@import wtsUtilities
#'
#'@md
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
                                          position=ggplot2::position_dodge(0.2),
                                          ci=0.80,
                                          numRecent=15,
                                          ylims=NULL,
                                          facets="fleet~x",
                                          scales="free_y",
                                          verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareData.FleetTimeSeriesABs().\n");
    options(stringsAsFactors=FALSE);

    if (inherits(objs,"data.frame")){
        mdfr = objs;
    } else {
        mdfr<-rCompTCMs::extractMDFR.Data.FleetTimeSeriesABs(objs=objs,
                                                              fleet.type=fleet.type[1],
                                                              catch.type=catch.type[1],
                                                              data.type=data.type[1],
                                                              fleets=fleets,
                                                              sexs=sexs,
                                                              maturity_states=maturity_states,
                                                              shell_conditions=shell_conditions,
                                                              ci=ci,
                                                              verbose=verbose);
        #--keep only factors with at least one non-zero value
        tmp = mdfr %>%
                dplyr::group_by(case,process,fleet,category,type,pc,x,m,s) %>%
                dplyr::summarize(tot=wtsUtilities::Sum(val)) %>%
                dplyr::ungroup() %>%
                dplyr::filter(tot>0) %>%
                dplyr::select(!tot);
        mdfr %<>% dplyr::inner_join(tmp,by=c("case","process","fleet","category","type","pc","x","m","s"));
    }

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();

    #----------------------------------
    # plot fits to time series
    #----------------------------------
    if (verbose) message("Plotting",nrow(mdfr),"rows.\n")
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
                                 position=position,
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

    if (verbose) message("Finished rCompTCMs::compareData.FleetTimeSeries().\n");
    return(plots);
}

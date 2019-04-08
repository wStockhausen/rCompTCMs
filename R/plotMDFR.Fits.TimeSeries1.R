#'
#'@title Plot comparisons of fits to time series from a set of model runs
#'
#'@description Function to plot a comparison of fits to time series from a set of model runs.
#'
#'@param dfr - dataframe
#'@param plot1stObs - flag to plot observations from first case only
#'@param x - column name with x axis values
#'@param y - column name with y axis values
#'@param lci - column name with y axis values
#'@param uci - column name with y axis values
#'@param case - column name with case names
#'@param type - column name with type values (i.e., "observed","predicted")
#'@param facets - string giving faceting formula
#'@param position - indicates ggplot2 position_ to use ('dodge','jitter','identity',)
#'@param scales - ggplot2 scales option for facet_grid
#'@param plotObs - plot observations
#'@param plotMod - plot case fits/predictions
#'@param ci - confidence interval for error bars
#'@param pdfType - assumed error distribution for confidence intervals
#'@param xlab -
#'@param ylab -
#'@param title -
#'@param xlims -
#'@param ylims -
#'@param showPlot -
#'
#'@return ggplot object
#'
#'@details None.
#'
#'@import ggplot2
#'
#'@export
#'
plotMDFR.Fits.TimeSeries1<-function(dfr,
                                    plot1stObs=TRUE,
                                    x="y",
                                    y="val",
                                    lci="lci",
                                    uci="uci",
                                    case="case",
                                    type="type",
                                    facets=NULL,
                                    position=position_dodge(0.2),
                                    scales='fixed',
                                    plotObs=TRUE,
                                    plotMod=TRUE,
                                    xlab='year',
                                    ylab=NULL,
                                    title=NULL,
                                    xlims=NULL,
                                    ylims=NULL,
                                    showPlot=FALSE,
                                    verbose=FALSE){
    idx<-dfr[[type]]!='observed';
    if (verbose) {
        cat("Starting plotMDFR.Fits.TimeSeries1().\n")
        cat("Plotting",sum(idx),"model predictions.\n")
        cat("Plotting",sum(!idx),"observations.\n")
    }
    dfrp<-dfr[idx,];#predicted values
    if (plot1stObs){
        #remove observations from all but first case
        dfr$case<-as.character(dfr$case);
        cases<-as.character(unique(dfr$case));
        dfro<-dfr[(dfr$case==cases[1])&(!idx),];
        if (!is.null(dfro)&&(nrow(dfro)>0)){
          dfro$case<-'observed';
          dfr<-rbind(dfro,dfrp);
        }
    } else {
        dfro<-dfr[!idx,];
    }
    cases<-unique(dfr$case);
    dfr$case <-factor(dfr$case, levels=cases);
    dfrp$case<-factor(dfrp$case,levels=cases);
    dfro$case<-factor(dfro$case,levels=cases);
    if (verbose)  cat("Cases: ",paste0(cases,collapse=", "),".\n")

    p <- ggplot(dfr,aes_string(x=x,y=y,color=case));
    p <- p + scale_color_hue(breaks=cases);#default color scheme
    if (plotObs){
        p <- p + geom_point(aes_string(shape=case),data=dfro,size=2,alpha=0.7,position=position);
        if (!is.null(dfro$lci)&&!all(is.na(dfro$lci))){
            if (verbose) cat("Plotting cis\n")
            p <- p + geom_errorbar(aes_string(ymin=lci,ymax=uci),data=dfro,position=position);
        }
    }
    if (plotMod) p <- p + geom_line(data=dfrp,position=position);
    p <- p + coord_cartesian(xlim=xlims,ylim=ylims)
    p <- p + labs(x=xlab,y=ylab);
    p <- p + ggtitle(title);
    if (!is.null(facets)) p <- p + facet_grid(facets,scales=scales);
    if (showPlot) print(p);

    return(p);
}


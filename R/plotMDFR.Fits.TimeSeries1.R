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
#'@param facets - grid faceting formula (as an expresssion for multilevel faceting)
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
#'@param colour_scale - ggplot2 scale_colour object (default is [ggplot2::scale_colour_hue()])
#'@param fill_scale - ggplot2 scale_fill object (default is [ggplot2::scale_fill_hue()])
#'@param showPlot - flag (T/F) to print plot
#'@param verbose - flag (T/F) to print debugging info
#'
#'@return ggplot object
#'
#'@details  \code{facets} should be given as an expression, not as a character string, if you want multilevel faceting.

#'
#'@import ggplot2
#'
#' @md
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
                                    colour_scale=ggplot2::scale_color_hue(),
                                    fill_scale=ggplot2::scale_fill_hue(),
                                    showPlot=FALSE,
                                    verbose=FALSE){
    std_theme = ggplot2::theme(plot.background =ggplot2::element_blank(),
                               panel.background=ggplot2::element_blank(),
                               panel.border    =ggplot2::element_rect(colour="black",fill=NA),
                               panel.grid      =ggplot2::element_blank(),
                               panel.spacing   =unit(0,units="cm"));

    levels_<-NULL;
    if (is.factor(dfr$case)) {
        levels_<-levels(dfr$case);
    } else {
        levels_<-unique(as.character(dfr$case));
    }
    dfr$case<-as.character(dfr$case); #strip factor levels

    dfrp = dfr |> dplyr::filter(type!="observed");#predicted values
    if (verbose) {
        message("Starting plotMDFR.Fits.TimeSeries1().\n")
        message("Plotting ",nrow(dfrp)," model predictions.\n")
        message("Plotting ",nrow(dfr)-nrow(dfrp)," observations.\n")
    }

    #--determine observed data
    obsCases = unique(dfr$case);
    if (is.logical(plot1stObs)){
        if (plot1stObs) obsCases = levels_[1];
    } else if (is.character(plot1stObs)){
        obsCases = plot1stObs;
    }
    #keep observations from requested cases
    dfro = dfr |> dplyr::filter(case %in% obsCases,type=="observed");

    if (verbose) cat("LEVELS =",levels_,"\n");

    dfr = dplyr::bind_rows(dfro,dfrp) |>
            dplyr::mutate(case=factor(case, levels=levels_));
    dfrp = dfr |> dplyr::filter(type!="observed");
    dfro = dfr |> dplyr::filter(type=="observed");
    if (verbose)  message("Levels: ",paste0(levels_,collapse=", "),".\n")

    p <- ggplot(dfr,aes_string(x=x,y=y,color=case,shape=case));
    p <- p + colour_scale;
    p <- p + fill_scale;
    if (plotMod) p <- p + geom_line(data=dfrp,position=position);
    if (plotObs){
        p <- p + geom_point(data=dfro,
                            size=2,alpha=0.7,position=position);
        if (!is.null(dfro$lci)&&!all(is.na(dfro$lci))){
            if (verbose) cat("Plotting cis\n")
            p <- p + geom_linerange(aes_string(ymin=lci,ymax=uci),data=dfro,
                                    position=position,show.legend=FALSE);
        }
        labs(shape="observed")
    }
    if (plotMod) p <- p + geom_line(data=dfrp,position=position);
    p <- p + coord_cartesian(xlim=xlims,ylim=ylims)
    p <- p + labs(x=xlab,y=ylab) + std_theme;
    if (!is.null(title))  p <- p + ggtitle(title);
    if (!is.null(facets)) p <- p + facet_grid(facets,scales=scales);
    if (showPlot) print(p);

    return(p);
}


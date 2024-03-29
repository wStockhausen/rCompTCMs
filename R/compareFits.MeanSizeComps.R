#'
#'@title Compare fits to mean size comps by fleet among several model runs
#'
#'@description Function to compare fits to mean size comps by fleet among
#'several model runs.
#'
#' @param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#' @param mdfr - melted dataframe from call to [extractFits.MeanSizeComps()] (as alternative to objs)
#' @param fleets - names of fleets to include (or "all")
#' @param fleet.type - fleet type ('fishery' or 'survey')
#' @param catch.type - catch type ('index','retained',  or 'total')
#' @param  years - years to plot, as numerical vector (or "all" to plot all years)
#' @param ci - confidence interval
#' @param plot1stObs - flag (T/F) to plot observations only from first case
#' @param facet_grid - faceting formula for [ggplot2::facet_grid()]
#' @param scales - scales parameter for [ggplot2::facet_grid()]
#' @param linesize - line width (in mm)
#' @param pdf - name for output pdf file
#' @param showPlot - flag (T/F) to show plot
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses [extractFits.MeanSizeComps()] to extract data.
#'Also uses [reshape2::dcast()], [wtsUtilities::calcCIs()], [wtsUtilities::printGGList()].
#'
#'@return list of ggplot2 objects, with captions as names
#'
#'@import ggplot2
#'
#'@md
#'
#'@export
#'
compareFits.MeanSizeComps<-function(objs=NULL,
                                    mdfr=NULL,
                                    fleets="all",
                                    fleet.type=c('survey','fishery'),
                                    catch.type=c('index','retained','discard','total'),
                                    years='all',
                                    ci=0.80,
                                    plot1stObs=TRUE,
                                    facet_grid="m+s~x",
                                    scales="free_y",
                                    linesize=1,
                                    pdf=NULL,
                                    showPlot=FALSE,
                                    verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareFits.MeanSizeComps().\n");
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

    if (is.null(mdfr)){
        mdfr<-extractFits.MeanSizeComps(objs=objs,
                                        fleets=fleets,
                                        fleet.type=fleet.type,
                                        catch.type=catch.type,
                                        years=years,
                                        ci=ci,
                                        plot1stObs=plot1stObs,
                                        verbose=verbose);
    }

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
    # plot fits to size comps
    #----------------------------------
    if (verbose) message("Plotting",nrow(mdfr),"rows.\n")
    ylab<-""; cap1<-"1";
    if ((catch.type=="index")&&(fleet.type=="survey")) {
        ylab<-"mean survey size comps";
        cap1<-"  \n  \nFigure &&figno. Comparison of observed and predicted mean survey size comps for &&fleet.  \n  \n";
    }
    if ((catch.type=="index")&&(fleet.type=="fishery")) {
        ylab<-"mean fishery CPUE size comps";
        cap1<-"  \n  \nFigure &&figno. Comparison of observed and predicted mean index catch (CPUE) size comps for &&fleet.  \n  \n";
    }
    if (catch.type=="retained") {
        ylab<-"mean retained catch size comps";
        cap1<-"  \n  \nFigure &&figno. Comparison of observed and predicted mean retained catch size comps for &&fleet.  \n  \n";
    }
    if (catch.type=="total") {
        ylab<-"mean total catch size comps";
        cap1<-"  \n  \nFigure &&figno. Comparison of observed and predicted mean total catch size comps for &&fleet.  \n  \n";
    }
    zs<-sort(unique(mdfr$z));

    xlab<-'size (mm CW)';
    for (fleet in unique(mdfr$fleet)){
        if (verbose) message("Plotting fleet '",fleet,"'.\n",sep='');
        dfrp<-mdfr[mdfr$fleet==fleet,];
        #do plot
        pd<-position_identity();
        p <- ggplot(data=dfrp)
        p <- p + geom_bar(aes(x=z,y=val,fill=case),data=dfrp[dfrp$type=='observed',],stat="identity",position=position_dodge(),alpha=0.5)
        p <- p + geom_errorbar(aes(x=z,ymin=lci,ymax=uci,colour=case),data=dfrp[dfrp$type=='observed',],linetype=2,position='identity',alpha=0.5)
        p <- p + geom_line(aes(x=z,y=val,colour=case),data=dfrp[(dfrp$type=='predicted'),],size=linesize)
        p <- p + geom_point(aes(x=z,y=val,colour=case,shape=case),data=dfrp[(dfrp$type=='predicted'),],size=linesize)
        p <- p + geom_errorbar(aes(x=z,ymin=lci,ymax=uci,colour=case),data=dfrp[dfrp$type=='predicted',],position='identity')
        p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
        p <- p + labs(x=xlab,y=ylab)
        p <- p + facet_grid(stats::as.formula(facet_grid),scales=scales)
        ttl<-paste0(fleet);
        if (verbose) message("Plotting '",ttl,"'.\n",sep='')
        p <- p + ggtitle(ttl)
        p <- p + guides(fill=guide_legend('observed'),colour=guide_legend('predicted'),shape=guide_legend('predicted'),linetype=guide_legend('type'))
        cp1<-gsub("&&fleet",fleet,cap1,fixed=TRUE);
        if (showPlot) figno<-wtsUtilities::printGGList(p,figno,cp1,showPlot)$figno;
        plots[[cp1]]<-p + std_theme;
    }#fleets

    if (verbose) message("Finished rCompTCMs::compareFits.MeanSizeComps().\n");
    return(plots);
}

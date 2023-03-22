#'
#'@title Compare fits to single-year size comps by fleet among several model runs
#'
#'@description Function to compare fits to single-year size comps by fleet among
#'several model runs.
#'
#' @param objs - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#' @param mdfr - dataframe from call to [rCompTCMs::extractFits.SizeComps()] (as alternative to objs)
#' @param fleets - names of fleets to include (or "all")
#' @param fleet.type - fleet type ('fishery' or 'survey')
#' @param catch.type - catch type ('index','retained',  or 'total')
#' @param years - years to plot, as numerical vector
#' @param plot1stObs - flag (T/F) to plot observations only from first case, or character vector cases cases from which to plot observations
#' @param colour - column name for colours (default="case")
#' @param facets -  formula for faceting (default = x+m+s~.)
#' @param scales - character string indicating how x and y axis scales are handled in faceting (default = "fixed")
#' @param useBars - flag to use bars for observations
#' @param usePins - flag to use pins for observations
#' @param usePinsAndPts - flag to add pts to observations when pins are used
#' @param useLines - flag to use lines for predictions
#' @param usePoints - flag to use points for predictions
#' @param pinSize - width of pin line
#' @param lineSize - prediction line size
#' @param pointSize - prediction point size
#' @param alpha - prediction transparency
#' @param stripText - [ggplot2::element_text()] object describing font and margin to use for panel strips
#' @param pdf - name for output pdf file
#' @param showPlot - flag (T/F) to show plot
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses [rCompTCMs::extractFits.SizeComps()] to extract results.
#'Also uses [wtsUtilities::printGGList()].
#'
#'
#'@import dplyr
#'@import ggplot2
#'
#'@md
#'
#'@export
#'
compareFits.SingleYearSizeComps<-function(objs=NULL,
                                        mdfr=NULL,
                                        fleets="all",
                                        fleet.type=c('survey','fishery'),
                                        catch.type=c('index','retained','discard','total'),
                                        years='all',
                                        colour="case",
                                        facets=x+m+s~.,
                                        scales='fixed',
                                        plot1stObs=TRUE,
                                        useBars=TRUE,
                                        usePins=FALSE,
                                        usePinsAndPts=FALSE,
                                        useLines=TRUE,
                                        usePoints=TRUE,
                                        pinSize=0.2,
                                        lineSize=0.2,
                                        pointSize=1,
                                        alpha=0.5,
                                        stripText=ggplot2::element_text(),
                                        pdf=NULL,
                                        showPlot=FALSE,
                                        verbose=FALSE){

    if (verbose) message("Starting rCompTCMs::compareFits.SingleYearSizeComps().\n");
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

    if (is.null(mdfr)){
        mdfr<-extractFits.SizeComps(objs,
                                   fleets=fleets,
                                   fleet.type=fleet.type,
                                   catch.type=catch.type,
                                   years=years,
                                   plot1stObs=plot1stObs,
                                   verbose=verbose);
    }

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;
    std_theme = wtsPlots::getStdTheme();

    #----------------------------------
    # plot fits to size comps
    #----------------------------------
    if (verbose) message("Plotting",nrow(mdfr),"rows.\n")
    ylab<-""; cap1<-"1";
    if ((catch.type=="index")&&(fleet.type=="survey")) {
        ylab<-"survey size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted survey size comps in &&y for &&fleet. &&pg.  \n  \n";
    }
    if ((catch.type=="index")&&(fleet.type=="fishery")) {
        ylab<-"fishery CPUE size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted index catch (CPUE) size comps in &&y for &&fleet. &&pg.  \n  \n";
    }
    if (catch.type=="retained") {
        ylab<-"retained catch size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted retained catch size comps in &&y for &&fleet. &&pg.  \n  \n";
    }
    if (catch.type=="total") {
        ylab<-"total catch size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted total catch size comps in &&y for &&fleet. &&pg.  \n  \n";
    }
    xs<-c("male","female","all sex");
    ms<-c("immature","mature","all maturity");
    ss<-c("new shell","old shell","all shell");
    zs<-sort(unique(mdfr$z));

    xlab<-'size (mm CW)';
    if (verbose) message(paste0("names(mdfr)= '",names(mdfr),"'.\n"));
    for (fleet in unique(mdfr$fleet)){
        if (verbose) message("Plotting fleet '",fleet,"'.\n",sep='');
        mdfr1 = mdfr %>% dplyr::filter(fleet==fleet);
        yrs1  = sort(unique(mdfr1$y));
        if (years!="all") yrs1 = yrs1[yrs1 %in% years];
        for (yr1 in yrs1){
            if (verbose) message("\tPlotting year '",yr1,"'.\n",sep='');
            dfrp = mdfr1 %>% dplyr::filter(y==yr1);
            fg = "x+m+s~.";
            dfrpo = dfrp %>% dplyr::filter(type=='observed');
            #do plot
            pd = position_identity();
            p  = ggplot(data=dfrp)
            if (plot1stObs){
                if (useBars)       p = p + geom_bar(aes(x=z,y=val),fill="black",data=dfrpo,stat="identity",position='identity',alpha=0.5)
                if (usePins)       p = p + geom_linerange(aes(x=z,ymax=val),colour="black",data=dfrpo,stat="identity",position='identity',ymin=0.0,size=pinSize)
                if (usePinsAndPts) p = p + geom_point(aes(x=z,y=val),colour="black",data=dfrpo,stat="identity",position='identity',size=0.5,alpha=1)
            } else {
                if (useBars)       p = p + geom_bar(aes(x=z,y=val,fill=case),data=dfrpo,stat="identity",position='identity',alpha=0.5)
                if (usePins)       p = p + geom_linerange(aes(x=z,ymax=val,colour=case),data=dfrpo,stat="identity",position='identity',ymin=0.0,size=pinSize)
                if (usePinsAndPts) p = p + geom_point(aes(x=z,y=val,colour=case),data=dfrpo,stat="identity",position='identity',size=0.5,alpha=1)
            }
            if (useLines)      p = p + geom_line( aes(x=z,y=val,colour=case),data=dfrp[(dfrp$type=='predicted'),],size=lineSize,alpha=alpha)
            if (usePoints)     p = p + geom_point(aes(x=z,y=val,colour=case,shape=case),data=dfrp[(dfrp$type=='predicted'),],size=pointSize)
            p <- p + ylim(0,NA)
            p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
            p <- p + labs(x=xlab,y=ylab)
            p <- p + facet_grid(facets,scales=scales)
            ttl<-paste0(fleet,': ',yr1);
            if (verbose) message("Plotting '",ttl,"'.\n",sep='')
            p <- p + ggtitle(ttl)
            p <- p + guides(fill=guide_legend('observed'),colour=guide_legend('predicted'),shape=guide_legend('predicted'))
            cp1<-gsub("&&y",yr1,cap1,fixed=TRUE);
            cp1<-gsub("&&fleet",fleet,cp1,fixed=TRUE);
            if (showPlot) figno<-wtsUtilities::printGGList(p,figno,cp1,showPlot)$figno;
            plots[[cp1]]<-p+std_theme+theme(strip.text=stripText);
        }#--yr1
    }#--fleet

    if (verbose) message("Finished rCompTCMs::compareFits.SingleYearSizeComps().\n");
    return(plots);
}

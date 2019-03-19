#'
#'@title Plot mean and probability of post-molt size for several model runs
#'
#'@description Function to plot mean and probability of post-molt size for several model runs.
#'
#'@param mdfrMnG - melted dataframe for mean growth (from calling \code{rTCSAM02::getMDFR.Pop.MeanGrowth})
#'@param mdfrPrG - melted dataframe with growth probabilities (from calling \code{rTCSAM02::getMDFR.Pop.GrowthMatrices})
#'@param scale - scaling factor for probabilities
#'@param zbnds - 2-element vector indicating lower and upper bounds on model size bins
#'@param showPlot - flag to print plot to current device
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Mean post-molt size as a function of pre-molt size is plotted as a line for each case (model),
#'overlaid on the (scaled) actual probability of post-molt size.
#'Cases are distinguished by fill and line colours.
#'A 1:1 line and lines indicating the model size limits are also shown.
#'Plots are produced by sex, and a list of plots is returned.
#'
#'@return list of ggplots by sex
#'
#'@import ggplot2
#'
#'@export
#'
plotPop.MeanGrowthPlusProbs<-function(mdfrMnG,
                                      mdfrPrG,
                                      scale=10,
                                      xbnds=NULL,
                                      ybnds=NULL,
                                      zbnds=c(25,185),
                                      showPlot=FALSE,
                                      verbose=FALSE){
    #--modify mean growth dataframe
    mdfr<-mdfrMnG;
    mdfr$z<-as.numeric(mdfr$z);#make sure z is numeric

    #--modify growth probabilities dataframe
    mdfr1<-mdfrPrG;
    mdfr1$z<-as.numeric(mdfr1$z);     #make sure pre-molt size is numeric
    mdfr1$zp<-as.numeric((mdfr1$zp)); #make sure post-molt size is numeric
    zps<-sort(unique(mdfr1$zp));
    dzp<-zps[2]-zps[1];
    scale<-scale/max(mdfr1$val);

    mdfr1$zgrp<-factor(mdfr1$z,levels=unique(mdfr1$z))
    mdfr1$zprb<-mdfr1$z+scale*mdfr1$val;
    mdfr1<-mdfr1[mdfr1$val>0,];

    mdfr1$xmin<-mdfr1$z;
    mdfr1$xmax<-mdfr1$zprb;
    mdfr1$ymin<-mdfr1$zp-dzp/2;
    mdfr1$ymax<-mdfr1$zp+dzp/2;

    #--adjust bounds accordingly
    if (is.null(xbnds)) {if (!is.null(zbnds)) xbnds<-zbnds;}
    if (is.null(ybnds)) {if (!is.null(ybnds)) ybnds<-zbnds;}

    #--loop over sexes
    plts<-list();
    uXs<-unique(mdfrMnG$x);
    for (x in uXs){
        mdfrp <-mdfr[mdfr$x==x,];
        mdfr1p<-mdfr1[mdfr1$x==x,];
        #-------------------------------------------#
        #plot mean growth + growth probabilities
        #-------------------------------------------#
        #cat("names =",names(mdfr1p),"\n")
        #print(head(mdfr1p))
        p <- ggplot(data=mdfr1p,mapping=aes_string(x="zprb",y="zp"));
        p <- p + geom_abline(slope=1,colour="black",linetype=2);
        p <- p + geom_hline(yintercept=185.0,colour="gray",linetype=3);
        p <- p + geom_vline(xintercept=185.0,colour="gray",linetype=3);
        p <- p + geom_hline(yintercept= 25.0,colour="gray",linetype=3);
        p <- p + geom_vline(xintercept= 25.0,colour="gray",linetype=3);
#        p <- p + geom_polygon(mapping=aes_string(group="zgrp",fill="case"),alpha=0.5);
#        p <- p + geom_path(mapping=aes_string(group="zgrp",colour="case"));
        p <- p + geom_rect(data=mdfr1p,mapping=aes_string(xmin="xmin",ymin="ymin",xmax="xmax",ymax="ymax",group="zgrp",fill="case"),alpha=0.5);
        p <- p + geom_rect(data=mdfr1p,mapping=aes_string(xmin="xmin",ymin="ymin",xmax="xmax",ymax="ymax",group="zgrp",colour="case"),fill=NA);
        p <- p + geom_line(data=mdfrp,mapping=aes_string(x='z',y='val',colour='case'));
        p <- p + geom_point(data=mdfrp,mapping=aes_string(x='z',y='val',colour='case'));
        p <- p + facet_grid(rows=vars(x));
        p <- p + coord_cartesian(xlim=xbnds,ylim=ybnds);
#        p <- p + ggtitle("Size transition probabilities");
        p <- p + labs(x='pre-molt size (mm CW)',y="post-molt size (mm CW)");
        if (showPlot) print(p);
        plts[[x]]<-p;
    }

    return(plts);
}

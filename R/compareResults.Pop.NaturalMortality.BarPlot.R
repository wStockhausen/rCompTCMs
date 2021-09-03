#'
#'@title Function to compare natural mortality rates as bars
#'
#'@description This function compares natural mortality estimates by
#'   sex and maturity state using barplots.
#'
#'@param objs - list of resLst objects from TCSAM02
#'@param colour_scale - ggplot2 scale_colour object (default is ggplot2::scale_colour_hue())
#'@param pdf - creates pdf, if not NULL
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object as list element
#'
#'@details None.
#'
#'@import cowplot
#'@import ggplot2
#'@import magrittr
#'
#'@export
#'
compareResults.Pop.NaturalMortality.BarPlot<-function(
                                              objs,
                                              colour_scale=ggplot2::scale_color_hue(),
                                              pdf=NULL,
                                              showPlot=FALSE,
                                              verbose=FALSE){
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    if (!inherits(objs,"data.frame")){
      dfrNMs<-rTCSAM02::getMDFR.Pop.NaturalMortality(objs,'M_cxm',verbose);
    } else {
      dfrNMs = objs;
    }

    #----------------------------------
    # plot natural mortality rates as bars
    #----------------------------------
    # dfrNMsW.Imm<-reshape2::dcast(dfrNMs %>% subset(m=="IMMATURE"),y+case~x,fun.aggregate=wtsUtilities::Sum,value.var="val");
    # dfrNMsW.Mat<-reshape2::dcast(dfrNMs %>% subset(m=="MATURE"),  y+case~x,fun.aggregate=wtsUtilities::Sum,value.var="val");
    mxNM<-max(dfrNMs$val);
    uMs<-unique(tolower(dfrNMs$m));
    plots<-list();
    for (uM in uMs){
        tmp<-dfrNMs[tolower(dfrNMs$m)==uM,];
        p <- ggplot(tmp,aes_string(x="x",y="val",fill="case"))+geom_bar(stat="identity",position=position_dodge());
        p <- p + colour_scale;
        p <- p + facet_grid(y~m);
        p <- p + ylim(0,mxNM);
        p <- p + labs(x="",y="natural mortality",fill="scenario");
        pl<- p; #--save plot w/ legend
        p <- p + theme(legend.position="none",);
        plots[[uM]]<-p;
    }
    prows<-cowplot::plot_grid(plotlist=plots,nrow=2,rel_heights=c(1,2));
    legend<-cowplot::get_legend(pl);
    p<-cowplot::plot_grid(prows,legend,ncol=2,rel_widths=c(4,2));
    if (showPlot) print(p);

    plots1<-list();
    cap1<-"  \n  \nFigure &&figno. Estimated natural mortality rates, by time period.  \n  \n";
    plots1[[cap1]]<-p;
    return(plots1);
}

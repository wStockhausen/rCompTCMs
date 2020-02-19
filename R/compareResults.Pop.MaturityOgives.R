#'
#' @title Compare predicted maturity ogives among several model runs
#'
#' @description Function to compare predicted maturity ogives among several model runs.
#'
#' @param objs - list of resLst objects
#' @param nyrs - number of years per plot
#' @param pdf - name for output pdf file
#' @param showPlot - flag to print plot to current device
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @details None.
#'
#' @return ggplot object
#'
#' @import ggplot2
#'
#' @export
#'
compareResults.Pop.MaturityOgives<-function(objs,
                                            nyrs=10,
                                             pdf=NULL,
                                             showPlot=FALSE,
                                             verbose=FALSE){
    options(stringsAsFactors=FALSE);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        mdfr1<-NULL;
        obj<-objs[[case]];
        #if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.MeanMaturity(obj,verbose);
        #if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.MeanMaturity(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.MaturityOgives(obj,verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }

    if (is.null(mdfr)) {
        cat("\n \nNo maturity ogive data.\n \n")
        return(NULL);
    }

    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.character(mdfr$y);

    datasets<-unique(mdfr$category);
    plots<-list();
        mdfrp<-mdfr;
        dcs<-unique(mdfrp$case);    #unique cases
        uys<-sort(unique(mdfrp$y)); #unique years
        cat("dcs: ",dcs,"\n")
        cat("uys: ",uys,"\n")
        #-------------------------------------------#
        #plot predicted maturity ogives
        #-------------------------------------------#
        nys<-length(uys);
        for (iy in 1:ceiling(nys/nyrs)){
            iys<-min(c(nyrs*(iy-1)+1,nys+1)):min(c(nyrs*(iy),nys));
            cat("iys: ",iys,"\n");
            cat("uys[iys]: ",uys[iys],"\n");
            dfrp<-mdfrp[mdfrp$y %in% uys[iys],];
            mdfrpp<-dfrp[dfrp$type == 'predicted',];
            p <- ggplot(mdfrpp,aes_string(x='z',y='val',colour='y'));
            p <- p + geom_line(data=mdfrpp);
            if (any(!is.na(mdfrpp$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
            p <- p + labs(x='size (mm CW)',y="probability(mature)");
            #p <- p + ggtitle(d);
            p <- p + facet_grid(case~.);
            if (showPlot) print(p);
            cap<-paste0("\n  \nFigure &&figno. Predicted maturity ogives for ",uys[min(iys)]," to ",uys[max(iys)],".\n   \n")
            plots[[cap]]<-p;
        }

    return(plots);
}

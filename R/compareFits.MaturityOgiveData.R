#'
#' @title Compare fits to maturity ogive data among several model runs
#'
#' @description Function to compare fits to maturity ogive data among several model runs.
#'
#' @param objs - list of resLst objects
#' @param nyrs - number of years/plot
#' @param types - requested plot types ("fits","nlls",and/or "zscores")
#' @param minSize - minimum size to include in plots
#' @param dodge - width to dodge overlapping series
#' @param plot1stObs - flag to include observations only from 1st model scenario
#' @param pdf - name for output pdf file
#' @param showPlot - flag to print plot to current device
#' @param verbose - flag (T/F) to print diagnostic information
#'
#' @details None.
#'
#' @return ggplot object
#'
#'@import ggplot2
#'@import stringr
#'@import wtsPlots
#'
#' @export
#'
compareFits.MaturityOgiveData<-function(objs,
                                        nyrs=5,
                                        types=c("fits","nlls","zscores"),
                                        minSize=0,
                                        dodge=0.2,
                                        plot1stObs=FALSE,
                                        pdf=NULL,
                                        showPlot=FALSE,
                                        verbose=FALSE){
    options(stringsAsFactors=FALSE);

    std_theme = wtsPlots::getStdTheme();

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
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.MaturityOgiveData(obj,verbose);
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }

    if (is.null(mdfr)) {
        cat("\n \nNo fits to maturity data.\n \n")
        return(NULL);
    }

    mdfr$z<-as.numeric(mdfr$z)
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.character(mdfr$y);

    pd<-ggplot2::position_dodge(width=dodge);
    datasets<-unique(mdfr$category);
    plots<-list();
    for (d in datasets){
        mdfrp<-mdfr[(mdfr$category==d),];
        dp = stringr::str_replace_all(d,stringr::fixed("_")," ");
        if (nrow(mdfrp)>0){
            dcs<-unique(mdfrp$case);   #unique cases
            uys<-sort(unique(mdfrp$y)); #unique years
            if (verbose) cat("dcs: ",dcs,"\n")
            if (verbose) cat("uys: ",uys,"\n")
            nys<-length(uys);
            if ("fits" %in% types){
                #-------------------------------------------#
                #plot maturity data and fits
                #-------------------------------------------#
                if (verbose) cat("\n--Plotting fits.\n")
                if (plot1stObs) {
                    mdfrpo<-mdfrp[(mdfrp$type == 'observed')&(mdfrp$case==dcs[1]), ];
                } else {
                    mdfrpo<-mdfrp[mdfrp$type == 'observed', ];
                }
                mdfrpp<-mdfrp[mdfrp$type == 'predicted',];
                zlims<-range(c(mdfrpo$z,mdfrpp$z));
                ylims<-c(0,1);
                if (verbose) cat("zlims = ",zlims,"\n");
                if (verbose) cat("ylims = ",ylims,"\n");
                for (iy in 1:ceiling(nys/nyrs)){
                    iys<-min(c(nyrs*(iy-1)+1,nys+1)):min(c(nyrs*(iy),nys));
                    if (verbose) cat("iys: ",iys,"\n");
                    if (verbose) cat("uys[iys]: ",uys[iys],"\n");
                    dfrp<-mdfrp[mdfrp$y %in% uys[iys],];
                    if (plot1stObs) {
                        mdfrpo<-dfrp[(dfrp$type == 'observed')&(dfrp$case==dcs[1]), ];
                    } else {
                        mdfrpo<-dfrp[dfrp$type == 'observed', ];
                    }
                    mdfrpp<-dfrp[dfrp$type == 'predicted',];
                    p <- ggplot(dfrp,aes_string(x='z',y='val',colour='case',shape='case'));
                    p <- p + coord_cartesian(xlim=zlims,ylim=ylims);
                    p <- p + geom_point(data=mdfrpo,position=pd);
                    p <- p + geom_line(data=mdfrpp,position=pd);
                    if (any(!is.na(mdfr$lci))) p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd);
                    p <- p + labs(x='size (mm CW)',y="probability(mature)");
                    p <- p + facet_grid(y~.) + std_theme;
                    if (showPlot) print(p);
                    cap<-paste0("\n  \nFigure &&figno. Model fits to ",dp," for ",uys[min(iys)]," to ",uys[max(iys)],".\n   \n")
                    plots[[cap]]<-p;
                }
                rm(mdfrpo,mdfrpp);
            }
            if ("nlls" %in% types){
                #-------------------------------------------#
                #plot nll scores
                #-------------------------------------------#
                if (verbose) cat("\n--Plotting nlls.\n")
                mdfrpn<-mdfrp[(mdfrp$type == 'nlls')&(mdfrp$z>=minSize),];
                zlims<-range(mdfrpn$z);
                ylims<-c(min(c(0,mdfrpn$val)),max(mdfrpn$val));
                if (verbose) cat("zlims = ",zlims,"\n");
                if (verbose) cat("ylims = ",ylims,"\n");
                for (iy in 1:ceiling(nys/nyrs)){
                    iys<-min(c(nyrs*(iy-1)+1,nys+1)):min(c(nyrs*(iy),nys));
                    if (verbose) cat("iys: ",iys,"\n");
                    if (verbose) cat("uys[iys]: ",uys[iys],"\n");
                    dfrp<-mdfrp[mdfrp$y %in% uys[iys],];
                    mdfrpn<-dfrp[(dfrp$type == 'nlls')&(dfrp$z>=minSize),];
                    p <- ggplot(mdfrpn,aes_string(x='z',y='val',colour='case',shape='case'));
                    p <- p + coord_cartesian(xlim=zlims,ylim=ylims);
                    p <- p + geom_point(position=pd);
                    p <- p + geom_abline(slope=0,linetype=2);
                    p <- p + labs(x='size (mm CW)',y="NLLs");
                    p <- p + facet_grid(y~.) + std_theme;
                    if (showPlot) print(p);
                    cap<-paste0("\n  \nFigure &&figno. Negative log-likelihood values for fits to ",dp," for ",uys[min(iys)]," to ",uys[max(iys)],".\n   \n")
                    plots[[cap]]<-p;
                }
                rm(mdfrpn);
            }
            if ("zscores" %in% types){
                #-------------------------------------------#
                #plot zscores
                #-------------------------------------------#
                if (verbose) cat("\n--Plotting zscores.\n")
                mdfrpz<-mdfrp[(mdfrp$type == 'zscores')&(mdfrp$z>=minSize),];
                zlims<-range(mdfrpz$z);
                if (verbose) cat("zlims = ",zlims,"\n");
                for (iy in 1:ceiling(nys/nyrs)){
                    iys<-min(c(nyrs*(iy-1)+1,nys+1)):min(c(nyrs*(iy),nys));
                    if (verbose) cat("iys: ",iys,"\n");
                    if (verbose) cat("uys[iys]: ",uys[iys],"\n");
                    dfrp<-mdfrp[mdfrp$y %in% uys[iys],];
                    mdfrpz<-dfrp[(dfrp$type == 'zscores')&(dfrp$z>=minSize),];
                    ylims<-c(-1.01,1.01)*max(abs(mdfrpz$val));
                    if (verbose) cat("ylims = ",ylims,"\n");
                    p <- ggplot(mdfrpz,aes_string(x='z',y='val',colour='case',shape='case'));
                    p <- p + coord_cartesian(xlim=zlims,ylim=ylims);
                    p <- p + geom_smooth(mapping=aes(group=case,fill=case,colour=case),alpha=0.5);
                    p <- p + geom_point(position=pd);
                    p <- p + geom_abline(slope=0,linetype=2);
                    p <- p + labs(x='size (mm CW)',y="z-scores");
                    p <- p + facet_grid(y~.) + std_theme;
                    if (showPlot) print(p);
                    cap<-paste0("\n  \nFigure &&figno. Z-scores for fits to ",dp," for ",uys[min(iys)]," to ",uys[max(iys)],".\n   \n")
                    plots[[cap]]<-p;
                }
            }
        }
    }#d

    return(plots);
}

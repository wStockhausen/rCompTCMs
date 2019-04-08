#'
#'@title Plot fits to size comps by fleet among several model runs from a dataframe
#'
#'@description Function to plot fits to size comps by fleet among
#'several model runs from a dataframe.
#'
#' @param mdfr - dataframe from call to \code{rCompTCMs::extractFits.SizeComps}
#' @param ylab - y axis label
#' @param cap1 - template for caption
#' @param plotObs - flag to plot observations
#' @param plotMod - flag to plot model predictions
#' @param plotObsAsBars - flag (T/F) to plot observations/predictions as bars/lines (T) or lines/bars (F)
#' @param nrow - number of rows per page for output plots
#' @param ncol - number of columns per page for output plots
#' @param pdf - name for output pdf file
#' @param showPlot - flag (T/F) to show plot
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{wtsUtilities::printGGList}.
#'
#'@return Non-nested list of ggplot2 objects, with captions as names
#'
#'@import ggplot2
#'
#'@export
#'
plotMDFR.Fits.SizeComps<-function(mdfr=NULL,
                                  ylab="",
                                  cap1="1",
                                  plotObs=TRUE,
                                  plotMod=TRUE,
                                  plotObsAsBars=TRUE,
                                  nrow=5,
                                  ncol=3,
                                  pdf=NULL,
                                  showPlot=FALSE,
                                  verbose=FALSE){

    if (verbose) cat("Starting rCompTCMs::plotMDFR.Fits.SizeComps().\n");
    options(stringsAsFactors=FALSE);

    #create pdf, if necessary
    if(!is.null(pdf)){
        cat("Creating pdf:",pdf,"\n");
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;

    idx<-mdfr$x=="all"; mdfr$x[idx]<-"all sex";
    idm<-mdfr$m=="all"; mdfr$m[idm]<-"all maturity";
    ids<-mdfr$s=="all"; mdfr$s[ids]<-"all shell";

    mdfrp<-NULL;
    if (plotObs) mdfrp<-rbind(mdfrp,mdfr[mdfr$type=="observed",]);
    if (plotMod) mdfrp<-rbind(mdfrp,mdfr[mdfr$type=="predicted",]);
    mdfr<-mdfrp;

    #----------------------------------
    # plot fits to size comps
    #----------------------------------
    if (verbose) cat("Plotting",nrow(mdfr),"rows.\n")
    xs<-c("male","female","all sex");
    ms<-c("immature","mature","all maturity");
    ss<-c("new shell","old shell","all shell");
    zs<-sort(unique(mdfr$z));

    mxp<-nrow*ncol;
    xlab<-'size (mm CW)';
    for (fleet in unique(mdfr$fleet)){
        if (verbose) cat("Plotting fleet '",fleet,"'.\n",sep='');
        idf<-mdfr$fleet==fleet;
            pxs<-list();
            for (x in xs){
                idx<-mdfr$x==x;
                pms<-list();
                for (m in ms){
                    idm<-mdfr$m==m;
                    pss<-list();
                    for (s in ss){
                        ids<-mdfr$s==s;
                        if (verbose) cat("Checking",x,m,s,"\n");
                        pgs<-list();
                        if (sum(idf&idx&idm&ids)==0){
                            if (verbose) cat("--Dropping",x,m,s,"\n");
                        } else {
                            if (verbose) cat("--Plotting",x,m,s,"size comps\n");
                            mdfrp<-mdfr[idf&idx&idm&ids,];#select results for fleet, sex, maturity state, and shell condition
                            if (verbose) cat("--Plotting",nrow(mdfrp),"rows.\n")

                            #add in missing years as size comps with 0's
                            ys<-sort(unique(mdfrp$y));
                            mny<-5*floor(min(ys)/5);
                            mxy<-mny+mxp*ceiling((max(ys)-mny+1)/mxp)-1;
                            mdfrpp<-mdfrp[1,];
                            mdfrpp$val<-0;
                            if (mny<min(ys)){
                                for (y in mny:(min(ys)-1)) {
                                    mdfrpp$y<-y;
                                    mdfrp<-rbind(mdfrp,mdfrpp);
                                }
                            }
                            if (mxy>max(ys)){
                                for (y in (max(ys)+1):mxy) {
                                    mdfrpp$y<-y;
                                    mdfrp<-rbind(mdfrp,mdfrpp);
                                }
                            }
                            ys<-mny:mxy;
                            npg<-ceiling(length(ys)/mxp);
                            if (verbose) cat("mny =",mny,",mxy =",mxy,", npg =",npg,'\n')
                            for (y in ys){
                                if (!any(mdfrp$y==y)) {
                                    #year y is missing, so add in zero size comp for year y
                                    mdfrpp$y<-y;
                                    mdfrp<-rbind(mdfrp,mdfrpp);
                                }
                            }

                            rng<-range(mdfrp$val,na.rm=TRUE,finite=TRUE);
                            if (verbose) cat("rng = ",rng,'\n')

                            for (pg in 1:npg){ #loop over pages
                                dfrp<-mdfrp[(mdfrp$y %in% ys[(pg-1)*mxp+1:mxp]),]
                                #do plot
                                pd<-position_identity();
                                p <- ggplot(data=dfrp);
                                if (plotObsAsBars){
                                    p <- p + geom_bar(aes(x=z,y=val,fill=case),data=dfrp[dfrp$type=='observed',],stat="identity",position='identity',alpha=0.5);
                                    p <- p + geom_line(aes(x=z,y=val,colour=case),data=dfrp[(dfrp$type=='predicted'),],size=1);
                                    p <- p + guides(fill=guide_legend('observed'),colour=guide_legend('predicted'));
                                } else {
                                    p <- p + geom_bar(aes(x=z,y=val,fill=case),data=dfrp[dfrp$type=='predicted',],stat="identity",position='identity',alpha=0.5);
                                    p <- p + geom_line(aes(x=z,y=val,colour=case),data=dfrp[(dfrp$type=='observed'),],size=1);
                                    p <- p + guides(fill=guide_legend('predicted'),colour=guide_legend('observed'));
                                }
                                p <- p + ylim(0,rng[2])
                                p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
                                p <- p + labs(x=xlab,y=ylab)
                                p <- p + facet_wrap(~y,ncol=ncol,dir='v')
                                ttl<-paste0(fleet,': ',x,", ",m,", ",s);
                                if (verbose) cat("Plotting '",ttl,"'.\n",sep='')
                                p <- p + ggtitle(ttl)
                                xms<-paste0(x,", ",m,", ",s);
                                cp1<-gsub("&&xms",xms,cap1,fixed=TRUE);
                                cp1<-gsub("&&fleet",fleet,cp1,fixed=TRUE);
                                cp1<-gsub("&&pg",paste0("Page ",pg," of ",npg),cp1,fixed=TRUE);
                                if (showPlot) figno<-wtsUtilities::printGGList(p,figno,cp1,showPlot)$figno;
                                plots[[cp1]]<-p;
                            }#pg
                        }#if
                    }#ss
                }#ms
            }#xs
    }#fleets

    if (verbose) cat("Finished rCompTCMs::plotMDFR.Fits.SizeComps().\n");
    return(plots);
}

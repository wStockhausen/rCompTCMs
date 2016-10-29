#'
#'@title Compare fits to size comps by fleet among several model runs
#'
#'@description Function to compare fits to size comps by fleet among 
#'several model runs.
#'
#'@param obj - object that can be converted into a list of tcsam2013.resLst and/or tcsam02.resLst objects
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param catch.type - catch type ('index','retained',  or 'total')
#'@param nrow - number of rows per page for output plots
#'@param ncol - number of columns per page for output plots
#'@param pdf - name for output pdf file
#'@param showPlot - flag (T/F) to show plot
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details Uses \code{rTCSAM2013::getMDFR.SurveyQuantities()},
#'\code{rTCSAM2013::getMDFR.FisheryQuantities()}, \code{rTCSAM02::getMDFR.Fits.FleetData()}.
#'Also uses \code{wtsUtilities::printGGList}.
#'
#'@return Non-nested list of ggplot2 objects, with captions as names
#'
#'@import ggplot2
#'
#'@export
#'
compareFits.SizeComps<-function(objs=NULL,
                                  fleet.type=c('survey','fishery'),
                                  catch.type=c('index','retained','discard','total'),
                                  nrow=5,
                                  ncol=3,
                                  numRecent=15,
                                  plot1stObs=TRUE,
                                  scales="free_y",
                                  pdf=NULL,
                                  showPlot=FALSE,
                                  verbose=FALSE){
    
    if (verbose) cat("Starting rCompTCMs::compareFits.SizeComps().\n");
    options(stringsAsFactors=FALSE);
    
    fleet.type<-fleet.type[1];
    catch.type<-catch.type[1];

    if (fleet.type=='survey') catch.type<-'index';

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    if (catch.type=='index')    type<-'prNatZ_yxmz';
    if (catch.type=='retained') type<-'prNatZ.ret';
    if (catch.type=='total')    type<-'prNatZ.tot';
    
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        mdfr1<-NULL;
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-NULL;
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Fits.FleetData(obj,
                                                                                      fleet.type=fleet.type,
                                                                                      data.type='n.at.z',
                                                                                      catch.type=catch.type,
                                                                                      verbose=verbose);
        if (fleet.type=='survey'){
            if (inherits(obj,"tcsam2013.resLst"))
            mdfr1<-rTCSAM2013::getMDFR.SurveyQuantities(obj,
                                                        type=type,
                                                        verbose=verbose);
        }
        if (fleet.type=='fishery'){
            if (inherits(obj,"tcsam2013.resLst"))
                mdfr1<-rTCSAM2013::getMDFR.FisheryQuantities(obj,
                                                             type=type,
                                                             verbose=verbose);
        }
        if (!is.null(mdfr1)){
            mdfr1$case<-case;
            mdfr<-rbind(mdfr,mdfr1);
        }
    }
    mdfr$case<-factor(mdfr$case,levels=cases);
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$x[mdfr$x=='all']<-'all sex';
    mdfr$m[mdfr$m=='all']<-'all maturity';
    mdfr$s[mdfr$s=='all']<-'all shell';

    #----------------------------------
    # define output list of plots
    #----------------------------------
    plots<-list();
    figno<-1;
    
    #----------------------------------
    # plot fits to size comps 
    #----------------------------------
    if (verbose) cat("Plotting",nrow(mdfr),"rows.\n")
    ylab<-""; cap1<-"1";
    if ((catch.type=="index")&&(fleet.type=="survey")) {
        ylab<-"survey size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&xms survey size comps for &&fleet. &&pg.  \n  \n";
    }
    if ((catch.type=="index")&&(fleet.type=="fishery")) {
        ylab<-"fishery CPUE size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&xms index catch (CPUE) size comps for &&fleet. &&pg.  \n  \n";
    }
    if (catch.type=="retained") {
        ylab<-"retained catch size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&xms retained catch size comps for &&fleet. &&pg.  \n  \n";
    }
    if (catch.type=="total") {
        ylab<-"total catch size comps";
        cap1<-"  \n  \nFigure &&fno. Comparison of observed and predicted &&xms total catch size comps for &&fleet. &&pg.  \n  \n";
    }
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
                            mdfrp<-mdfr[idf&idx&idm&ids,];#select results for fleet, catch type, and sex
                            if (verbose) cat("--Plotting",nrow(mdfrp),"rows.\n")
                            
                            ys<-sort(unique(mdfrp$y));
                            mny<-5*floor(min(ys)/5);
                            mxy<-mny+mxp*ceiling((max(ys)-mny)/mxp)-1;
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
                            
                            rng<-range(mdfrp$val,na.rm=TRUE,finite=TRUE);
                            if (verbose) cat("rng = ",rng,'\n')
                            
                            for (pg in 1:npg){ #loop over pages
                                dfrp<-mdfrp[(mdfrp$y %in% ys[(pg-1)*mxp+1:mxp]),]
                                #do plot
                                pd<-position_identity();
                                p <- ggplot(data=dfrp)
                                p <- p + geom_bar(aes(x=z,y=val,fill=case),data=dfrp[dfrp$type=='observed',],stat="identity",position='identity',alpha=0.5)
                                p <- p + geom_line(aes(x=z,y=val,colour=case),data=dfrp[(dfrp$type=='predicted'),],size=1)
                    #             p <- p + scale_x_continuous(breaks=pretty(uz)) 
                    #             p <- p + scale_y_continuous(breaks=pretty(rng),expand=c(0.01,0))
                                p <- p + ylim(0,rng[2])
                                p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
                                p <- p + labs(x=xlab,y=ylab)
                                p <- p + facet_wrap(~y,ncol=ncol,dir='v') 
                                ttl<-paste0(fleet,': ',x,", ",m,", ",s);
                                if (verbose) cat("Plotting '",ttl,"'.\n",sep='')
                                p <- p + ggtitle(ttl)
                                p <- p + guides(fill=guide_legend('observed'),colour=guide_legend('predicted'))
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

    if (verbose) cat("Finished rCompTCMs::compareFits.SizeComps().\n");
    return(plots);
}

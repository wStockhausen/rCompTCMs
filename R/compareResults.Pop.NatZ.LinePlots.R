#'
#'@title Function to compare population numbers-at-size estimates by year among several models
#'
#'@description This function compares population numbers-at-size estimates by year
#'   using line plots among several models.
#'
#'@param objs - list of resLst objects
#'@param type - type of abuundance ("N_yxmsz","N_yxmz","N_yxz","iN_xmsz","fN_xmsz")
#'@param dodge - width to dodge overlapping series
#'@param ncol - number of columns per page for multi-year plots
#'@param nrow - number of rows per page (nominal) for multi-year plots
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details uses \code{rTCSAM2013::getMDFR.Pop.Quantities},
#'\code{rsimTCSAM::getMDFR.Pop.Quantities}, \code{rsimTCSAM::getMDFR.Pop.Quantities}, and
#'\code{plotMDFR.XY}.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.NatZ.LinePlots<-function(objs,
                                            type=c("N_yxmsz","N_yxmz","N_yxz",
                                                   "iN_xmsz","fN_xmsz"),
                                            dodge=0.2,
                                            ncol=3,nrow=5,
                                            showPlot=FALSE,
                                            pdf=NULL,
                                            verbose=FALSE){
    if (verbose) cat("Starting rCompTCMs::compareResults.Pop.NatZ.LinePlots().\n");
    options(stringsAsFactors=FALSE);

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    types<-c("N_yxmsz","N_yxmz","N_yxz","iN_xmsz","fN_xmsz")
    if (!(type[1] %in% types)){
        cat("rCompTCMs::compareResults.Pop.NatZ.LinePlots: Unknown type requested: '",type[1],"'.\n",sep='');
        return(NULL);
    }

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.Quantities(obj,type=type[1],verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type=type[1],verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type=type[1],verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.character(mdfr$y);
    mdfr$x<-as.character(mdfr$x);
    mdfr$m<-as.character(mdfr$m);
    mdfr$s<-as.character(mdfr$s);
    mdfr$z<-as.numeric(mdfr$z);
    mdfr$case<-factor(mdfr$case,levels=cases);

    #----------------------------------
    #abundance by year
    #----------------------------------
    if (substr(type[1],1,3)=='N_y'){
        plots<-list();
        np<-ncol*nrow;
        uyr<-sort(unique(mdfr$y));
        npg<-ceiling(length(uyr)/np);
        ux<-unique(mdfr$x);
        um<-unique(mdfr$m);
        us<-unique(mdfr$s);
        for (x in ux){
            for (m in um){
                for (s in us){
                    idx<-(mdfr$x==x)&(mdfr$m==m)&(mdfr$s==s);
                    dfr<-mdfr[idx,];
                    if ((!is.null(dfr))&&(nrow(dfr)>0)){
                        title<-paste0("sex= ",x,", maturity= ",m,", shell condition= ",s)
                        cat(paste(x,m,s,sep=", "),"\n");
                        for (pg in 1:npg){
                            idxp<-dfr$y %in% uyr[(1+np*(pg-1)):min(length(uyr),(np*pg))];
                            dfrp<-dfr[idxp,];
                            p<-plotMDFR.XY(dfrp,x='z',agg.formula=NULL,
                                           xlab='size (mm CW)',ylab='Abundance',units="millions",
                                           facet_wrap='y',ncol=ncol,dir='v',
                                           dodge=dodge,title=title,
                                           colour='case',guideTitleColor='',
                                           shape='case',guideTitleShape='');
                            if (showPlot||!is.null(pdf)) print(p);
                            cap<-paste(x,m,s,pg,sep=".");
                            plots[[cap]]<-p;
                        }
                    }
                }
            }
        }
    }
    if (substr(type,1,3)=="iN_"){
        plots<-list();
        p<-plotMDFR.XY(mdfr,x='z',agg.formula=NULL,
                       xlab='size (mm CW)',ylab='Initial Abundance',units="millions",lnscale=FALSE,
                       facet_grid='m+s~x',dodge=dodge,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$A<-p;
        p<-plotMDFR.XY(mdfr,x='z',agg.formula=NULL,
                       xlab='size (mm CW)',ylab='Initial Abundance',units="millions",lnscale=TRUE,
                       facet_grid='m+s~x',dodge=dodge,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$lnA<-p;
    }
    if (substr(type,1,3)=="fN_"){
        plots<-list();
        p<-plotMDFR.XY(mdfr,x='z',agg.formula=NULL,faceting=NULL,
                       xlab='size (mm CW)',ylab='Final Abundance',units="millions",lnscale=FALSE,
                       facet_grid='m+s~x',dodge=dodge,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$A<-p;
        p<-plotMDFR.XY(mdfr,x='z',agg.formula=NULL,faceting=NULL,
                       xlab='size (mm CW)',ylab='Final Abundance',units="millions",lnscale=TRUE,
                       facet_grid='m+s~x',dodge=dodge,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$lnA<-p;
    }


    if (verbose) cat("rCompTCMs::compareResults.Pop.NatZ.LinePlots: Done!\n");
    return(plots)
}

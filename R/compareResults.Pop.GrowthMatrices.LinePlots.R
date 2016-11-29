#'
#'@title Compare growth matrices among several model runs using line plots
#'
#'@description Function to compare growth matrices among several model runs using line plots.
#'
#'@param objs - list of resLst objects
#'@param dodge - width to dodge overlapping series
#'@param ncol - number of columns per page for multi-year plots
#'@param nrow - number of rows per page (nominal) for multi-year plots
#'@param showPlot - flag to print plot to current device
#'@param pdf - name for output pdf file
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@details The line plots are faceted by pre-molt size. If multiple models are compared, then sex-specific plots are
#'created, with individual lines corresponding to different models. If a single model is plotted, the
#'different sexes are plotted using different colors on the same faceted plots.
#'
#'@return list of ggplot objects, by sex
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.GrowthMatrices.LinePlots<-function(objs,
                                                      dodge=0.2,
                                                      ncol=3,nrow=5, 
                                                      showPlot=FALSE,
                                                      pdf=NULL,
                                                      verbose=TRUE){

    options(stringsAsFactors=FALSE);
    
    #create pdf, if necessary
    if (!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    cases<-names(objs);
    
    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.GrowthMatrices(obj,verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.GrowthMatrices(obj,verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.GrowthMatrices(obj,verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$z<-factor(mdfr$z,levels=as.character(mdfr$z));
    mdfr$zp<-as.numeric(mdfr$zp);
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    #----------------------------------
    # plot growth transition matrices
    #----------------------------------
    plots<-list();
    np<-ncol*nrow;
    uz<-sort(unique(mdfr$z));
    uy<-sort(unique(mdfr$y));
    npg<-ceiling(length(uz)/np);
    if (length(cases)>1){
        ux<-unique(mdfr$x);
        for (x in ux){
            idxx<-mdfr$x == x;
            for (y in uy){
                idxy<- mdfr$y==y;
                for (pg in 1:npg){
                    idxp<-mdfr$z %in% uz[(1+np*(pg-1)):min(length(uz),(np*pg))];
                    dfrp<-mdfr[idxp&idxx&idxy,];
                    p<-plotMDFR.XY(dfrp,x='zp',agg.formula=NULL,
                                   xlab='post-molt size (mm CW)',ylab='pr(post-molt size)',units="",
                                   facet_wrap='z',ncol=ncol,dir='v',
                                   dodge=dodge,title=paste(tolower(x),"growth:",uy),
                                   colour='case',guideTitleColor='',
                                   shape='case',guideTitleShape='');
                    if (showPlot||!is.null(pdf)) print(p);
                    cap<-paste(x,y,pg,sep='.');
                    plots[[cap]]<-p;
                }#pg
            }#y
        }#x
    } else {
        for (y in uy){
            idxy<- mdfr$y==y;
            for (pg in 1:npg){
                idxp<-mdfr$z %in% uz[(1+np*(pg-1)):min(length(uz),(np*pg))];
                dfrp<-mdfr[idxp&idxy,];
                p<-plotMDFR.XY(dfrp,x='zp',agg.formula=NULL,
                               xlab='post-molt size (mm CW)',ylab='pr(post-molt size)',units="",
                               facet_wrap='z',ncol=ncol,dir='v',
                               dodge=dodge,title='',
                               colour='x',guideTitleColor='',
                               shape='x',guideTitleShape='');
                if (showPlot||!is.null(pdf)) print(p);
                cap<-as.character(pg);
                plots[[cap]]<-p;
            }#pg
        }#y
    }
    
    return(plots);
}

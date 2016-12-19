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

    if (verbose) cat("Starting rCompTCMs::compareResults.Pop.GrowthMatrices.LinePlots().\n")
    #create pdf, if necessary
    if (!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }
    
    cases<-names(objs);
    
    mdfr<-extractMDFR.Pop.GrowthMatrices(objs,verbose);
    mdfr$z<-floor(mdfr$z);
    mdfr$z<-factor(mdfr$z,levels=as.character(sort(unique(mdfr$z))));
    
    #----------------------------------
    # plot growth transition matrices
    #----------------------------------
    plots<-list();
    np<-ncol*nrow;
    uz<-as.character(sort(unique(as.numeric(as.character(mdfr$z)))));
    #uy<-sort(unique(mdfr$y));
    uy<-paste(unique(mdfr$y),collapse=", ");
    if (verbose) cat("Unique z = ",uz,"\n")
    if (verbose) cat("Unique y = ",uy,"\n")
    npg<-ceiling(length(uz)/np);
    if (length(cases)>1){
        if (verbose) cat("Plotting",length(cases),"\n")
        ux<-unique(mdfr$x);
        for (x in ux){
            if (verbose) cat("--Plotting sex",x,"\n")
            idxx<-mdfr$x == x;
            for (y in uy){
                if (verbose) cat("--Plotting year(s)",y,"\n")
                #idxy<- mdfr$y==y;
                for (pg in 1:npg){
                    idxp<-as.character(mdfr$z) %in% uz[(1+np*(pg-1)):min(length(uz),(np*pg))];
                    #dfrp<-mdfr[idxp&idxx&idxy,];
                    dfrp<-mdfr[idxp&idxx,];
                    p<-plotMDFR.XY(dfrp,x='zp',agg.formula=NULL,
                                   xlab='post-molt size (mm CW)',ylab='pr(post-molt size)',units="",
                                   facet_wrap='z',ncol=ncol,dir='v',
                                   dodge=dodge,title=paste(tolower(x),"growth:",uy),
                                   colour='case',guideTitleColor='',
                                   shape='case',guideTitleShape='');
                    if (showPlot||!is.null(pdf)) print(p);
                    cap<-paste("  \n  \nFigure &&figno. Growth matrices for ",x,"s during ",y,", page ",pg,".  \n  \n",sep='');
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
                cap<-paste("  \n  \nFigure &&figno. Growth matrices for ",y,", page ",pg,".  \n  \n",sep='');
                plots[[cap]]<-p;
            }#pg
        }#y
    }
    
    if (verbose) cat("Finished rCompTCMs::compareResults.Pop.GrowthMatrices.LinePlots().\n")
    return(plots);
}

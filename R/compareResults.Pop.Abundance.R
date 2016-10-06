#'
#'@title Function to compare population abundance estimates by year among several models
#'
#'@description This function compares population abundance estimates by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param type - type of abuundance ("N_yxms","N_yxm","N_yx","iN_xmsz","fN_xmsz")
#'@param numRecent - number of "recent" years to plot
#'@param dodge - width to dodge overlapping series
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details uses \code{rTCSAM2013::getMDFR.PopQuantities}, 
#'\code{rsimTCSAM::getMDFR.Pop.Quantities}, \code{rsimTCSAM::getMDFR.Pop.Quantities}, and 
#'\code{plotMDFR.XY}.
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Abundance<-function(objs,
                                       type=c("N_yxms","N_yxm","N_yx"),
                                       numRecent=15,
                                       dodge=0.2,
                                       showPlot=TRUE,
                                       pdf=NULL,
                                       verbose=TRUE){
    if (verbose) cat("rCompTCMs::compareResults.Pop.Abundance: Plotting abundance.\n");
    
    types<-c("N_yxms","N_yxm","N_yx");
    if (!(type[1] %in% types)){
        cat("rCompTCMs::compareResults.Pop.Abundance: Unknown type requested: '",type[1],"'.\n",sep='');
        return(NULL);
    }
    
    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(dev.off());
        showPlot<-TRUE;
    }

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.PopQuantities(obj,type=type[1],verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type=type[1],verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type=type[1],verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$z<-as.numeric(mdfr$z);
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    idx<-mdfr$y>=(max(mdfr$y)-numRecent);
    
    #----------------------------------
    #abundance by year
    #----------------------------------
    plots<-list();
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Abundance',units="millions",lnscale=FALSE,
                   facet_grid='m+s~x',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$A<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Abundance',units="millions",lnscale=FALSE,
                   facet_grid='m+s~x',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$RA<-p;
    
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Abundance',units="millions",lnscale=TRUE,
                   facet_grid='m+s~x',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$lnA<-p;
    p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Abundance',units="millions",lnscale=TRUE,
                   facet_grid='m+s~x',dodge=dodge,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$lnRA<-p;

    if (verbose) cat("rCompTCMs::compareResults.Pop.Abundance: Done!\n");
    return(plots)
}

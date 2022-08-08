#'
#'@title Function to compare initial/final population abundance estimates among several models
#'
#'@description This function compares initial/final population abundance estimates
#'   among several models.
#'
#'@param objs - list of resLst objects
#'@param type - type of abuundance ("iN_xmsz","fN_xmsz")
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
compareResults.Pop.IF.NatZ<-function(objs,
                                     type=c("iN_xmsz","fN_xmsz"),
                                     dodge=0.2,
                                     showPlot=FALSE,
                                     pdf=NULL,
                                     verbose=FALSE){
    if (verbose) cat("Strating rCompTCMs::compareResults.Pop.IF.NatZ().\n");
    options(stringsAsFactors=FALSE);

    type<-type[1];
    types<-c("iN_xmsz","fN_xmsz");
    if (!(type %in% types)){
        cat("rCompTCMs::compareResults.Pop.IF.NatZ: Unknown type requested: '",type[1],"'.\n",sep='');
        return(NULL);
    }

    cases<-names(objs);

    #create pdf, if necessary
    if(!is.null(pdf)){
        pdf(file=pdf,width=11,height=8,onefile=TRUE);
        on.exit(grDevices::dev.off());
        showPlot<-TRUE;
    }

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$z<-as.numeric(mdfr$z);
    mdfr$case<-factor(mdfr$case,levels=cases);

        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
    if (type=='iN_xmsz') ylab<-'Initial Population Abundance\n';
    if (type=='fN_xmsz') ylab<-'Final Population Abundance\n';
    p<-plotMDFR.XY(mdfr,x='z',value.var='val',agg.formula=NULL,
                   facet_grid=formula(m+s~x),
                   xlab='size (mm CW)',ylab=ylab,units='millions',lnscale=FALSE,
                   colour='case',guideTitleColor='',
                   shape='case',guideTitleShape='',
                   showPlot=FALSE);
    if (showPlot||!is.null(pdf)) print(p);

    if (verbose) cat("rCompTCMs::compareResults.Pop.IF.NatZ(): Done!\n");
    return(p)
}

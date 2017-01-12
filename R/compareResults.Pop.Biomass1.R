#'
#'@title Function to compare population biomass estimates by year among several models
#'
#'@description This function compares biomass estimates (aggregated or as size comps, depending on "type") by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param type - type of biomass ("B_yxmsz","B_yxmz","B_yxz","B_yxms","B_yxm","B_yx")
#'@param years - "all" or numerical vector of years to include
#'@param numRecent - number of "recent" years to plot
#'@param facet_grid - formula for faceting using facet_grid
#'@param facet_wrap - formula for faceting using facet_wrap
#'@param scales - ggplot2 scales option for facet_grid
#'@param dodge - width to dodge overlapping series
#'@param mxy - max number of years per page
#'@param nrow - number of rows per page, when facet_wrap'ing 
#'@param showPlot - flag (T/F) to show plot
#'@param pdf - creates pdf, if not NULL
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return ggplot2 object
#'
#'@details  This function compares biomass estimates (aggregated or as size comps, depending on "type") by year
#'   among several models. It uses \code{rTCSAM2013::getMDFR.Pop.Quantities}, 
#'\code{rsimTCSAM::getMDFR.Pop.Quantities}, and to extract model results, and \code{rsimTCSAM::getMDFR.Pop.Quantities}, and 
#'\code{plotMDFR.XY} to plot them. The level of aggregation is based on the value for "type" (unlike 
#'\code{compreResults.Pop.Biomass}, where a cast'ing formula is specified.) 
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Biomass1<-function(objs,
                                      type=c("B_yxmsz","B_yxmz","B_yxz","B_yxms","B_yxm","B_yx"),
                                      years='all',
                                      numRecent=15,
                                      facet_grid=NULL,
                                      facet_wrap=NULL,
                                      scales="fixed",
                                      dodge=0.2,
                                      mxy=15,
                                      nrow=5,
                                      showPlot=FALSE,
                                      pdf=NULL,
                                      verbose=FALSE){
    if (verbose) cat("starting rCompTCMs::compareResults.Pop.Biomass1().\n");
    options(stringsAsFactors=FALSE);
    
    type<-type[1];
    types<-c("B_yxmsz","B_yxmz","B_yxz","B_yxms","B_yxm","B_yx");
    if (!(type %in% types)){
        cat("rCompTCMs::compareResults.Pop.Biomass1: Unknown type requested: '",type,"'.\n",sep='');
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
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        if (!is.null(mdfr1)) mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    if (is.numeric(years)) mdfr<-mdfr[mdfr$y %in% years,];
    
    idx<-mdfr$y>=(max(mdfr$y)-numRecent);
    
    if (sum(grep('z',type,fixed=TRUE))==0){
        #----------------------------------
        #biomass by year
        #----------------------------------
        plots<-list();
        p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Biomass',units="1000's t",lnscale=FALSE,
                       facet_grid='m+s~x',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        cap<-paste0("\n  \nFigure &&figno. Population biomass trends.  \n  \n")
        plots[[cap]]<-p;
        p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Biomass',units="1000's t",lnscale=FALSE,
                       facet_grid='m+s~x',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        cap<-paste0("\n  \nFigure &&figno. Recent population biomass trends.  \n  \n")
        plots[[cap]]<-p;
        
        p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Biomass',units="1000's t",lnscale=TRUE,
                       facet_grid='m+x~s',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        cap<-paste0("\n  \nFigure &&figno. Ln-scale population biomass trends.  \n  \n")
        plots[[cap]]<-p;
        p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Biomass',units="1000's t",lnscale=TRUE,
                       facet_grid='m+x~s',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        cap<-paste0("\n  \nFigure &&figno. Recent ln-scale population biomass trends.  \n  \n")
        plots[[cap]]<-p;
    } else {
        #plot size comps by year
        if (verbose) cat("Plotting size comps\n")
        plots<-list();
        mdfr$z<-as.numeric(mdfr$z);
        uY<-sort(unique(mdfr$y));
        for (pg in 1:ceiling(length(uY)/mxy)){
            mdfrp<-mdfr[mdfr$y %in% uY[(1+mxy*(pg-1)):min(length(uY),mxy*pg)],];
            p<-plotMDFR.XY(mdfrp,x='z',value.var='val',agg.formula=NULL,
                           facet_grid=facet_grid,facet_wrap=facet_wrap,nrow=nrow,
                           xlab='size (mm CW)',ylab='Population Biomass',units="1000's t",lnscale=FALSE,
                           colour='case',guideTitleColor='',
                           shape='case',guideTitleShape='',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            cap<-paste0("\n  \nFigure &&figno. Population biomass size compositions (",type,"; ",pg," of ",ceiling(length(uY)/mxy),").  \n  \n")
            plots[[cap]]<-p;
        }#pg
    }

    if (verbose) cat("rCompTCMs::compareResults.Pop.Biomass1: Done!\n");
    return(plots)
}

#'
#'@title Function to compare population abundance estimates by year among several models
#'
#'@description This function compares population abundance estimates (aggregated or as size comps, depending on "type") by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param type - type of abuundance ("N_yxmsz","N_yxmz","N_yxz","N_yxms","N_yxm","N_yx")
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
#'@details This function compares population abundance estimates (aggregated or as size comps, depending on "type") by year
#'   among several models. It uses \code{rTCSAM2013::getMDFR.Pop.Quantities}, 
#'\code{rsimTCSAM::getMDFR.Pop.Quantities}, and to extract model results, and \code{rsimTCSAM::getMDFR.Pop.Quantities}, and 
#'\code{plotMDFR.XY} to plot them. The level of aggregation is based on the value for "type" (unlike 
#'\code{compreResults.Pop.Abundance}, where a cast'ing formula is specified.) 
#'
#'@import ggplot2
#'
#'@export
#'
compareResults.Pop.Abundance1<-function(objs,
                                       type=c("N_yxmsz","N_yxmz","N_yxz","N_yxms","N_yxm","N_yx"),
                                       years='all',
                                       numRecent=15,
                                       facet_grid=NULL,
                                       facet_wrap=NULL,
                                       scales='fixed',
                                       dodge=0.2,
                                       mxy=15,
                                       nrow=5,
                                       showPlot=TRUE,
                                       pdf=NULL,
                                       verbose=TRUE){
    if (verbose) cat("starting rCompTCMs::compareResults.Pop.Abundance1().\n");
    
    type<-type[1];
    types<-c("N_yxmsz","N_yxmz","N_yxz","N_yxms","N_yxm","N_yx");
    if (!(type %in% types)){
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
        if (inherits(obj,"tcsam2013.resLst")) mdfr1<-rTCSAM2013::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type=type,verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y);
    mdfr$case<-factor(mdfr$case,levels=cases);
    
    if (is.numeric(years)) mdfr<-mdfr[mdfr$y %in% years,];
    
    idx<-mdfr$y>=(max(mdfr$y)-numRecent);
    
    if (sum(grep('z',type,fixed=TRUE))==0){
        #----------------------------------
        #abundance by year
        #----------------------------------
        plots<-list();
        p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Abundance',units="millions",lnscale=FALSE,
                       facet_grid='m+s~x',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$A<-p;
        p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Abundance',units="millions",lnscale=FALSE,
                       facet_grid='m+s~x',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$RA<-p;
        
        p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Abundance',units="millions",lnscale=TRUE,
                       facet_grid='m+s~x',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$lnA<-p;
        p<-plotMDFR.XY(mdfr[idx,],x='y',agg.formula=NULL,faceting=NULL,
                       xlab='year',ylab='Abundance',units="millions",lnscale=TRUE,
                       facet_grid='m+s~x',dodge=dodge,scales=scales,
                       colour='case',guideTitleColor='',
                       shape='case',guideTitleShape='');
        if (showPlot||!is.null(pdf)) print(p);
        plots$lnRA<-p;
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
                           xlab='size (mm CW)',ylab='Population Abundance',units='millions',lnscale=FALSE,
                           colour='case',guideTitleColor='',
                           shape='case',guideTitleShape='',
                           showPlot=FALSE);
            if (showPlot||!is.null(pdf)) print(p);
            plots[[paste(type,pg,sep=".")]]<-p;
        }#pg
    }

    if (verbose) cat("rCompTCMs::compareResults.Pop.Abundance1: Done!\n");
    return(plots)
}

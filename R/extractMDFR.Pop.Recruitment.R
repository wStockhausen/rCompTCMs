#'
#'@title Function to extract recruitment estimates by year among several models
#'
#'@description This function extracts recruitment estimates by year
#'   among several models.
#'   
#'@param objs - list of resLst objects
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return dataframe in canonical format
#'
#'@details None.
#'
#'@export
#'
extractMDFR.Pop.Recruitment<-function(objs,
                                      verbose=TRUE){
    if (verbose) cat("Starting rCompTCMs::extractMDFR.Pop.Recruitment().\n");
    options(stringsAsFactors=FALSE);
    
    cases<-names(objs);

    mdfr<-NULL;
    for (case in cases){
        obj<-objs[[case]];
        if (verbose) cat("Processing '",case,"', a ",class(obj)[1]," object.\n",sep='');
        if (inherits(obj,"tcsam2013.resLst")) {
            mdfr1<-rTCSAM2013::getMDFR.Pop.Recruitment(obj,verbose);
            mdfr1$y<-as.numeric(mdfr1$y);
            mdfr1$y<-mdfr1$y-1;#adjust to TCSAM02 sense for timing of recruitment
        }
        if (inherits(obj,"rsimTCSAM.resLst")) mdfr1<-rsimTCSAM::getMDFR.Pop.Quantities(obj,type="R_y",verbose=verbose);
        if (inherits(obj,"tcsam02.resLst"))   mdfr1<-rTCSAM02::getMDFR.Pop.Quantities(obj,type="R_y",verbose=verbose);
        mdfr1$case<-case;
        mdfr<-rbind(mdfr,mdfr1);
    }
    mdfr$y<-as.numeric(mdfr$y)
    mdfr$case<-factor(mdfr$case,levels=cases);

    if (verbose) cat("rCompTCMs::extractMDFR.Pop.Recruitment: Done!\n");
    return(mdfr)
}

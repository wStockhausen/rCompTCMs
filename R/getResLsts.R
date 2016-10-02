#'
#'@title Create a list of TCSAM2013, TCSAM02 and rsimTCSAM resLst objects from a set of model runs
#'
#'@description Function to create a list of TCSAM2013, TCSAM02 and rsimTCSAM resLst objects from a set of model runs.
#'
#'@param inp.dirs - a dataframe with columns "case", "modelType", "path", "repType", "modelName", "prsType"
#'@param verbose - flag (T/F) to print diagnostic information
#'
#'@return a named list of the appropriate resLst objects.
#'
#'@details Uses \code{getResLst} to open a folders with model runs listed in inp.dirs.
#'
#'@export
#'
getResLsts<-function(inp.dirs=NULL){
    if (is.null(inp.dirs)){
        cat("Warning from getResLsts(...).\n");
        cat("--The dataframe inp.dirs cannot be NULL.\n",sep='');
        cat("--Returning NULL.\n")
        return(NULL);
    }

    nr<-nrow(inp.dirs);
    lst<-list();
    resLst<-NULL;
    cat("rCompTCMs:getResLsts(): Creating ",nr," resLst objects.\n",sep='');
    for (r in 1:nr){
        rw<-inp.dirs[r,];
        cat("rCompTCMs:getResLsts(): Creating resLst[",r,"]: \n\t'",rw$path,"'\n",sep='');
        if (tolower(rw$modelType)==tolower("TCSAM2013")) resLst<-rTCSAM2013::getResLst(rw$path,rw$rep,rw$model,rw$prsType);
        if (tolower(rw$modelType)==tolower("rsimTCSAM")) resLst<-rsimTCSAM::getResLst(rw$path);
        if (tolower(rw$modelType)=="tcsam02")   resLst<-rTCSAM02::getResLst(rw$path,rw$rep,rw$model,rw$prsType);
        if (!is.null(resLst)){
            lst[[rw$case]]<-resLst;
            resLst<-NULL;
        }
    }

    cat("rCompTCMs:getResLsts(): Done!\n",sep='');
    return(lst);
}

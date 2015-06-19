#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 2014
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# author__ = "SPSS, JKP"
# version__ = "1.0.1"

# History
# 29-apr-2014 Original Version


helptext='STATS BAGPLOT VARIABLES=list of variables
CASELIMIT=number
/OPTIONS FENCE=number OUTLIERS=YES or NO
    WHISKERS=YES or NO LOOPPOINTS=YES or NO
    BAGPOINTS=YES or NO POINTSIZE=number
    TITLE="title text"

Example:
STATS BAGPLOT VARIABLES=salary educ.

This command produces a bagplot.  Bagplots are two-dimensional 
versions of the boxplot.  Instead of a box, the bagplot displays 
a convex polygon that contains half of the points (the bag).  
The loop is a polygon that contains the non-outlier points.
Outlier points are those beyond the fence multiple of the bag.

If there are more than two variables, a matrix of bagplots is
produced.

Split files mode is supported for this procedure.  Missing
values are always excluded.

VARIABLES specifies the names of the variables to be plotted.
Nominal-level variables cannot be used, and if there are value
labels, no two values of a variable should have the same label.

CASELIMIT specifies the number of cases above which some statistics
will be computed based on a sample of the data.  The default is 300.

FENCE specifies the multiple of the bag polygon outside of which
points are classified as outliers.  The default is 3.

TITLE optionally specifies a title for the plot.  If there are more
than two variables, the split information is used as the title
if no TITLE was specified.

POINTSIZE specifies a scale factor to increase or decrease the
default size of points.  The default is .6.

The remaining parameters are ignored if there are more than
two variables, because the R package does not honor them.

OUTLIERS determines whether outliers are plotted.

WHISKERS specifies whether whiskers are drawn.

LOOPPOINTS specifies whether the points between the bag and
loop boundaries are drawn.

BAGPOINTS specifies whether points within the bag boundary
are drawn.

STATS BAGPLOT /HELP.  prints this information and does nothing else.
'

### MAIN ROUTINE ###
dobagplot = function(variables, fence=3, outliers=TRUE, whiskers=TRUE, looppoints=TRUE,
    caselimit=300, pointsize=.6, bagpoints=TRUE, title=NULL) {
    # Calculate inequality measures and lorenz curve
    
    setuplocalization("STATS_BAGPLOT")
    
    # A warnings proc name is associated with the regular output
    # (and the same omsid), because warnings/errors may appear in
    # a separate procedure block following the regular output
    procname=gtxt("Bagplot")
    warningsprocname = gtxt("Bagplot: Warnings")
    omsid="STATSBAGPLOT"
    warns = Warn(procname=warningsprocname,omsid=omsid)

    tryCatch(library(aplpack), error=function(e){
        warns$warn(gtxtf("The R %s package is required but could not be loaded.", "aplpack"),dostop=TRUE)
        }
    )
    nvars = length(variables)
    if (nvars < 2 ||nvars > 10) {
        warns$warn(gtxt("The number of variables must be between two and ten."), dostop=TRUE)
    }
    titlespecified = !is.null(title)
    if (is.null(title) && nvars == 2) {
        title=gtxtf("Bagplot of %s by %s", variables[[2]], variables[[1]])
    }
    splits = spssdata.GetSplitVariableNames()
    nsplitvars = length(splits)
    sub = NULL
    vardict = spssdictionary.GetDictionaryFromSPSS(variables)
    if ("nominal" %in% vardict["varMeasurementLevel",]) {
        warns$warn(gtxt("Variables with a nominal measurement level cannot be used in this plot"),
                   dostop=TRUE)
    }
    if (!is.null(splits)) {
        catdict = spssdictionary.GetCategoricalDictionaryFromSPSS(splits)
    }
    sink(file=tempfile()) # suppress some verbose debugging output from bagplot function
    while (!spssdata.IsLastSplit()) {

        # with split files, plotting data and split variables must be
        # retrieved separately because bagplot functions do not do well with
        # factors.  Split variable information is only used for labelling

        dta = spssdata.GetSplitDataFromSPSS(c(variables, splits), missingValueToNA=TRUE,
            factorMode="none")

        if (!is.null(splits)) {
            splitlabels=getsplitinfo(catdict, splits, 
                dta[1, (nvars+1):(nvars + nsplitvars)])
            sub = paste(splits, splitlabels, 
                sep="=", collapse=", ")
        }
        if (nvars == 2) {
            tryCatch(bagplot(dta[[1]], dta[[2]], factor=fence, na.rm=TRUE,
                xlab=variables[[1]], ylab=variables[[2]], cex=pointsize,
                approx.limit=caselimit, show.outlier=outliers,
                show.whiskers=whiskers, show.looppoints=looppoints,
                show.bagpoints=bagpoints, main=title, sub=sub),
                error=function(e) {warns$warn(e$message, dostop=FALSE)},
                warning=function(e){warns$warn(e$message, dostop=FALSE)}
            )
        } else {
            if (!is.null(splits) && !titlespecified) {
                title = sub
            }
            # many parameters do not work with bagplot.pairs
            # missing data must be removed as bagplot.pairs fails
            # erratically if it is present
            dta = dta[complete.cases(dta),]
            if (nrow(dta) > 1) {
                tryCatch(
                    bagplot.pairs(dta[1:nvars], factor=fence,
                        cex=pointsize, main=title, approx.limit=caselimit),
                    error=function(e) {warns$warn(paste(gtxt(
                        "A computational error has occurred.  A plot cannot be produced."),
                        e$message, sep="--"),
                        dostop=FALSE)},
                    warning=function(e){warns$warn(e$message, dostop=FALSE)}
                )

            }
        }
    }
    sink(file=NULL)
    spssdata.CloseDataConnection()
    warns$display()

}
getsplitinfo = function(catdict, splits, values) {
    # return values or labels as available for split variables
    
    # catdict is a categorical dictionary that includes the
    # categorical variables in the split list, but the list could
    # contain non-categorical variables
    # splits is a list of the split variables
    # values is a list of the values to map
    
    labellist = list()
    count = 0    # for each split variable, extract a value label if available
    # otherwise use the value
    for (v in 1:length(splits)) {
        if (splits[[v]] %in% catdict$name) {
            count = count + 1
            thematch =match(values[[v]], catdict$dictionary[[count]]$levels)
            if (!is.na(thematch)) {
                label = catdict$dictionary[[count]]$labels[[thematch]]
            } else {
                label = values[[v]]
            }
            labellist[[v]] = label
        } else {
            labellist[[v]] = values[[v]]
        }
    }
    return(labellist)
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment

    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.

        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 

        if (is.null(msg) || dostop) {
            lcl$display(inproc)  # display messages and end procedure state
            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any

        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spss.EndProcedure()
            }
        } else {
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    TRUE
                    },
                    error = function(e) {
                        FALSE
                    }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                    gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory, 
                        spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}

# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 
# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}

gtxt <- function(...) {
    return(gettext(...,domain="STATS_BAGPLOT"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_BAGPLOT"))
}


Run = function(args) {
    #Execute the STATS BAGPLOT command
    
    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("VARIABLES", subc="",  ktype="existingvarlist", 
            var="variables", islist=TRUE),
        spsspkg.Template("CASELIMIT", subc="", ktype="int", var="caselimit"),
        
        spsspkg.Template("FENCE", subc="OPTIONS", ktype="float", var="fence",
            vallist=list(0.1)),
        spsspkg.Template("OUTLIERS", subc="OPTIONS", ktype="bool", var="outliers"),
        spsspkg.Template("WHISKERS", subc="OPTIONS", ktype="bool", var="whiskers"),
        spsspkg.Template("LOOPPOINTS", subc="OPTIONS", ktype="bool", var="looppoints"),
        spsspkg.Template("BAGPOINTS", subc="OPTIONS", ktype="bool", var="bagpoints"),
        spsspkg.Template("POINTSIZE", subc="OPTIONS", ktype="float", var="pointsize",
            vallist=list(0,5)),
        spsspkg.Template("TITLE", subc="OPTIONS", ktype="literal", var="title")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "dobagplot")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}
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
# version__ = "1.0.2"

# History
# 03-apr-2014 Original Version
# 07-apr-2014 time series support


helptext='Get information about an R workspace or create
an SPSS dataset from an R data frame.

STATS GET R
FILE="filespec" or * CLEARENV=YES or NO
/INFO NAMES=list of names or . CLASSES=list of R class types or .
/GET DATAFRAME=dataframe name SERIES=list of series DATASET=SPSS dataset name.

Example:
STATS GET R FILE="c:/data/work.Rdata"
/GET DATAFRAME=results DATASET=results.

FILE specifies the name of the R workspace file.  * can be used to refer to
a workspace file previously loaded by this command in the current session.

CLEARENV, which defaults to YES, specifies whether or not to remove the
contents of the loaded workspace after the command is run.

You can use either the INFO subcommand or the GET subcommand in one
invocation but not both.  Typically a user would start by using INFO
to find the contents of a workspace and then GET to extract a data frame
and make an SPSS dataset.

If neither GET nor INFO is specified, contents information is displayed.

The INFO subcommand produces a pivot table displaying the contents of
the workspace filtered by NAMES and CLASSES specifications.
NAMES can be a list of names or one or more regular expressions.  For example
NAMES=abc def would limit the information to R items with those names.
NAMES="a.*" "b.*" would limit the output to names starting with a or
starting with b.  (The two regular expressions could, of course, be
combined into one in this example.)
CLASSES can specify a list of R object classes to limit the output.
Any classes can be listed, but the default is data.frame.

NOTE: Since R names are case sensitive, you must use the appropriate case
in setting these filters.

Use a period to indicate all names or all classes.

The GET subcommand is used to create an SPSS dataset from an R data frame.
Specify the name via DATAFRAME or specify one or more series that can be
combined into a data frame via SERIES.

Specify the SPSS dataset name to be assigned to the new dataset with DATASET.
The dataset name must not already be in use, and the current active file
should have a dataset name in order to keep it open when the new dataset
is created.

While most R names will be legal in Statistics, in case of difficulty enclose
the names in quotes.  When a dataset is created, a reasonable attempt is
made to make the data frame variable names into legal SPSS names.  If
the name is modified, the orginal name becomes the variable label.

If the series are all consistent time series, the time points
are used as the row names.  Otherwise the series are just numbered.

STATS GET R /HELP.  prints this information and does nothing else.
'

### MAIN ROUTINE ###
dogetr = function(rfile=NULL, rdata=NULL, package=NULL, names=".", classes=list("data.frame"),
        dataframe=NULL, series=NULL, dataset=NULL, alldata=FALSE,
        clearenv=FALSE) {
    # get data from an R workspace
    
    setuplocalization("STATS_GET_R")
    
    # A warnings proc name is associated with the regular output
    # (and the same omsid), because warnings/errors may appear in
    # a separate procedure block following the regular output
    procname=gtxt("Get R Data")
    warningsprocname = gtxt("Get R Data: Warnings")
    omsid="STATSGETR"
    warns = Warn(procname=warningsprocname,omsid=omsid)
    outputreq = !is.null(dataframe) || !is.null(series)
    if (!is.null(dataset) != outputreq) {
        warns$warn(gtxt("A dataset was specified without items to fetch or items were specified without a dataset"),
            dostop=TRUE)
    }
    # * can be used to refer to the currently loaded workspace
    if (!alldata && ((is.null(rfile) && is.null(rdata)) || (!is.null(rfile) && !is.null(rdata)))) {
        warns$warn(gtxt("Exactly one data source must be specified"), dostop=TRUE)
    }
    if (length(rfile) > 1 || length(rdata) > 1) {
        warns$warn(gtxt("Only one file or data source can be named"), dostop=TRUE)
    }
    thesource = NULL
    if (!is.null(rfile) && rfile=="*") {
        thesource = rfile
        if (exists("STATS_GET_R_Marker", envir=.GlobalEnv)) {
            thisenv = STATS_GET_R_Marker
        } else {
            thisenv = .GlobalEnv
        }
    } else {
            thisenv = new.env()
            attr(thisenv, "name") = "STATS_GET_R_Marker"
            if (!is.null(rfile)) {
                tryCatch(load(file=rfile, envir=thisenv),
                    error=function(e) {warns$warn(e$message, dostop=TRUE)}
                )
                thesource = rfile
            } else {
                if (!is.null(rdata)) {
                # need to call data roundaboutly because it takes the first argument literally
                tryCatch(do.call(data, list(rdata, package=package, envir=thisenv)),
                    error=function(e) {warns$warn(e$message)}, dostop=TRUE
                )
                thesource = rdata
                }
            }
            assign("STATS_GET_R_Marker", thisenv, envir=.GlobalEnv)
    }

    if (!is.null(dataset)) {
        # get a data frame as an SPSS dataset
        getitem(dataset, dataframe, series, thisenv, warns)
    } else {
        # get information about the workspace
        displayresult(getinfo(thesource, names, classes, thisenv, warns),
            thesource, procname, omsid, alldata, warns)
    }
    if (clearenv) {
        rm(list=ls(envir=thisenv), envir=thisenv)
        rm(STATS_GET_R_Marker, envir=.GlobalEnv)
    }
}

getitem = function(dataset, dataframe, series, envir, warns) {
    # construct a dataset for the dataframe object or the listed series
    
    dfname = dataframe
    if (!is.null(dataframe) && !is.null(series)) {
        warns$warn(gtxt("Cannot request both a data frame and series"), dostop=TRUE)
    }
    if (dataset %in% spssdata.GetDataSetList()) {
        warns$warn(gtxt("The output dataset name is already in use."), dostop=TRUE)
    }
    if (!is.null(series)) {
        # build a data frame from one or more series
        # special handling for time series row names
        s = tryCatch(
            get(series[[1]], envir=envir),
            error=function(e) {warns$warn(e$message, dostop=TRUE)}
        )
        lengths = length(s)
        # if these are time series, and they all have consistent times
        # use the time stamps as the row names.
        if (is.ts(s)) {
            sdataframe = data.frame(row.names=as.character(time(s)))
            rownamesaretimes = TRUE
            first = start(s)
            freq = frequency(s)
        } else {
            sdataframe = data.frame(row.names=1:lengths)
            rownamesaretimes = FALSE
        }
        for (v in 1:length(series)) {
            thisseries = tryCatch(
                get(series[[v]], envir=envir),
                error=function(e) {warns$warn(e$message, dostop=TRUE)}
            )
            if (length(thisseries) != lengths) {
                warns$warn(gtxt("The series vary in length.  All series must have the same length."), dostop=TRUE)
            }
            sdataframe[series[[v]]] = thisseries
            if (rownamesaretimes) {
                if (start(thisseries) != first || frequency(thisseries) != freq) {
                    # series are inconsistent
                    row.names(sdataframe) = 1:lengths
                    rownamesaretimes = FALSE
                }
            }
        }
        assign("__dataframe__", sdataframe, envir=envir)
        dataframe = "__dataframe__"
    } else {
        if (!exists(dataframe, envir=envir)) {
            warns$warn(gtxtf("The specified data frame was not found: %s", dataframe), dostop=TRUE)
        }
    }
    dataframe = get(dataframe, envir=envir)
    if (!is.data.frame(dataframe)) {
        warns$warn(gtxtf("The specified workspace item is not a dataframe: %s", dfname), dostop=TRUE)
    }

    # build variable dictionary
    dict = list()
    dfnames = names(dataframe)
    rownames = "row.names"
    while (rownames %in% dfnames) {
        rownames = paste("row.names", rpois(1, 500), sep="")
    }
    # row names will always be nominal strings
    rnlength = max(nchar(row.names(dataframe)[[1]])) * 3 #unicode worst-case expansion
    dict[[1]] = c(rownames, "", rnlength, paste("A", rnlength, sep=""), "nominal")

    for (v in 1:length(dataframe)) {
        meta = gettype(dataframe[v])
        dict[[v+1]] = c(dfnames[v], "", meta[["type"]], meta[["format"]], meta[["measure"]])
        # TODO: categorydictionary
    }
    dict = fixnames(dict)
    dict = spssdictionary.CreateSPSSDictionary(dict)
    spssdictionary.SetDictionaryToSPSS(dataset, dict)
    spssdata.SetDataToSPSS(dataset, data.frame(row.names(dataframe), dataframe))
    spssdictionary.EndDataStep()
}

gettype = function(column) {
    # return value type to use, meas level, and format
    
    result = list()

    if (is.factor(column[[1]])) {
        result["type"] = max(nchar(as.character(column[[1]]))) * 3  #utf-8 expansion factor
        result["format"] = paste("A", result[["type"]], sep="")
        if (is.ordered(column)) {
            result["measure"] = "ordinal"
        } else {
            result["measure"] = "nominal"
        }
    } else if (mode(column[[1]]) == "character") {
        result["type"] = max(nchar(as.character(column[[1]]))) * 3
        result["format"] = paste("A", result[["type"]], sep="")
        result["measure"] = "nominal"
    } else {
         result["type"] = 0
         result["format"] = "F8.2"
         result["measure"] = "scale"
    }
    
    return(result)
}
reserved = c('all','and', 'by','eq','ge','gt','le','lt','ne','not','or','to','with')
fixnames = function(dict) {
    # fix R variable names to be valid as SPSS names
    thenames = sapply(dict, function(item) {return(item[1])})
    thenamescopy = thenames
    for (v in 1:length(thenames)) {
        # invalid characters (most of them)
        thenames[v] = gsub("[][+/*() \"\t'),=-]", "_", thenames[v])
        # reserved words
        if (!is.na(match(tolower(thenames[v]), reserved))) {
            thenames[v] = paste(substring(thenames[v], 1, 1), "_", substring(thenames[v], 2), sep="")
        }
        # regular first character
        if (substring(thenames[v],1,1) %in% list("$", "#", "@", "_")) {
            thenames[v] = paste('x', thenames[v], sep="")
        }
        # unique names ignoring case
        name = thenames[v]
        lowernames = lapply(thenames, tolower)
        while (v > 1 && tolower(name) %in% lowernames[1:(v-1)]) {
            name = paste(thenames[v], rpois(1,100), sep="")
        }
        thenames[v] = name
        }
    # set new names and create value labels for the originals if changed
    for (v in 1:length(thenames)) {
        if (thenames[v] != thenamescopy[v]) {
            dict[[v]][1] = thenames[v]
            dict[[v]][2] = thenamescopy[v]
        }
    }
    return(dict)
}

getinfo = function(rfile, names, classes, thisenv, warns) {
    # print information about the contents of selected objects
    # rfile is the file name (or *) of the loaded workspace
    # names is a list of names or wildcard expressions for items
    # classes is a list of object types to report on
    # thisenv is the environment object
    # warns is the warnings object
    if (!is.null(rfile)) {
        df = data.frame(character(), character(), character(), stringsAsFactors=FALSE)
        
        if ("." %in% names) {  # no name filter
            names = NULL
        }
    
        for (itemname in ls(envir=thisenv)) {
            item = get(itemname, envir=thisenv)
            iteminfo = selected(item, itemname, names, classes)
            if (!is.null(iteminfo)) {
                if (iteminfo[[2]] == "data.frame") {
                    iteminfo[[3]] = paste(names(item), collapse=" ")
                } else {
                    if (is.ts(item)) {
                        st = start(item)
                        ed = end(item)
                        iteminfo[[3]] = sprintf("Start: %s:%s, End: %s:%s, Frequency: %s", 
                            st[[1]], st[[2]], ed[[1]], ed[[2]], frequency(item))
                    } else{
                    iteminfo[[3]] = ""
                    }
                }
            df[nrow(df) + 1,] = iteminfo
            }
    
    
        }
        names(df) = c(gtxt("Item"), gtxt("Class"), gtxt("Series"))
        return(df)
    } else {
        return(NULL)
    }
}

selected = function(item, thename, names, classes) {
    # return name and class info or NULL
    # names is a list of fixed strings and/or regular expressions for filtering
    # classes is a list of classes for filtering
    
    okay = TRUE
    itemclass = class(item)
    
    # type test
    if (!is.null(classes) && !("." %in% classes)) {
        okay = !is.na(match(itemclass, classes))
    }  
    # name test

    tokay = TRUE
    if (okay && !is.null(names)) {
            tokay = FALSE
            if (!is.na(match(thename, names))) {
                tokay = TRUE
            } else {
                for (pattern in names) {
                    if (tryCatch(grepl(pattern, thename),
                        error=function(e) {return(FALSE)},
                        warning=function(e) {return(FALSE)}
                    )) {
                        tokay = TRUE
                        break
                    }
                }
            }
    }
    if (okay && tokay) {
        return(list(thename, itemclass))
    } else {
        return(NULL)
    }
}

displayresult = function(df, rsource, procname, omsid, alldata, warns) {
    # Display result, if any
    # df is a data frame of result information
    # procname and omsid are the procedure name and oms id
    
    if (!is.null(df)) {
        if (nrow(df) == 0) {
            warns$warn(gtxt("No items match the selection criteria"), dostop=TRUE)
        } else {
            spsspivottable.Display(df, title=gtxt("R Workspace Information for Selected Items"),
                templateName="GETRINFO",
                outline=gtxt("Workspace Information"),
                caption=gtxtf("Source: %s", rsource)
            )
        }
    }
    if (alldata) {
        rdata = data(package=.packages(all.available=TRUE))
        ddf = data.frame(rdata$results[,1], rdata$results[,3], rdata$results[,4])
        names(ddf) = c(gtxt("Package"), gtxt("Data"), gtxt("Title"))
        spsspivottable.Display(ddf, title=gtxt("R Datasets"),
            templateName="GETRDATAINFO",
            outline=gtxt("R Datasets"),
            caption=gtxt("To access these datasets, use the DATA keyword instead of FILE.  Datasets in non-base packages should also specify PACKAGE.")
        )
    }
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
    return(gettext(...,domain="STATS_GET_R"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_GET_R"))
}


Run = function(args) {
    #Execute the STATS GET R command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("FILE", subc="",  ktype="literal", var="rfile"),
        spsspkg.Template("DATA", subc="", ktype="literal", var="rdata"),
        spsspkg.Template("PACKAGE", subc="", ktype="literal", var="package"),
        spsspkg.Template("ALLDATA", subc="", ktype="bool", var="alldata"),
        spsspkg.Template("CLEARENV", subc="", ktype="bool", var="clearenv"),
        
        spsspkg.Template("NAMES", subc="INFO", ktype="literal", var="names", islist=TRUE),
        spsspkg.Template("CLASSES", subc="INFO", ktype="str", var="classes",
            islist=TRUE),

        
        spsspkg.Template("DATAFRAME", subc="GET",  ktype="literal", var="dataframe"),
        spsspkg.Template("SERIES", subc="GET", ktype="literal", var="series", islist=TRUE),
        spsspkg.Template("DATASET", subc="GET", ktype="literal", var="dataset")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "dogetr")
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

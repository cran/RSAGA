
rsaga.env = function( workspace=".", 
    cmd = ifelse(.Platform$OS.type=="windows", "saga_cmd.exe", "saga_cmd"), 
    path, modules,
    check.libpath = TRUE, 
    check.SAGA = TRUE, 
    check.PATH = .Platform$OS.type == "windows",
    check.os.default = TRUE,
    os.default.path = ifelse(.Platform$OS.type=="windows", "C:/Progra~1/SAGA-GIS", "/usr/local/bin") )
{
    rsaga.get.possible.SAGA.paths = function( check.libpath, check.SAGA,
            check.PATH, check.os.default, os.default.path )
    {
        path = c()
        if (check.libpath) {
            path.pckg = file.path( .libPaths(), "RSAGA" )
            try( path.pckg <- .path.package("RSAGA"), silent = TRUE )
            path = c( path, file.path( path.pckg, "saga_vc" ) )
        }
        if (check.SAGA)
            path = c( path, Sys.getenv("SAGA"), getwd() )

        if (check.PATH)
        {
            os.path = Sys.getenv("PATH")
    
            # This won't work:
            #os.paths = strsplit(os.path,";")[[1]]
            # on Windows, filenames may contain a ";"!
    
            # Split the PATH environment variable at ";",
            # except when the ";" occurs inside a path in quotation marks:
            i = 1
            while (i <= nchar(os.path)) {
                next.path = ""
                while ((i <= nchar(os.path)) & (substr(os.path,i,i) != ";")) {
                    # repeat until end of PATH or ";":
                    if (substr(os.path,i,i) == '"') { ## potential problems under unix??
                        # if a quote begins, ignore any ";"s:
                        i = i + 1 # skip the quotation mark
                        while ((i <= nchar(os.path)) & (substr(os.path,i,i) != '"')) {
                            # inside the quotation, pass any normal characters
                            # and possible ";'s to next.path:
                            next.path = paste(next.path, substr(os.path,i,i), sep="")
                            i = i + 1
                        } # until end of PATH or end of quotation
                        if (substr(os.path,i,i) != '"') { ## potential problems under unix??
                            # unlikely case that quotation ends at the end of PATH,
                            # with the closing quotation mark missing:
                            next.path = paste(next.path, substr(os.path,i,i), sep="")
                        }
                        # note that the '"' is not passed to next.path
                    } else {
                        # pass any 'normal' character from PATH to next.path:
                        next.path = paste(next.path, substr(os.path,i,i), sep="")
                    }
                    i = i + 1
                } # until end of PATH or next ";"
                path = c(path, next.path)
                i = i + 1
            } # until end of PATH
        }
        
        if (check.os.default) {
            if (!is.null(os.default.path))
                path = c(path, os.default.path)
        }

        return(path)
    }


    if (workspace == "") workspace = "."

    if (missing(path)) {
        path = NULL
    } else {
        if (!file.exists(file.path(path,cmd))) {
            warning("SAGA command line program ", cmd, " not found in the specified path ", 
                path, ".", "\nTrying to find it somewhere else.")
            path = NULL
        }
    }
    if (is.null(path)) {
        path = rsaga.get.possible.SAGA.paths( check.libpath, check.SAGA,
                                    check.PATH, check.os.default,
                                    os.default.path = os.default.path )
        if (length(path)==0) {
            warning("don't know where to look for SAGA command line program, no paths specified",
                "\nTrying to find it in the current working directory...")
            path = getwd()
        }
        the.path = NULL
        for (pa in path) {
            if (file.exists(file.path(pa,cmd))) {
                the.path = pa
                break
            }
        }
        if (is.null(the.path)) {
            warning("SAGA command line program '", cmd, 
                    "' not found in any of the paths\n", paste(path,collapse="\n"))
            return(NULL)
        }
        path = the.path
    }    

    if (missing(modules)) {
        modules = file.path(path,"modules")
        if ( .Platform$OS.type == "unix") 
            modules = "/usr/local/lib/saga"
    } else {
        if (modules == "") modules = getwd()
    }
    if (!file.exists(workspace))
        warning("Invalid workspace path ", workspace)
    if (!file.exists(path))
        warning("Invalid SAGA path ", path)
    #if (!file.exists(file.path(path,cmd)))
    #    warning("SAGA command line program ", file.path(path,cmd), " not found.")
    if (!file.exists(modules))
        warning("Invalid SAGA modules path ", modules)
    return( list(
        workspace = workspace,
        cmd = cmd,
        path = path,
        modules = modules ))
}


rsaga.get.libraries = function(path=rsaga.env()$modules, dll=.Platform$dynlib.ext)
{
    dllnames = dir(path,paste("^.*\\",dll,"$",sep=""))
    if (.Platform$OS.type == "unix")
        dllnames = substr(dllnames, 4, nchar(dllnames)) # remove the "lib"
    return( gsub(dll,"",dllnames,fixed=TRUE ) )
}


rsaga.get.lib.modules = function(lib, env=rsaga.env(), interactive=FALSE)
{
    res = NULL

    rawres = rsaga.geoprocessor(lib, module=NULL,
        silent=FALSE, env=env,
        intern=TRUE, show.output.on.console=FALSE, invisible=TRUE,
        reduce.intern=FALSE)

    wh = which( gsub(" ","",tolower(rawres)) == "availablemodules:" )

    if (length(wh) > 0) {
        rawres = rawres[ (wh[length(wh)]+1) : length(rawres) ]
        rawres = rawres[ rawres != "" ]
        rawres = rawres[ rawres != "type -h or --help for further information" ]
    }
    if (length(wh) > 0) {
        rawres = strsplit(rawres,"\t- ")
        mcodes = c()
        mnames = c()
        minteracs = c()
        for (descr in rawres) {
            mygrep = c( grep("[",descr[1],fixed=TRUE), grep("]",descr[1],fixed=TRUE),
                grep("[interactive]",descr[2],fixed=TRUE) )
            minterac = (length(mygrep) > 0)
            # skip interactive modules if only interactive ones are allowed:
            if (!minterac | interactive) {
                mcode = gsub("[","",gsub("]","",gsub(" ","",descr[1]),fixed=TRUE),fixed=TRUE)
                mname = gsub("[interactive] ","",descr[2],fixed=TRUE)
                mcodes = c(mcodes, as.numeric(mcode))
                mnames = c(mnames, mname)
                minteracs = c(minteracs, minterac)
            }
        }
        #if (length(mcodes) > 0)
        res = data.frame(code=mcodes, name=mnames, interactive=minteracs)
    }
    return(res)
}


rsaga.get.modules = function(libs, env=rsaga.env(),...) {
    if (missing(libs)) libs = rsaga.get.libraries(env$modules)
    res = as.list(rep(NA,length(libs)))
    names(res) = libs
    for (i in 1:length(libs)) {
        modules = rsaga.get.lib.modules(libs[i], env=env,...)
        if (!is.null(modules))
            res[[i]] = modules
    }
    return(res)
}


rsaga.get.usage = function(lib, module, env=rsaga.env(), show=TRUE)
{
    if (is.function(lib))
        lib = deparse(substitute(lib))
    
    if (substr(lib,1,6)=="rsaga.") {
        if (lib=="rsaga.fill.sinks") {
            warning("'rsaga.fill.sinks' uses three modules from the 'ta_preprocessor' library:\n",
                "   for 'method=\"planchon.darboux.2001\"': module 2\n",
                "   for 'method=\"wang.liu.2006\"': module 3\n",
                "   for 'method=\"xxl.wang.liu.2006\"': module 4\n",
                "using 'module=2'\n")
        }
        lib = switch(lib,
                rsaga.close.gaps          = list(lib="grid_tools", module=7),
                rsaga.esri.to.sgrd        = list(lib="io_grid", module=1),
                rsaga.sgrd.to.esri        = list(lib="io_grid", module=0),
                rsaga.parallel.processing = list(lib="ta_hydrology", module=0),
                rsaga.local.morphometry   = list(lib="ta_morphometry", module=0),
                rsaga.slope               = list(lib="ta_morphometry", module=0),
                rsaga.aspect              = list(lib="ta_morphometry", module=0),
                rsaga.curvature           = list(lib="ta_morphometry", module=0),
                rsaga.plan.curvature      = list(lib="ta_morphometry", module=0),
                rsaga.profile.curvature   = list(lib="ta_morphometry", module=0),
                rsaga.sink.route          = list(lib="ta_preprocessor", module=0),
                rsaga.sink.removal        = list(lib="ta_preprocessor", module=1),
                rsaga.fill.sinks          = list(lib="ta_preprocessor", module=2),
                rsaga.contour             = list(lib="shapes_grid", module=5),
                rsaga.hillshade           = list(lib="ta_lighting", module=0),
                rsaga.solar.radiation     = list(lib="ta_lighting", module=2),
                rsaga.insolation          = list(lib="ta_lighting", module=3),
                rsaga.filter.simple       = list(lib="grid_filter", module=0),
                rsaga.filter.gauss        = list(lib="grid_filter", module=1) )
        module = lib$module
        lib = lib$lib
    }

    res = NULL
    usage = rsaga.geoprocessor(lib, module, param = list(h=""), env = env,
        intern = TRUE, show.output.on.console = FALSE, silent = FALSE)
    skip = 0
    while ((length(usage)>(1+skip)) & (substr(usage[1+skip],1,6)!="Usage:")) {
        if (substr(usage[1+skip],1,8) %in% 
                c("SAGA CMD","Copyrigh","library ","module n","________")) {
            skip = skip + 1
        } else {
            if (skip == 0) {
                usage = usage[ 2 : length(usage) ]
            } else {
                usage = usage[ c(1:skip, (skip+2):length(usage)) ]
            }
        }
    }
    if (length(usage) > 1) {
        res = usage[ 1 : (length(usage)-1) ]
        if (substr(res[length(res)],1,6)=="______") {
            res = c(res, "Usage description not available (interactive module?)")
            warning("usage description not available for module ",
                module, " in library ", lib, " (interactive module?)")
        }
    } else
        warning("usage description not available for module ",
            module, "\nin library ", lib, " (interactive module?)")
    if (show) {
        if (!is.null(res))
            cat(paste(res,collapse="\n"),"\n\n")
    }
    invisible(res)
}


rsaga.html.help = function(lib, module, env=rsaga.env(), ...)
{
    if (missing(module)) module = NA

    if (is.function(lib))
        lib = deparse(substitute(lib))
    
    if (substr(lib,1,6)=="rsaga.") {
        if (lib=="rsaga.fill.sinks") {
            warning("'rsaga.fill.sinks' uses three modules from the 'ta_preprocessor' library:\n",
                "   for 'method=\"planchon.darboux.2001\"': module 2\n",
                "   for 'method=\"wang.liu.2006\"': module 3\n",
                "   for 'method=\"xxl.wang.liu.2006\"': module 4\n",
                "using 'module=NULL'\n")
        }
        lib = switch(lib,
                rsaga.close.gaps          = list(lib="grid_tools", module=7),
                rsaga.esri.to.sgrd        = list(lib="io_grid", module=1),
                rsaga.sgrd.to.esri        = list(lib="io_grid", module=0),
                rsaga.parallel.processing = list(lib="ta_hydrology", module=0),
                rsaga.local.morphometry   = list(lib="ta_morphometry", module=0),
                rsaga.slope               = list(lib="ta_morphometry", module=0),
                rsaga.aspect              = list(lib="ta_morphometry", module=0),
                rsaga.curvature           = list(lib="ta_morphometry", module=0),
                rsaga.plan.curvature      = list(lib="ta_morphometry", module=0),
                rsaga.profile.curvature   = list(lib="ta_morphometry", module=0),
                rsaga.sink.route          = list(lib="ta_preprocessor", module=0),
                rsaga.sink.removal        = list(lib="ta_preprocessor", module=1),
                rsaga.fill.sinks          = list(lib="ta_preprocessor", module=NULL),
                rsaga.contour             = list(lib="shapes_grid", module=5),
                rsaga.hillshade           = list(lib="ta_lighting", module=0),
                rsaga.solar.radiation     = list(lib="ta_lighting", module=2),
                rsaga.insolation          = list(lib="ta_lighting", module=3),
                rsaga.filter.simple       = list(lib="grid_filter", module=0),
                rsaga.filter.gauss        = list(lib="grid_filter", module=1) )
        if (!is.null(module)) module = lib$module
        lib = lib$lib
    }
    
    if (!is.null(module)) if (is.na(module)) module = NULL

    libnames = c( "geostatistics_points",
        "io_esri_e00", "io_grid", "io_grid_gdal", "io_grid_image", "io_shapes",
        "pj_georeference", "pj_geotrans", "pj_proj4",
        "shapes_grid", "shapes_tools", "sim_cellular_automata", 
        "ta_channels", "ta_hydrology", "ta_lighting", 
        "ta_morphometry", "ta_preprocessor" )
    foldernames = c( "Geostatistics",
        "ESRI_E00_IO", "Grid_IO", "Grid_IO_GDAL", "Grid_IO_Image", "Shapes_IO", 
        "Georeference", "Projection_GeoTRANS", "Projection_Proj4",
        "Grid_Shapes", "Shapes_Tools", "Cellular_Automata",
        "Terrain_Analysis_Channels", "Terrain_Analysis_Flow", "Terrain_Analysis_Lighting",
        "Terrain_Analysis_Morphometry", "Terrain_Analysis_Preprocessing" )

    if (any(libnames==lib))  lib = foldernames[libnames==lib]
    # ...otherwise hope that the foldername is equal to the library name...
    url = paste("file:",.Platform$file.sep,.Platform$file.sep,sep="")
    url = paste(url,env$path,"doc",lib,sep=.Platform$file.sep)
    
    if (is.null(module)) {
        url = paste(url,"index.html",sep=.Platform$file.sep)
    } else {
        stopifnot(is.numeric(module))
        warning("Some modules are currently not correctly linked to their help\n",
            "files; if you get the wrong help file, use 'module=NULL' and browse\n",
            "from the SAGA library's main help page to the module's help page.\n")
        url = paste(url,.Platform$file.sep,"Module_",module,".html",sep="")
    }
    cat("Open",url,"\n")
    browseURL(url,...)
}


rsaga.geoprocessor = function(
    lib, module = NULL, param = list(),
    silent = TRUE, beep.off,
    show.output.on.console = TRUE, invisible = TRUE, intern = TRUE,
    env = rsaga.env(), display.command = FALSE, reduce.intern = TRUE, ... )
{
    old.wd = getwd()
    old.saga = Sys.getenv("SAGA")
    old.saga.mlb = Sys.getenv("SAGA_MLB")
    on.exit(setwd(old.wd))
    on.exit(Sys.setenv(SAGA=old.saga,SAGA_MLB=old.saga.mlb), add=TRUE)
    
    setwd(env$workspace)
    Sys.setenv(SAGA=env$path, SAGA_MLB=env$modules)
    
    if (.Platform$OS.type == "windows") {
        # It is safer to use quotes under Windows:
        command = paste( shQuote( paste( env$path, .Platform$file.sep, env$cmd, sep="" ) ),
                        " ", lib, sep="")
    } else {
        # Trying to use quotes under Unix too:
        command = paste( shQuote( paste( env$path, .Platform$file.sep, env$cmd, sep="" ) ),
                        " ", "lib", lib, sep="")
    }
    
    if (!is.null(module)) {
        if (is.character(module)) module = shQuote(module)
        command = paste(command, module)
        if (silent)
            command = paste(command, "-silent")
        if (length(param)>0) {
            i = 1
            while (i<=length(param)) {
                if (is.logical(param[[i]])) {
                    if (!param[[i]]) {
                        param[[i]] = NULL
                        i = i - 1
                    } else param[[i]] = ""
                }
                i = i + 1
            }
            nm = names(param)
            val = as.character(unlist(param))
            param = paste("-",nm," ",val,sep="",collapse=" ")
            command = paste(command, param)
        }
    }
    if (display.command) cat(command,"\n")
    if (!missing(beep.off))
        warning("rsaga.geoprocessor currently ignores 'beep.off'")
#    if (beep.off & .Platform$OS.type=="windows") {
#        command = gsub("/","\\",command,fixed=TRUE)
#        batch = c("net stop beep",command)
#        batchfilename = paste(tempfile(),".bat",sep="")
#        batchfile = file(batchfilename,"wt")
#        writeLines(batch,con=batchfile)
#        close(batchfile)
#        command = batchfilename
#    }
    if (.Platform$OS.type == "windows") {
        res = system( command, intern=intern,
            show.output.on.console=show.output.on.console, 
            invisible=invisible, ...)
    } else {
        res = system( command, intern=intern, ...)
        # 'show.output.on.console' and 'invisible' only work under Windows
    }
#    if (beep.off & .Platform$OS.type=="windows") unlink(batchfilename)
    if (intern) {
        if (reduce.intern) {
#            if (beep.off & .Platform$OS.type=="windows") {
#                removeline = function(x) {
#                    if (x=="") return(TRUE)
#                    rmv = c(tolower(batch),"the beep service","more help is available")
#                    for (txt in rmv)
#                        if (length(grep(txt,tolower(x),fixed=TRUE)) > 0)
#                            return(TRUE)
#                    return(FALSE)
#                }
#                while ( removeline(res[1]) )
#                    res = res[-1]
#            }
            remove = grep("\r",res,fixed=TRUE)
            if (length(remove) > 0)
                res = res[ -remove ]
            remove = grep("^.*##.*##",res)
            if (length(remove) > 0)
                res = res[ -remove ]
            if (any(remove <- res=="go...")) res = res[!remove]
            if (any(remove <- res=="okay"))  res = res[!remove]
            if (any(remove <- substr(res,1,7)=="type -h")) res = res[!remove]
            if (any(remove <- substr(res,1,7)=="_______")) res = res[!remove]
        }
        if (show.output.on.console)
            cat(res,sep="\n")
    }
    if (intern) {
        invisible(res)
    } else   return(res)
}



rsaga.esri.wrapper = function(fun, in.esri=TRUE, out.esri=TRUE, 
    env=rsaga.env(), esri.workspace=env$workspace,
    format="ascii", georef="corner", prec=5, esri.extension,
    condensed.res=TRUE, clean.up=TRUE, intern=TRUE, ...)
{
    in.res = NULL
    geoproc.res = NULL
    out.res = NULL
    format = match.arg.ext(format,choices=c("binary","ascii"),base=0,ignore.case=TRUE,numeric=TRUE)
    if (missing(esri.extension))
        esri.extension = c(".flt",".asc")[format+1]
    args = list(...)
    argnms = names(args)
    
    in.ok = TRUE
    if (in.esri) {
        wh = grep("^in\\.",names(args))
        if (length(wh)==0) {
            warning("'in.esri' is TRUE, but the geoprocessing function does not have an 'in.*' grid argument")
        } else {
            in.args = args[wh]
            in.res = rsaga.esri.to.sgrd(in.grids=set.file.extension(in.args,esri.extension),
                intern=intern, show.output.on.console=FALSE,
                out.sgrds=in.args, in.path=esri.workspace, env=env) # more args to geoproc
            if (!intern) in.ok = all(in.res==0)
        }
    }
    
    geoproc.ok = TRUE
    if (in.ok) {
        geoproc.res = fun(env=env,intern=intern,...)
        if (!intern) geoproc.ok = all(geoproc.res==0)
    }
    if (clean.up) {
        del.files = set.file.extension(in.args,"")
        del.files = unlist(lapply(as.list(del.files), function(x) paste(x,c("sgrd","hgrd","sdat"),sep="")))
        unlink(del.files)
    }
    
    out.ok = TRUE
    if (out.esri & in.ok & geoproc.ok) {
        wh = grep("^out\\.",names(args))
        if (length(wh)==0) {
            warning("'out.esri' is TRUE, but the geoprocessing function does not have an 'out.*' grid argument")
        } else {
            out.args = args[wh]
            out.res = rsaga.sgrd.to.esri(in.sgrds=out.args,
                out.grids=set.file.extension(out.args,esri.extension),
                out.path=esri.workspace, env=env, intern=intern, show.output.on.console=FALSE,
                format=format, georef=georef, prec=prec) # more args to geoproc
            if (!intern) out.ok = all(out.res==0)
            if (clean.up) {
                del.files = set.file.extension(out.args,"")
                del.files = unlist(lapply(as.list(del.files), function(x) paste(x,c("sgrd","hgrd","sdat"),sep="")))
                unlink(del.files)
            }
        }
    }

    res = list( in.res=in.res, geoproc.res=geoproc.res, out.res=out.res )
    if (condensed.res) {
        if (intern) {
            res = geoproc.res
        } else   res = max(abs(unlist(res)))
    }
    if (intern) {
        invisible(res)
    } else  return( res )
}

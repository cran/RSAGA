
rsaga.target = function(
    target = c("user.defined", "grid.system", "target.grid", "header"),
    user.cellsize = 100, user.fit.extent = TRUE,
    user.x.extent, user.y.extent, user.bbox,
    system.nx, system.ny, system.xy, system.d,
    target.grid, header)
{
    target = match.arg.ext(target, base = 0, numeric = TRUE)
    
    if (target == 3) {
        stopifnot(missing(user.bbox))
        target = 1
        system.nx = header$ncols
        system.ny = header$nrows
        if (!any(names(header) == "xllcenter"))
            header$xllcenter = header$xllcorner + header$cellsize / 2
        if (!any(names(header) == "yllcenter"))
            header$yllcenter = header$yllcorner + header$cellsize / 2
        system.xy = c(header$xllcenter, header$yllcenter)
        system.d = header$cellsize
    }

    param = list(TARGET = target)
    
    if (target == 0) {
        stopifnot(missing(system.nx) & missing(system.ny) & missing(system.xy) &
            missing(system.d) & missing(target.grid))
        stopifnot(is.logical(user.fit.extent))
        param = c(param,
            USER_CELL_SIZE = user.cellsize,
            USER_FIT_EXTENT = user.fit.extent)
        if (user.fit.extent) {
            if (!missing(user.x.extent) | !missing(user.y.extent) | !missing(user.bbox))
                warning("'user.x.extent', 'user.y.extent' and 'user.bbox' will be ignored because 'user.fit.extent=TRUE'")
        } else {
            if (missing(user.bbox)) 
                user.bbox = rbind(user.x.extent, user.y.extent, deparse.level = 0)
            param = c(param,
                USER_X_EXTENT_MIN = user.bbox[1,1],
                USER_X_EXTENT_MAX = user.bbox[1,2],
                USER_Y_EXTENT_MIN = user.bbox[2,1],
                USER_Y_EXTENT_MAX = user.bbox[2,2])
        }
    } else if (target == 1) {
        stopifnot(missing(user.x.extent) & missing(user.y.extent) & 
            missing(user.bbox) & missing(target.grid))
        stopifnot(length(system.xy) == 2)
        param = c(param,
            SYSTEM_SYSTEM_NX = system.nx,
            SYSTEM_SYSTEM_NY = system.ny,
            SYSTEM_SYSTEM_X  = system.xy[1],
            SYSTEM_SYSTEM_Y  = system.xy[2],
            SYSTEM_SYSTEM_D  = system.d)
    } else if (target == 2) {
        stopifnot(missing(system.nx) & missing(system.ny) & missing(system.xy) & missing(system.d) &
            missing(user.x.extent) & missing(user.y.extent) & missing(user.bbox))
        target.grid = default.file.extension(target.grid, ".sgrd")
        param = c(param,
            GRID_GRID = target.grid)
    }
    return(param)
}


############################################
########     Module io_grid_gdal    ########
############################################


rsaga.import.gdal = function( in.grid, out.grid, ... ) {
    if (missing(out.grid)) {
        out.grid = set.file.extension(in.grid, "")
        out.grid = substr(out.grid, 1, nchar(out.grid) - 1)
    }
    param = list( GRIDS = out.grid, FILE = in.grid )
    rsaga.geoprocessor("io_grid_gdal", 0, param=param)
}



############################################
########       Module io_grid       ########
############################################


rsaga.esri.to.sgrd = function( in.grids, 
    out.sgrds=set.file.extension(in.grids,".sgrd"), in.path, ... )
{
    in.grids = default.file.extension(in.grids,".asc")
    out.sgrds = default.file.extension(out.sgrds,".sgrd")
    if (!missing(in.path))
        in.grids = file.path(in.path,in.grids)
    if (length(in.grids) != length(out.sgrds))
        stop("must have the same number of input and outpute grids")
    res = c()
    for (i in 1:length(in.grids))
        res = c(res, rsaga.geoprocessor("io_grid", 1,
            list(FILE=in.grids[i],GRID=out.sgrds[i]),...) )
    invisible(res)
}


rsaga.sgrd.to.esri = function( in.sgrds, out.grids, out.path,
    format="ascii", georef="corner", prec=5, ... )
{
    in.sgrds = default.file.extension(in.sgrds,".sgrd")
    format = match.arg.ext(format,choices=c("binary","ascii"),base=0,ignore.case=TRUE,numeric=TRUE)
    georef = match.arg.ext(georef,choices=c("corner","center"),base=0,ignore.case=TRUE,numeric=TRUE)
    if (missing(out.grids))
        out.grids = set.file.extension(in.sgrds, c(".flt",".asc")[format+1])
    out.grids = default.file.extension(out.grids, c(".flt",".asc")[format+1])
    if (!missing(out.path))
        out.grids = file.path(out.path,out.grids)
    if (length(out.grids) != length(in.sgrds))
        stop("must have the same number of input and outpute grids")
    if ((length(prec)==1) & (length(in.sgrds)>1))
        prec = rep(prec,length(in.sgrds))
    if (length(prec) != length(in.sgrds))
        stop("must have same number of in-/output grids and 'prec' parameters (or length(prec)==1)")
    res = c()
    for (i in 1:length(in.sgrds))
        res = c(res, rsaga.geoprocessor("io_grid", 0,
            list( GRID=in.sgrds[i], FILE=out.grids[i], FORMAT=format, GEOREF=georef, PREC=prec[i]),
            ...))
    invisible(res)
}




############################################
########    Module ta_morphometry   ########
############################################


rsaga.local.morphometry = function( in.dem, 
    out.slope, out.aspect, out.curv, out.hcurv, out.vcurv,
    method = "poly2zevenbergen", ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    choices = c("maxslope","maxtriangleslope","lsqfitplane",
        "poly2bauer","poly2heerdegen","poly2zevenbergen","poly3haralick")
    method = match.arg.ext(method,choices,numeric=TRUE,base=0)
    if (missing(out.aspect)) {
        out.aspect = tempfile()
        on.exit(unlink(paste(out.aspect,".*",sep="")), add = TRUE)
    }
    if (missing(out.slope)) {
        out.slope = tempfile()
        on.exit(unlink(paste(out.slope,".*",sep="")), add = TRUE)
    }
    param = list(ELEVATION=in.dem, SLOPE=out.slope, ASPECT=out.aspect)
    if (!missing(out.curv))
        param = c(param, CURV=out.curv)
    if (!missing(out.hcurv))
        param = c(param, HCURV=out.hcurv)
    if (!missing(out.vcurv))
        param = c(param, VCURV=out.vcurv)
    param = c(param, METHOD=method)
    rsaga.geoprocessor("ta_morphometry",0,param,...)
}

rsaga.slope = function( in.dem, out.slope, method = "poly2zevenbergen", ... ) {
    stopifnot(!missing(out.slope))
    rsaga.local.morphometry( in.dem=in.dem, out.slope=out.slope, method=method, ... )
}

rsaga.aspect = function( in.dem, out.aspect, method = "poly2zevenbergen", ... ) {
    stopifnot(!missing(out.aspect))
    rsaga.local.morphometry( in.dem=in.dem, out.aspect=out.aspect, method=method, ... )
}
rsaga.curvature = function( in.dem, out.curv, method = "poly2zevenbergen", ... ) {
    stopifnot(!missing(out.curv))
    rsaga.local.morphometry( in.dem=in.dem, out.curv=out.curv, method=method, ... )
}

rsaga.plan.curvature = function( in.dem, out.hcurv, method = "poly2zevenbergen", ... ) {
    stopifnot(!missing(out.hcurv))
    rsaga.local.morphometry( in.dem=in.dem, out.hcurv=out.hcurv, method=method, ... )
}

rsaga.profile.curvature = function( in.dem, out.vcurv, method = "poly2zevenbergen", ... ) {
    stopifnot(!missing(out.vcurv))
    rsaga.local.morphometry( in.dem=in.dem, out.vcurv=out.vcurv, method=method, ... )
}




############################################
########   Module ta_preprocessor   ########
############################################


rsaga.fill.sinks = function(in.dem,out.dem,
    method="planchon.darboux.2001", out.flowdir, out.wshed, minslope, ...)
{
    stopifnot(is.character(method))
    method = match.arg.ext(method, ignore.case=TRUE, numeric=TRUE, base=2,
        choices=c("planchon.darboux.2001","wang.liu.2006","xxl.wang.liu.2006"))
    in.dem = default.file.extension(in.dem,".sgrd")
    stopifnot(!missing(out.dem))
    if (missing(minslope)) minslope = NULL
    if (method==2) {
        param = list( DEM=in.dem, RESULT=out.dem )
        if (missing(minslope)) minslope = 0.01
        minslope = as.numeric(minslope)
    } else if (method==3) {
        if (missing(out.flowdir)) {
            out.flowdir = tempfile()
            on.exit(unlink(paste(out.flowdir,".*",sep="")), add = TRUE)
        }
        if (missing(out.wshed)) {
            out.wshed = tempfile()
            on.exit(unlink(paste(out.wshed,".*",sep="")), add = TRUE)
        }
        param = list(ELEV=in.dem, FILLED=out.dem, FDIR=out.flowdir, WSHED=out.wshed)
    } else if (method==4) {
        param = list(ELEV=in.dem, FILLED=out.dem)
    }
    if (!is.null(minslope)) param = c( param, MINSLOPE=minslope )
    rsaga.geoprocessor("ta_preprocessor",method,param,...)
}



rsaga.sink.route = function(in.dem,out.sinkroute,threshold,thrsheight=100,...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    param = list( ELEVATION=in.dem, SINKROUTE=out.sinkroute )
    if (!missing(threshold)) {
        if (threshold)   param = c( param, THRESHOLD="" )
    }
    # I guess thrsheight is redundant if threshold is missing/false:
    param = c( param, THRSHEIGHT=as.numeric(thrsheight) )
    rsaga.geoprocessor("ta_preprocessor", 0, param, ...)
}


rsaga.sink.removal = function(in.dem,in.sinkroute,out.dem,method="fill",...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    method = match.arg.ext(method,c("deepen drainage routes","fill sinks"),ignore.case=TRUE,numeric=TRUE)
    param = list( DEM=in.dem )
    if (!missing(in.sinkroute)) {
        in.sinkroute = default.file.extension(in.sinkroute,".sgrd")
        param = c(param, SINKROUTE=in.sinkroute)
    }
    param = c( param, DEM_PREPROC=out.dem, METHOD=method )
    rsaga.geoprocessor("ta_preprocessor", 1, param, ...)
}




############################################
########     Module grid_tools      ########
############################################



rsaga.close.gaps = function(in.dem,out.dem,threshold=0.1,...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    param = list( INPUT=in.dem, RESULT=out.dem, THRESHOLD=as.numeric(threshold) )
    rsaga.geoprocessor("grid_tools",7,param,...)
}

rsaga.close.one.cell.gaps = function(in.dem,out.dem,...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    param = list( INPUT = in.dem, RESULT = out.dem )
    rsaga.geoprocessor("grid_tools", 6, param, ...)
}

rsaga.search.modules = function(text, modules, search.libs=TRUE, search.modules=TRUE,
    env=rsaga.env(), ignore.case=TRUE, ...)
{
    pattern = paste("^.*",text,sep="")
    lib = NULL
    mod = NULL
    if (search.libs) {
        lib.nm = rsaga.get.libraries(path=env$modules)
        wh.lib = grep(pattern,lib.nm,ignore.case=ignore.case)
        lib = lib.nm[wh.lib]
    }
    if (search.modules) {
        if (missing(modules))  modules = rsaga.get.modules(env=rsaga.env(),...)
        mod.nm = unlist(sapply(modules,function(x) as.character(x$name)),use.names=FALSE)
        mod.libs = sapply(modules,function(x) nrow(x))
        mod.libs = rep(names(mod.libs),mod.libs)
        wh.mod = grep(pattern,mod.nm,ignore.case=ignore.case)
        mod = data.frame( lib=mod.libs[wh.mod], module=mod.nm[wh.mod] )
    }
    return( list( lib = lib, modules = mod ) )
}



############################################
########     Module ta_lighting     ########
############################################


rsaga.hillshade = function(in.dem, out.grid,
    method="standard", azimuth=315, declination=45, exaggeration=4, ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    out.grid = default.file.extension(out.grid,".sgrd")
    method = match.arg.ext(method, numeric=TRUE, ignore.case=TRUE, base=0,
        choices=c("standard","max90deg.standard","combined.shading","ray.tracing"))
    param = list(ELEVATION=in.dem, SHADE=out.grid, METHOD=method,
        AZIMUTH=azimuth, DECLINATION=declination, EXAGGERATION=exaggeration)
    rsaga.geoprocessor("ta_lighting", 0, param, ...)
}


rsaga.solar.radiation = function(in.dem, out.grid, out.duration, latitude, 
    unit=c("kWh/m2","J/m2"), solconst=1367.0, method=c("lumped","components"),
    transmittance=70, pressure=1013, water.content=1.68, dust=100,
    time.range=c(0,24), time.step=1,
    days=list(day=21,month=3), day.step=5, ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    if (missing(out.duration)) {
        out.duration = tempfile()
        on.exit(unlink(paste(out.duration,".*",sep="")), add = TRUE)
    }
    unit = match.arg.ext(unit,numeric=TRUE,ignore.case=TRUE,base=0)
    method = match.arg.ext(method,numeric=TRUE,ignore.case=TRUE,base=0)
    stopifnot( (transmittance>=0) & (transmittance<=100) )
    stopifnot( (latitude>=-90) & (latitude<=90) )
    stopifnot( length(time.range)==2 )
    stopifnot( all(time.range>=0) & all(time.range<=24) & (time.range[1]<time.range[2]) )
    stopifnot( (time.step>0) & (time.step<=12) )
    stopifnot( (day.step>0) & (day.step<=100) )

    param = list( ELEVATION=in.dem, INSOLAT=out.grid, DURATION=out.duration,
        UNIT=unit, SOLCONST=as.numeric(solconst), METHOD=method,
        TRANSMITT=as.numeric(transmittance), PRESSURE=as.numeric(pressure), 
        WATER=as.numeric(water.content), DUST=as.numeric(dust),
        LATITUDE=as.numeric(latitude), 
        HOUR_RANGE_MIN=time.range[1], HOUR_RANGE_MAX=time.range[2], 
        HOUR_STEP=time.step )
        
    if (is.null(days)) { # one year
        param = c( param, TIMESPAN=2 )
    } else if (is.list(days)) { # single day
        stopifnot(length(days)==2)
        stopifnot( (days$day>=1) & (days$day<=31) )
        stopifnot( (days$month>=1) & (days$month<=12) )
        param = c( param, TIMESPAN=0,
            SINGLE_DAY_DAY=days$day-1, SINGLE_DAY_MONTH=days$month-1 )
    } else if (is.numeric(days)) { # range of days
        stopifnot(length(days)==2)
        stopifnot( days[1] <= days[2] )
        stopifnot( (days[1]>=1) & (days[2]<=366) )
        param = c( param, TIMESPAN=1, 
            DAY_RANGE_MIN=days[1], DAY_RANGE_MAX=days[2], 
            DAY_STEP=day.step )
    }
    rsaga.geoprocessor("ta_lighting", 2, param, ...)
}



rsaga.insolation = function(in.dem, in.vapour, in.latitude, in.longitude,
    out.direct, out.diffuse, out.total,
    horizontal=FALSE, solconst=8.1640, atmosphere=12000, water.vapour.pressure=10.0,
    type=c("moment","day","range.of.days","same.moment.range.of.days"),
    time.step=1, day.step=5, days, moment, latitude, bending=FALSE,
    radius=6366737.96,
    lat.offset="user", lat.ref.user=0,
    lon.offset="center", lon.ref.user=0,
     ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    param = list( GRD_DEM=in.dem )
    type = match.arg.ext(type,numeric=TRUE,ignore.case=TRUE,base=0)
    stopifnot( (!missing(out.direct)) | (!missing(out.diffuse)) | (!missing(out.total)) )
    stopifnot( !missing(latitude) )
    if (!missing(moment)) {
        if (!(type==0 | type==3)) {
            warning("'moment' argument only relevant for 'type=\"moment\"'\n",
                    "or 'type=\"same.moment.range.of.days\"' -\n",
                    "ignoring the 'moment' argument")
        }
    }
    if (!missing(in.vapour)) {
        in.vapour = default.file.extension(in.vapour,".sgrd")
        param = c(param, GRD_VAPOUR=in.vapour)
    }
    if (!missing(in.latitude)) {
        in.latitude = default.file.extension(in.latitude,".sgrd")
        param = c(param, GRD_LAT=in.latitude)
    }
    if (!missing(in.longitude)) {
        in.longitude = default.file.extension(in.longitude,".sgrd")
        param = c(param, GRD_LON=in.longitude)
    }
    if (!missing(out.direct)) param = c(param, GRD_DIRECT=out.direct)
    if (!missing(out.diffuse)) param = c(param, GRD_DIFFUS=out.diffuse)
    if (!missing(out.total)) param = c(param, GRD_TOTAL=out.total)
    stopifnot( (days[1]>=0) & (days[1]<=366) )
    param = c(param, BHORIZON=horizontal, SOLARCONST=solconst,
        ATMOSPHERE=atmosphere, VAPOUR=water.vapour.pressure,
        PERIOD=type, DHOUR=time.step, DDAYS=day.step,
        DAY_A=days[1])
    if (type>=2) { # range of days / same moment in a range of days
        stopifnot( (days[2]>=days[1]) & (days[2]<=366) )
        param = c(param, DAY_B=days[2])
    }
    if ((type==0) | (type==3)) {
        stopifnot( (moment>=0) & (moment<=24) )
        param = c(param, MOMENT=moment)
    }
    param = c(param, LATITUDE=latitude, BENDING=bending, RADIUS=radius)
    lat.offset = match.arg.ext(lat.offset, c("bottom","center","top","user"),
        numeric=TRUE, ignore.case=TRUE, base=0)
    lon.offset = match.arg.ext(lon.offset, c("left","center","right","user"),
        numeric=TRUE, ignore.case=TRUE, base=0)
    param = c(param, LAT_OFFSET=lat.offset)
    if (lat.offset==3) { # user-defined
        #stopifnot(!missing(lat.ref.user))
        param = c(param, LAT_REF_USER=as.numeric(lat.ref.user))
    }
    param = c(param, LON_OFFSET=lon.offset)
    if (lon.offset==3) { # user-defined
        #stopifnot(!missing(lon.ref.user))
        param = c(param, LON_REF_USER=as.numeric(lon.ref.user))
    }
    rsaga.geoprocessor("ta_lighting", 3, param, ...)
}




############################################
########     Module grid_filter     ########
############################################


rsaga.filter.simple = function(in.grid, out.grid, mode="circle",
    method=c("smooth","sharpen","edge"), radius,...)
{
    in.grid = default.file.extension(in.grid,".sgrd")
    mode = match.arg.ext(mode,choices=c("square","circle"),
        numeric=TRUE,base=0,ignore.case=TRUE)
    method = match.arg.ext(method,numeric=TRUE,base=0,ignore.case=TRUE)
    if (missing(radius)) stop("the search 'radius' argument (in # pixels) must be specified")
    if (round(radius) != radius) {
        warning("'radius' must be an integer >=1 (# pixels); rounding it...")
        radius = round(radius)
    }
    if (radius<1) {
        warning("'radius' must be an integer >=1 (# pixels); setting 'radius=1'...")
        radius = 1
    }
    param = list(INPUT=in.grid, RESULT=out.grid, MODE=mode,
        METHOD=method, RADIUS=radius)
    rsaga.geoprocessor("grid_filter", 0, param, ...)
}



rsaga.filter.gauss = function(in.grid, out.grid, sigma,
    radius=ceiling(2*sigma),...)
{
    in.grid = default.file.extension(in.grid,".sgrd")
    if (missing(sigma)) stop("the 'sigma' standard deviation argument (in # pixels) must be specified")
    stopifnot(sigma>0.0001)
    if (round(radius) != radius) stop("'radius' must be an integer (# pixels)")
    stopifnot(radius>=1)
    param = list(INPUT=in.grid, RESULT=out.grid, SIGMA=sigma, RADIUS=radius)
    rsaga.geoprocessor("grid_filter", 1, param, ...)
}




############################################
########     Module ta_hydrology    ########
############################################


rsaga.parallel.processing = function(in.dem, in.sinkroute, in.weight,
    out.carea, out.cheight, out.cslope, out.caspect, out.flowpath,
    step, method="mfd", linear.threshold=Inf, convergence=1.1, ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    method = match.arg.ext(method, choices=c("d8","rho8","braunschweig","dinf","mfd"),
        numeric=TRUE, ignore.case=TRUE, base=0)
    param = list( ELEVATION=in.dem )
    if (!missing(in.sinkroute)) {
        in.sinkroute = default.file.extension(in.sinkroute,".sgrd")
        param = c(param, SINKROUTE=in.sinkroute)
    }
    if (!missing(in.weight)) {
        in.weight = default.file.extension(in.weight,".sgrd")
        param = c(param, SINKROUTE=in.weight)
    }
    if (!missing(out.carea))
        param = c(param, CAREA=out.carea)
    if (!missing(out.cheight))
        param = c(param, CHEIGHT=out.cheight)
    if (!missing(out.cslope))  
        param = c(param, CSLOPE=out.cslope)
    if (!missing(step))
        param = c(param, STEP=step)
    if (!missing(out.caspect))
        param = c(param, CASPECT=out.caspect)
    if (!missing(out.flowpath))
        param = c(param, FLWPATH=out.flowpath)
    param = c(param, Method=method)
    if (is.finite(linear.threshold)) {
        param = c(param, DOLINEAR=TRUE, LINEARTHRS=linear.threshold)
    } else param = c(param, DOLINEAR=FALSE)
    
    param = c(param, CONVERGENCE=convergence)
    
    rsaga.geoprocessor("ta_hydrology", 0, param, ...)
}


rsaga.wetness.index = function( in.dem, 
    out.wetness.index, out.carea, out.cslope, 
    out.mod.carea, t.param, ...)
{
    in.dem = default.file.extension(in.dem,".sgrd")
    if (missing(out.carea)) {
        out.carea = tempfile()
        on.exit(unlink(paste(out.carea,".*",sep="")), add = TRUE)
    }
    if (missing(out.cslope)) {
        out.cslope = tempfile()
        on.exit(unlink(paste(out.cslope,".*",sep="")), add=TRUE)
    }
    if (missing(out.mod.carea)) {
        out.mod.carea = tempfile()
        on.exit(unlink(paste(out.mod.carea,".*",sep="")), add=TRUE)
    }
    param = list(DEM=in.dem, C=out.carea, GN=out.cslope, 
                 CS=out.mod.carea, SB=out.wetness.index)
    if (!missing(t.param))
        param = c(param, T=as.numeric(t.param))
    rsaga.geoprocessor("ta_hydrology",15,param,...)
}





############################################
########    Module grid_calculus    ########
############################################


rsaga.grid.calculus = function(in.grids, out.grid, formula, ...)
{
    in.grids = default.file.extension(in.grids, ".sgrd")
    in.grids = paste(in.grids, collapse = ";")
    if (any(class(formula) == "formula"))
        formula = rev( as.character(formula) )[1]
    formula = gsub(" ", "", formula)
    param = list( INPUT = in.grids, RESULT = out.grid,
                  FORMUL = formula )
    rsaga.geoprocessor("grid_calculus", 1, param, ...)
}


rsaga.linear.combination = function(in.grids, out.grid, coef, 
    cf.digits = 16, remove.zeros = FALSE, remove.ones = TRUE, ...)
{
    fmt = paste("%.", cf.digits, "f", sep = "")
    coef = sprintf(fmt, coef)
    zero = sprintf(fmt, 0)
    omit = rep(FALSE, length(coef))

    if (length(coef) == length(in.grids)) { # no intercept provided
        coef = c(NA, coef)
        omit = c(TRUE, omit)
    }
    nvars = length(coef)
    if (nvars != length(in.grids) + 1)
        stop("'coef' must have length 'length(in.grids)' or 'length(in.grids)+1'")

    # Simplify the formula by removing terms that are zero
    # (after rounding to the specified number of digits):
    if (remove.zeros)
        omit = omit | (coef == zero)
    # Zero intercept is always removed:
    omit[1] = omit[1] | (coef[1] == zero)

    # Remove zeros at the end of the coefficients:
    for (i in 1:nvars) {
        if (omit[i]) next
        # Are there any digits at all?
        if (length(grep(".", coef[i], fixed = TRUE)) == 0) next
        nc = nchar(coef[i])
        # Remove all trailing zeros:
        while (substr(coef[i], nc, nc) == "0") {
            coef[i] = substr(coef[i], 1, nc - 1)
            nc = nchar(coef[i])
        }
        # Remove trailing decimal point:
        if (substr(coef[i], nc, nc) == ".")
            coef[i] = substr(coef[i], 1, nc - 1)
    }

    # Set up the formula:
    ltrs = letters[ 1 : sum(!omit[-1]) ]
    if (!omit[1]) ltrs = c("intercept", ltrs)
    formula = paste(coef[ !omit ], ltrs, 
                    collapse = "+", sep = "*")
    formula = gsub("*intercept", "", formula, fixed = TRUE)
    formula = gsub("+-", "-", formula, fixed = TRUE)
    if (remove.ones) {
        formula = gsub("-1*", "-", formula, fixed = TRUE)
        formula = gsub("+1*", "+", formula, fixed = TRUE)
    }
    
    rsaga.grid.calculus(in.grids = in.grids[!omit[-1]], out.grid = out.grid,
        formula = formula, ...)
}




############################################
########     Module shapes_grid     ########
############################################


rsaga.contour = function(in.grid,out.shapefile,zstep,zmin,zmax,...) {
    in.grid = default.file.extension(in.grid,".sgrd")
    param = list(INPUT=in.grid,CONTOUR=out.shapefile)
    if (!missing(zmin))  param = c(param, ZMIN=as.numeric(zmin))
    if (!missing(zmax))  param = c(param, ZMAX=as.numeric(zmax))
    if (!missing(zstep)) {
        stopifnot(as.numeric(zstep)>0)
        param = c(param, ZSTEP=as.numeric(zstep))
    }
    rsaga.geoprocessor("shapes_grid", 5, param, ...)
}


rsaga.add.grid.values.to.points = function(in.shapefile,
    in.grids, out.shapefile, 
    method = c("nearest.neighbour", "bilinear",
      "idw", "bicubic.spline", "b.spline"), ...)
{
    in.grids = default.file.extension(in.grids,".sgrd")
    in.grids = paste(in.grids, collapse = ";")
    method = match.arg.ext(method, base = 0, ignore = TRUE, numeric = TRUE)
    param = list(SHAPES = in.shapefile, GRIDS = in.grids,
                RESULT = out.shapefile, INTERPOL = method)
    rsaga.geoprocessor("shapes_grid", 0, param, ...)
} # "Add Grid Values to Points" in module "shapes_grid"


rsaga.grid.to.points.randomly = function(in.grid,
    out.shapefile, freq, ...)
{
    in.grid = default.file.extension(in.grid, ".sgrd")
    out.shapefile = default.file.extension(out.shapefile, ".shp")
    if (freq < 1) stop("'freq' must be an integer >=1")
    param = list(GRID = in.grid, FREQ = freq, POINTS = out.shapefile)
    rsaga.geoprocessor("shapes_grid", 4, param, ...)
}

#rsaga.grid.to.points.randomly("dem", "pts", freq = 20)


rsaga.grid.to.points = function(in.grids, out.shapefile, 
    in.clip.polygons, exclude.nodata = TRUE, ...)
{
    in.grids = default.file.extension(in.grids,".sgrd")
    in.grids = paste(in.grids, collapse = ";")
    param = list(GRIDS = in.grids, POINTS = out.shapefile,
                 NODATA = exclude.nodata)
    if (!missing(in.clip.polygons))
        param = c(param, POLYGONS = in.clip.polygons)
    rsaga.geoprocessor("shapes_grid", 3, param, ...)
} # "Grid Values to Points" in module "shapes_grid"







# note: using "target.grid" seems to crash SAGA, but AFTER successfully
# completing module execution
rsaga.inverse.distance = function(in.shapefile, out.grid, field, 
        power = 1, maxdist = 100, nmax = 10,
        target = rsaga.target(), ...)
{
    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")
    if (power <= 0)
        stop("'power' must be >0")
    if (maxdist <= 0)
        stop("'maxdist' must be >0")
    if (nmax <= 0)
        stop("'nmax' must be >0")
    if (field < 0)
        stop("'field' must be an integer >=0")
    param = list(
        GRID = out.grid,
        SHAPES = in.shapefile,
        FIELD = field,
        POWER = power,
        RADIUS = maxdist,
        NPOINTS = nmax)
    param = c(param, target)
        
    rsaga.geoprocessor("grid_gridding", 0, param, ...)
}


rsaga.nearest.neighbour = function(in.shapefile, out.grid, field,
    target = rsaga.target(), ...)
{
    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")
    if (field < 0)
        stop("'field' must be an integer >=0")
    param = list(
        GRID = out.grid,
        SHAPES = in.shapefile,
        FIELD = field)
    param = c(param, target)
        
    rsaga.geoprocessor("grid_gridding", 1, param, ...)
}

#rsaga.nearest.neighbour("pt", "mydem", field = 0,
#    target = rsaga.target(target="header", header=read.ascii.grid.header("dem.asc")))



rsaga.modified.quadratic.shephard = function(in.shapefile, out.grid, field,
    quadratic.neighbors = 13, weighting.neighbors = 19,
    target = rsaga.target(), ...)
{
    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")
    if (field < 0)
        stop("'field' must be an integer >=0")
    if (quadratic.neighbors < 5)
        stop("'quadratic.neighbors' must be an integer >=5")
    if (weighting.neighbors < 5)
        stop("'weighting.neighbors' must be an integer >=3")
    param = list(
        GRID = out.grid,
        SHAPES = in.shapefile,
        FIELD = field,
        QUADRATIC_NEIGHBORS = quadratic.neighbors,
        WEIGHTING_NEIGHBORS = weighting.neighbors)
    param = c(param, target)
        
    rsaga.geoprocessor("grid_gridding", 2, param, ...)
}


rsaga.triangulation = function(in.shapefile, out.grid, field,
    target = rsaga.target(), ...)
{
    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")
    if (field < 0)
        stop("'field' must be an integer >=0")
    param = list(
        GRID = out.grid,
        SHAPES = in.shapefile,
        FIELD = field)
    param = c(param, target)
        
    rsaga.geoprocessor("grid_gridding", 4, param, ...)
}


rsaga.ordinary.kriging = function(in.shapefile, out.grid, 
    out.variance.grid, field, 
    model = c("spherical", "exponential", "gaussian"),
    nugget = 0, sill = 10, range = 100,
    log.transform = FALSE, maxdist = 1000, blocksize, 
    nmin = 4, nmax = 20,
    target = rsaga.target(), ...)
{
    in.shapefile = default.file.extension(in.shapefile, ".shp")
    out.grid = default.file.extension(out.grid, ".sgrd")
    if (field < 0)
        stop("'field' must be an integer >=0")
    bvariance = !missing(out.variance.grid)
    block = !missing(blocksize)
    model = match.arg.ext(model, base = 0, numeric = TRUE)
    
    param = list(
        GRID = out.grid,
        SHAPES = in.shapefile,
        FIELD = field)
    
    if (bvariance)
        param = c(param, BVARIANCE = TRUE, VARIANCE = out.variance.grid)
    if (block)
        param = c(param, BLOCK = TRUE, DBLOCK = blocksize)
        
    param = c(param, BLOG = log.transform, MODEL = model,
            NUGGET = nugget, SILL = sill, RANGE = range,
            MAXRADIUS = maxdist,
            NPOINTS_MIN = nmin, NPOINTS_MAX = nmax)
        
    param = c(param, target)
        
    rsaga.geoprocessor("geostatistics_kriging", 4, param, ...)
}







pick.from.saga.grid = function( data, filename, path, varname, 
    prec = 7, show.output.on.console = FALSE, env = rsaga.env(), ... )
{
    if (!missing(path)) if (path!="") filename = file.path(path,filename)
    temp.asc = paste(tempfile(),".asc",sep="")
    if (missing(varname)) varname = create.variable.name(filename)
    rsaga.sgrd.to.esri(filename, temp.asc, format = "ascii",
        prec = prec, show.output.on.console = show.output.on.console,
        env = env)
    on.exit(unlink(temp.asc), add = TRUE)
    data = pick.from.ascii.grid(data,temp.asc, varname = varname,...)
    invisible(data)
}

read.sgrd = function( fname, return.header = TRUE, print = 0, 
    nodata.values = c(), at.once = TRUE, prec = 7, ... )
{
    temp.fname = paste(tempfile(),".asc",sep="")
    res = rsaga.sgrd.to.esri( fname, temp.fname, prec=prec, format="ascii",
        show.output.on.console=FALSE, intern=FALSE, ... )
    on.exit(unlink(temp.fname), add = TRUE)
    if (res==0) {
        data = read.ascii.grid( temp.fname, return.header=return.header,
            print=print, nodata.values=nodata.values, at.once=at.once )
    } else
        stop("error converting the SAGA sgrd file to a temporary ASCII grid file")
    invisible(data)
}

write.sgrd = function( data, file, header = NULL, prec = 7,    
    georef = "corner", ... )
    # 'georef' argument was missing - bug fixed 2008-05-02
{
    temp.fname = paste(tempfile(),".asc",sep="")
    write.ascii.grid( data = data, file = temp.fname, header = header, 
                digits = prec, georef = georef )
    on.exit(unlink(temp.fname), add = TRUE)
    res = rsaga.esri.to.sgrd( in.grids = temp.fname, out.sgrds = file,
        show.output.on.console = FALSE, intern = FALSE, ... )
    invisible(res)
}

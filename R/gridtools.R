
set.file.extension = function(filename, extension, fsep=.Platform$file.sep) {
    if (extension=="") extension = "."
    if (substr(extension,1,1)!=".") extension = paste(".",extension,sep="")
    filename = gsub("\\",fsep,filename,fixed=TRUE)
    the.extension = get.file.extension(filename)
    filename = substr(filename,1,nchar(filename)-nchar(the.extension))
    return( paste( filename, extension, sep="") )
}


get.file.extension = function(filename, fsep=.Platform$file.sep) {
    ext = rep("",length(filename))
    has.dot.extension = substring(filename, nchar(filename))=="."
    filename = gsub("\\",fsep,filename,fixed=TRUE)
    split = strsplit(filename,fsep,fixed=TRUE)
    split = sapply( split, function(x) x[length(x)] )
    split = strsplit(split,".",fixed=TRUE)
    ext = sapply( split, function(x) x[length(x)] )
    has.extension = sapply(split,length) > 1
    ext[ has.extension ] = paste(".",ext[has.extension],sep="")
    ext[ !has.extension ] = ""
    ext[ has.dot.extension ] = "."
    return(ext)
}


default.file.extension = function(filename, extension, force=FALSE) {
    if (force) {
        filename = set.file.extension(filename,extension)
    } else {
        use.default = (get.file.extension(filename)=="")
        if (any(use.default))
            filename[use.default] = set.file.extension(filename[use.default],extension)
    }
    return(filename)
}


create.variable.name = function( filename, prefix = NULL, fsep = .Platform$file.sep )
{
    has.dot.extension = substring(filename, nchar(filename))=="."
    varname = gsub("\\",fsep,filename,fixed=TRUE)
    varname = strsplit(varname,fsep,fixed=TRUE)[[1]]
    varname = varname[length(varname)]
    varname = strsplit(varname,".",fixed=TRUE)[[1]]
    if (length(varname) > 1) {
        if (!has.dot.extension)
            varname = varname[ 1 : (length(varname)-1) ]
        varname = paste(varname,collapse=".")
    }
    varname = gsub("-",".",gsub("_",".",varname))
    if (!is.null(prefix)) if (prefix!="") 
        varname = paste(prefix,".",varname,sep="")
    return(varname)
}


read.ascii.grid.header = function(file,...)
{
    if (is.character(file)) {
        file = default.file.extension(file,".asc")
        file = file(file,open="r")
        on.exit(close(file))
    }
    hdr = scan(file, what=list(attribute="",value=numeric(0)), nlines=6, quiet=TRUE, ...)
    hdr$attribute = tolower(hdr$attribute)
    res = hdr$value
    names(res) = hdr$attribute
    res = as.list(res)
    if (!is.null(res$xllcorner) & !is.null(res$yllcorner)) {
        res$xllcenter = res$xllcorner + res$cellsize / 2
        res$yllcenter = res$yllcorner + res$cellsize / 2
    } else if (!is.null(res$xllcenter) & !is.null(res$yllcenter)) {
        res$xllcorner = res$xllcenter - res$cellsize / 2
        res$yllcorner = res$yllcenter - res$cellsize / 2
    }
    return(res)
}


write.ascii.grid.header = function(file, header, georef, dec=".")
{
    if (missing(georef)) {
        # determine from the 'header' if georeferencing should refer
        # to corner or center of lower left grid cell:
        i.corner = min(c(Inf,grep("corner",tolower(names(header)))))
        i.center = min(c(Inf,grep("center",tolower(names(header)))))
        stopifnot(i.center!=i.corner) # this can only happen if header is corrupt
        georef = "corner"
        if (i.center < i.corner) georef = "center"
    } else {
        georef = match.arg(tolower(georef),choices=c("corner","center"))
    }
    fmt = c( "%-14s%-.0f", "%-14s%-.0f", "%-14s%-f", "%-14s%-f", "%-14s%-f", "%-14s%-f" )
    nm = c( "ncols", "nrows", paste(c("xll","yll"),georef,sep=""), "cellsize", "nodata_value" )
    if (is.character(file))  {
        file = default.file.extension(file,".asc")
        file = file(file, open="w")
        on.exit(close(file))
    } else {
        if (!isOpen(file,"write"))
            stop("'file' must be a file name or a connection opened for reading")
    }
    for (i in 1:length(nm)) {
        entry = gsub(".", dec, sprintf(fmt[i],nm[i],as.numeric(header[[ nm[i] ]])), fixed=TRUE)
        write( entry, file=file, append=(i>1) )
    }
    invisible()
}


read.ascii.grid = function( file, return.header = TRUE, print = 0,
    nodata.values = c(), at.once = TRUE )
{
    if (is.character(file)) {
        file = default.file.extension(file,".asc")
        con = file(file,open="r")
        on.exit(close(con))
    } else {
        if (!isOpen(file,"read"))
            stop("'file' must be a file name or a connection opened for reading")
    }
    hdr = read.ascii.grid.header(con)
    if (at.once) {
        data = scan(con, nlines=hdr$nrows, quiet=TRUE)
        data = matrix(data, ncol=hdr$ncols, nrow=hdr$nrows, byrow=TRUE)
        na = data == hdr$nodata_value
        for (na.val in nodata.values)  na = na | (data==na.val)
        if (any(na)) data[na] = NA
    } else {
        data = matrix(NA,ncol=hdr$ncols,nrow=hdr$nrows)
        for (i in 1:hdr$nrows) {
            if (print == 2) cat(i, " ", ifelse(round(i/20)==i/20,"\n","") )
            if (print == 1) if (round(i/100)==i/100) cat(i, " ", ifelse(round(i/1000)==i/1000,"\n",""))
            x = scan(con,nlines=1,quiet=TRUE)
            na = x == hdr$nodata_value
            for (na.val in nodata.values)  na = na | (x==na.val)
            if (any(na)) x[na] = NA
            data[i,] = x
        }
    }
    if (print == 2) cat("\nDone!\n")
    if (print == 1) cat("\n")
    if (return.header) data = list( header = hdr, data = data )
    invisible(data)
}


read.Rd.grid = function( fname, return.header = TRUE )
{
    fname = default.file.extension(fname,".Rd")
    load(fname)
    stopifnot(exists("data", envir=parent.frame()))
    if (is.list(data)) 
        stopifnot( (names(data)==c("header","data")) | (names(data)==c("data","header")) )
    if (return.header & !is.list(data)) { 
        warning("header missing")
        data = list(header=NA,data=data)
    } else if (!return.header & is.list(data)) 
        data = data$data
    invisible(data)
}



write.ascii.grid = function( data, file, header = NULL, write.header = TRUE, 
    digits, dec = ".", georef = "corner" ) 
{
    if (is.character(file)) {
        file = default.file.extension(file, ".asc")
        con = file(file,open="w")
        on.exit(close(con))
    } else {
        if (!isOpen(file,"write"))
            stop("'file' must be a file name or a connection opened for writing")
    }
    if (is.list(data)) {
        stopifnot( ("data" %in% names(data)) )
        if (write.header & is.null(header)) {
            stopifnot("header" %in% names(data))
            header = data$header
        }
        data = data$data
    } else stopifnot(is.matrix(data))
    if (!missing(digits)) data = round(data,digits=digits)
    if (write.header)  write.ascii.grid.header(con,header,dec=dec,georef=georef)
    write.table(data, file=con, append=TRUE, quote=FALSE,
        na=as.character(header$nodata_value),
        row.names=FALSE, col.names=FALSE, dec=dec)
}

write.Rd.grid = function(data, file, header=NULL, write.header=TRUE, 
    compress=TRUE)
{
    file = default.file.extension(file,".Rd")
    if (is.list(data)) {
        stopifnot( ("data" %in% names(data)) )
        if (write.header & is.null(header)) {
            stopifnot("header" %in% names(data))
            header = data$header
        }
        data = data$data
    } else stopifnot(is.matrix(data))
    if (write.header)  data = list( header = header, data = data )
    save(data, file=file, ascii=FALSE, compress=compress)
}


pick.from.shapefile = function(data, shapefile, X.name="x", Y.name="y", ...)
{
    require(shapefiles)
    shapefile = set.file.extension(shapefile,"")
    shapefile = substr(shapefile,1,nchar(shapefile)-1) # remove "." at the end
    src = read.shapefile(shapefile)
    src = add.xy(src)
    src = src$dbf[[1]]
    if (X.name != "XCOORD") {
        if (X.name %in% names(src)) {
            src[,X.name] = src[,"XCOORD"]
            src = src[,names(src)!="XCOORD"]
        }
    }
    if (Y.name != "YCOORD") {
        if (Y.name %in% names(src)) {
            src[,Y.name] = src[,"YCOORD"]
            src = src[,names(src)!="YCOORD"]
        }
    }
    data = pick.from.points(data,src,X.name=X.name,Y.name=Y.name,...)
    return(data)
}


pick.from.points = function(data, src, pick, 
    method = c("nearest.neighbour","krige"), set.na = FALSE,
    radius = 200, nmin = 0, nmax = 100,
    sill = 1, range = radius, nugget = 0,
    model = vgm(sill-nugget,"Sph",range=range,nugget=nugget),
    log = rep(FALSE,length(pick)),
    X.name = "x", Y.name = "y", cbind = TRUE )
{
    method = match.arg(method)
    if (!is.data.frame(data))  data = as.data.frame(data)
    if (missing(pick)) {
        pick = colnames(src)
        pick = pick[ (pick != X.name) & (pick != Y.name) ]
    }
    nc = rep(NA,length(pick))
    for (p in 1:length(pick)) {
        colnm = colnames(data)
        if (any(colnm==pick[p])) {
            nc[p] = which(colnm==pick[p])
            if (set.na)  data[,nc[p]] = rep(NA,nrow(data))
        } else {
            data = cbind(data,rep(NA,nrow(data)))
            colnames(data) = c(colnm,pick[p])
            nc[p] = ncol(data)
        }
    }
    stopifnot(!any(is.na(nc)))
    
    # prepare the source data.frame:
    src = src[ !is.na(src[,X.name]) & !is.na(src[,Y.name]) , ]
    if (nrow(src)==0) {
        if (!cbind) data = data[,pick]
        return(data)
    }
    the.src = src
    rm(src)

    if (method=="krige") {
        require(gstat)
        loc = as.formula(paste("~",X.name,"+",Y.name))
        for (p in 1:length(pick)) {
            form = as.formula(paste(pick[p],"~ 1"))
            src = the.src[ !is.na(the.src[,pick[p]]) , ]
            if (nrow(src)==0) next
            #src$tmp = src[,pick[p]]
            krg = krige(
                form, loc=loc, data=src, newdata=data,
                model=model, nmax=nmax, nmin=nmin,
                maxdist=radius ) $ var1.pred
            sel = !is.na(krg)
            data[sel,pick[p]] = krg[sel]
        }
    } else if (method=="nearest.neighbour") {
        for (i in 1:nrow(data)) {
            if (is.na(data[i,X.name]) | is.na(data[i,Y.name])) next
            di = sqrt((the.src[,X.name]-data[i,X.name])^2+(the.src[,Y.name]-data[i,Y.name])^2)
            mindi = min(di)
            if ((mindi) > radius) next
            
            wh = which(di == mindi)
            if (length(wh)>1) wh = sample(wh)[1]
            for (p in 1:length(pick))
                data[i,pick[p]] = the.src[wh,pick[p]]
        }
    }
    if (!cbind) data = data[,pick]
    return(data)
}


# test with zipfile connection!! how slow?

pick.from.ascii.grid = function( data, file, 
    path, varname, prefix,
    method = c("nearest.neighbour","krige"),
    nodata.values = c(-9999,-99999), at.once = TRUE, quiet = TRUE,
    X.name = "x", Y.name = "y", nlines = Inf,
    cbind = TRUE, range, radius, ... )
{
    stopifnot(is.data.frame(data))
    stopifnot( X.name %in% colnames(data) )
    stopifnot( Y.name %in% colnames(data) )

    # determine variable name from file name if 'varname' is missing:
    if (missing(varname)) {
        if (is.character(file)) {
            varname = create.variable.name(file)
        } else {
            if (cbind) {
                stop("'varname' must be specified unless 'file' is a character string with the filename")
            } else
                varname = paste("TEMP",X.name,Y.name,sep=".")
        }
    }

    # add a prefix to the variable?
    if (!missing(prefix)) if (!is.null(prefix)) if (prefix!="")
        varname = paste(prefix,varname,sep=".")

    method = match.arg(method)
    
    if (is.character(file)) {
        file = default.file.extension(file,".asc")
        if (!missing(path)) if (path!="") file = file.path(path,file)
        if (!file.exists(file)) stop("file ",file," not found")
        con = file(file,open="r")
        on.exit(close(con))
    } else {
        if (!isOpen(file,"read"))
            stop("'file' must be a file name, or a connection opened for reading")
    }

    # prepare 'data': add new column if necessary
    the.colnames = colnames(data)
    if (varname %in% the.colnames) {
        nc = which(the.colnames==varname)
    } else {
        data = cbind( data, rep(NA,nrow(data) ) )
        colnames(data) = c( the.colnames, varname )
        nc = ncol(data)
    }

    if (method=="krige")
    {
        if (!at.once)
            warning("row-by-row processing of grids is not yet implemented for kriging interpolation\n",
                "trying to process the whole grid at once...")
        src = read.ascii.grid(file,nodata.values=nodata.values)
        src = grid.to.xyz(src, colnames=c(X.name,Y.name,varname))
        if (missing(radius)) radius = 2.5 * hdr$cellsize
        if (missing(range)) range = radius
        if (range > radius) radius = range
        data = pick.from.points(data, src, pick=varname,
            X.name=X.name, Y.name=Y.name,
            method="krige", range=range, radius=radius,...)

    } else if (method=="nearest.neighbour")
    {
        hdr = read.ascii.grid.header(file)
        nodata.values = unique(c(nodata.values,hdr$nodata_value))

        select = cbind( 1 + round( (data[,X.name] - (hdr$xllcorner+hdr$cellsize/2)) / hdr$cellsize ),
                        1 + round( hdr$nrows - (data[,Y.name] - (hdr$yllcorner-hdr$cellsize/2)) / hdr$cellsize )  )
    
        if (any(!is.na(select)))
        {
            nr = nrow(select)
            nlines = min( hdr$nrows, max(select[,2],na.rm=TRUE), nlines )
        
            if (!at.once)
            {
                for (i in 1:nlines) {
                    if (!quiet & ((i/10)==floor(i/10))) {
                        cat(i," ")
                        if ( (i/100) == floor(i/100) )  cat("\n")
                    }
                    v = scan(file,nlines=1,quiet=TRUE)
                    if (length(v) < hdr$ncols) {
                        warning("grid row too short - corrupt grid file? filling with NA's...")
                        v = c(v, rep(nodata.values[1], hdr$ncols-length(v)))
                    } else if (length(v) > hdr$ncols) {
                        warning("grid row too long - corrupt grid file? ignoring redundant data...")
                        v = v[ 1:hdr$ncols ]
                    }
                    ass = ( select[,2] == i )
                    if (any(ass)) {
                        for (na in nodata.values) v[ v==na ] = NA
                        for (j in which(ass)) {
                            if ((select[j,1]>=1) & (select[j,1]<=hdr$ncols))
                                data[j,nc] = v[ select[j,1] ]
                        }
                        if (!quiet) cat("\n matches:",which(ass),"\n")
                    }
                }
            } else # if (at.once)
            {
                v = read.table(file)
                for (na in nodata.values) v[ v==na ] = NA
                for (j in 1:nr) {
                    if ( all(select[j,]>=1) & all(select[j,]<=c(ncol(v),nrow(v))) )
                        data[j,nc] = v[select[j,2],select[j,1]]
                }
            }
        } else {
            warning("all 'data' points are outside grid area")
        }
    } # end if (method=="nearest.neighbour")
    
    if (!cbind) data = data[,nc]
    return(data)
}


grid.to.xyz = function(data,header,varname="z",colnames=c("x","y",varname)) {
    if (missing(header)) {
        if (is.list(data)) {
            header = data$header
        } else {
            header = list(
                ncols = ncol(data),
                nrows = nrow(data),
                xllcenter = 0,
                yllcenter = 0,
                cellsize = 1,
                xllcorner = -0.5,
                yllcorner = -0.5 )
        }
    }
    if (is.list(data)) data = data$data
    data = data.frame(
        x = header$xllcenter + rep( seq( 0, (header$ncols-1) * header$cellsize, by=header$cellsize ), header$nrows ),
        y = rep( header$yllcenter + seq( (header$nrows-1) * header$cellsize, 0, by=-header$cellsize ), each=header$ncols ),
        z = as.vector(t(data)) )
    colnames(data) = colnames
    invisible(data)
}




centervalue = function(x) {
    i = ceiling(ncol(x) / 2)
    return(x[i,i])
}

resid.median = function(x) {
    if (missing(x)) return("rmed")
    return( median(x,na.rm=TRUE) - centervalue(x) )
}

resid.minmedmax = function(x) {
    if (missing(x)) return(c("rmin","rmed","rmax"))
    return( c(min(x,na.rm=TRUE),median(x,na.rm=TRUE),max(x,na.rm=TRUE)) - centervalue(x) )
}

relative.position = function(x) {
    if (missing(x)) return("relpos")
    zmin = min(x,na.rm=TRUE)
    zmax = max(x,na.rm=TRUE)
    return( (centervalue(x) - zmin) / (zmax - zmin) )
}

resid.quantile = function(x,probs) {
    if (missing(x)) return(NULL)
    return(quantile(x-centervalue(x),probs=probs,na.rm=TRUE,names=FALSE))
}

resid.quartiles = function(x) {
    if (missing(x)) return(c("r25","r50","r75"))
    return(quantile(x-centervalue(x),probs=c(0.25,0.5,0.75),na.rm=TRUE,names=FALSE))
}

relative.rank = function(x,ties.method="average") {
    if (missing(x)) return("relrank")
    x = as.vector(x)
    n = sum(!is.na(x))
    return( (rank(x,ties.method=ties.method)[(length(x)+1)/2]-1) / (n-1))
}

wind.shelter.prep = function(radius,direction,tolerance,cellsize=90) {
    nc = nr = 2*ceiling(radius)+1
    mask = matrix(TRUE,ncol=nc,nrow=nr)
    for (j in 1:nc) {
        for (i in 1:nr) {
            if ((i==j) & (i==((nr+1)/2))) next
            xy = c( j-(nc+1)/2, (nr+1)/2-i )
            xy = xy / sqrt(xy[1]^2+xy[2]^2)
            if ( xy[2]>0)  a = asin(xy[1])  else a = pi - asin(xy[1])
            if (a < 0) a = a + 2*pi
            d = abs(direction-a)
            if (d>2*pi) d = d-2*pi
            d = min(d,2*pi-d)
            if (d<=tolerance) mask[i,j] = FALSE
        }
    }
    dist = matrix(NA,ncol=nc,nrow=nr)
    for (i in 1:nr) for (j in 1:nc) {
        xy = c( j-(nc+1)/2, (nr+1)/2-i )
        dist[i,j] = sqrt(xy[1]^2+xy[2]^2) * cellsize
    }
    list( mask = mask, dist = dist )
}


wind.shelter = function(x,prob=NULL,control) {
    if (missing(x)) return("windshelter")
    if (missing(control)) stop("need 'control' argument - call 'wind.shelter.prep' first")
    ctr = centervalue(x)
    x[control$mask] = NA
    res = NA
    if (!all(is.na(x))) {
        x = atan((x-ctr)/control$dist)
        if (is.null(prob)) {
            res = max(x,na.rm=TRUE)
        } else res = quantile(x,probs=prob,na.rm=TRUE)
    }
    return(res)
}



focal.function = function( in.grid, in.factor.grid, out.grid.prefix,
    path=NULL, in.path=path, out.path=path,
    fun, varnames,
    radius=0, is.pixel.radius=TRUE,
    valid.range=c(-Inf,Inf), nodata.values=c(), out.nodata.value, 
    search.mode=c("circle","square"),
    digits=4, dec=".", quiet=TRUE, nlines=Inf,
    mw.to.vector = FALSE, mw.na.rm = FALSE, ... )
{
    if (radius > 0) {
        search.mode = match.arg(search.mode)
        if (mw.na.rm & !mw.to.vector)
            warning("'mw.na.rm=TRUE' only meaningful if moving window matrix is\n",
                "converted to a vector ('mw.to.vector=TRUE')")
    }
    
    # prepare input file:
    if (!is.null(in.path)) if (in.path!="")
        in.grid = file.path(in.path,in.grid)
    in.grid = default.file.extension(in.grid,".asc")
    in.file = file(in.grid,open="r")
    on.exit(close(in.file))
    in.hdr = read.ascii.grid.header(in.file,dec=dec)
    nodata.values = unique(c(nodata.values,in.hdr$nodata_value))
    nlines = max( 1, min( c(nlines,in.hdr$nrows), na.rm=TRUE ) )

    if (missing(in.factor.grid)) in.factor.grid = NULL
    if ((radius<=0) & !is.null(in.factor.grid)) {
        warning("'in.factor.grid' is ignored - only meaningful for 'radius>0'")
        in.factor.grid = NULL
    }
    if (!is.null(in.factor.grid)) {
        in.factor.grid = file.path(in.path,in.factor.grid)
        in.factor.grid = default.file.extension(in.factor.grid,".asc")
        in.factor.file = file(in.factor.grid,open="r")
        on.exit(close(in.factor.file))
        in.factor.hdr = read.ascii.grid.header(in.factor.file,dec=dec)
        if (in.hdr$ncols != in.factor.hdr$ncols |
            in.hdr$nrows != in.factor.hdr$nrows |
            in.hdr$cellsize != in.factor.hdr$cellsize |
            in.hdr$xllcorner != in.factor.hdr$xllcorner |
            in.hdr$yllcorner != in.factor.hdr$yllcorner)
            stop("input grid and factor grid must have same extent and cellsize")
    }

    # build output filenames:
    if (missing(varnames)) {
        # check if the function will return a vector with variable names
        # when called without arguments:
        varnames = try(do.call(fun,list()),silent=TRUE)
        if (class(varnames) == "try-error") {
            if (is.character(fun)) {
                varnames = gsub(".","",fun,fixed=TRUE)
            } else if (is.function(fun)) {
                varnames = deparse(substitute(fun))
            } else stop("unable to determine 'varnames' from 'fun'")
            varnames = abbreviate(varnames,6)
        }
    }
    if (missing(out.grid.prefix)) out.grid.prefix = ""
    if (is.null(out.grid.prefix)) out.grid.prefix = ""
    stopifnot(length(varnames) == length(unique(varnames)))
    do.paste = (varnames!="") & (out.grid.prefix!="")
    out.filenames = paste( out.grid.prefix, c("","_")[do.paste+1], varnames, sep="" )
    out.filenames = default.file.extension(out.filenames,".asc")
    if (!is.null(out.path)) if (out.path!="")
        out.filenames = file.path(out.path,out.filenames)
    if (any(out.filenames==in.grid)) stop("one of the output file names is identical to the input file name")

    # prepare output files:
    N.out = length(out.filenames)
    out.files = as.list(1:N.out)
    out.hdr = in.hdr
    if (missing(out.nodata.value)) out.nodata.value = in.hdr$nodata_value
    out.hdr$nodata_value = out.nodata.value
    for (k in 1:N.out) {
        out.files[[k]] = file(out.filenames[k],open="w")
        write.ascii.grid.header(out.files[[k]],out.hdr,dec=dec)
    }
    on.exit( for (k in 1:N.out) close(out.files[[k]]) )
    fmt = paste("%.",digits,"f",sep="")

    if (radius <= 0) {
        # Apply 'fun' as a local function:
    
        # Process one line at a time:
        for (i in 1:nlines) {
            if (!quiet) if ((i %% 10)==0) cat("*")
            if (!quiet) if ((i %% 100)==0) cat("\n")
            
            # Read one line at a time:
            v0 = scan(in.file,nlines=1,quiet=TRUE,dec=dec)
            if (length(v0) != in.hdr$ncols) {
                warning("grid line does not have NCOLS values")
                v0 = c( v0, rep(NA,length(v)-length(v0)) )
            }
            for (na in nodata.values)  v0[ v0==na ] = NA
            v0[ v0 < valid.range[1] ] = NA
            v0[ v0 > valid.range[2] ] = NA
                
            res = matrix(NA,ncol=in.hdr$ncol,nrow=N.out)
            for (j in 1:in.hdr$ncol) {
                r = do.call(fun,list(v0[j],...))
                res[,j] = r
            }
            res[ is.na(res) ] = out.nodata.value
            for (k in 1:N.out) {
                txt = paste(sprintf(fmt,res[k,]),collapse=" ")
                if (dec!=".") txt = gsub(".",dec,txt,fixed=TRUE)
                writeLines(txt,con=out.files[[k]])
            }
        }
    
    } else { # if (radius > 0)
    
        if (!is.pixel.radius) radius = radius / in.hdr$cellsize
        exact.radius = radius
        radius = ceiling(radius)
    
        # 'v' is a matrix that will receive a set of rows copied from the grid;
        # it must be a bit wider than the grid so the moving window can move over
        # it without having to worry about edge effects:
        v = matrix( NA, ncol=in.hdr$ncols+2*radius, nrow=2*radius+1 )
        # 'fac': same for in.factor.grid, if available:
        if (!is.null(in.factor.grid))
            fac = matrix( NA, ncol=in.hdr$ncols+2*radius, nrow=2*radius+1 )
        # 'f' will be the mask of a moving window in case of a circular window:    
        if (search.mode=="circle") {
            f = matrix(FALSE,ncol=2*radius+1,nrow=2*radius+1)
            for (i in ((-1)*radius):radius)
                for (j in ((-1)*radius):radius)
                    if (sqrt(i^2+j^2) > exact.radius)
                        f[ i+radius+1, j+radius+1 ] = TRUE
        }
        
        # the look-ahead step:
        for (i in (radius+1):(2*radius)) {
            v[i+1,] = c( rep(NA,radius), scan(in.file,nlines=1,quiet=TRUE,dec=dec), rep(NA,radius) )
            if (!is.null(in.factor.grid))
                fac[i+1,] = c( rep(NA,radius), scan(in.factor.file,nlines=1,quiet=TRUE,dec=dec), rep(NA,radius) )
        }
        # Process nodata values:
        for (na in nodata.values)  v[ v==na ] = NA
        v[ v < valid.range[1] ] = NA
        v[ v > valid.range[2] ] = NA
        # Process nodata values of the factor grid:
        if (!is.null(in.factor.grid)) {
            fac[ fac==in.factor.hdr$nodata_value ] = NA
            v[ is.na(fac) ] = NA
        }
        
        # Process the grid line by line:
        for (i in 1:nlines) {
            if (!quiet) if ((i %% 10)==0) cat("*")
            if (!quiet) if ((i %% 100)==0) cat("\n")
            
            if (i <= nlines - radius) {
                # Read a line from the grid file:
                v0 = scan(in.file,nlines=1,quiet=TRUE,dec=dec)
                if (length(v0) != in.hdr$ncols) { # check if corrupt
                    warning("grid line does not have NCOLS values")
                    v0 = c( v0, rep(NA,ncol(v)-length(v0)) )
                }
                # process all the nodata values:
                for (na in nodata.values)  v0[ v0==na ] = NA
                v0[ v0 < valid.range[1] ] = NA
                v0[ v0 > valid.range[2] ] = NA

                # Read a line from the factor grid:
                if (!is.null(in.factor.grid)) {
                    fac0 = scan(in.factor.file,nlines=1,quiet=TRUE)
                    if (length(fac0) != in.factor.hdr$ncols) {
                        warning("factor grid line does not have NCOLS values")
                        fac0 = c( fac0, rep(NA,ncol(fac)-length(fac0)) )
                    }
                    fac0[ fac0 == in.factor.hdr$nodata_value ] = NA
                    # Pass NA's on to the grid itself:
                    v0[ is.na(fac0) ] = NA
                }
            } else {
                v0 = rep(NA,in.hdr$ncols)
                if (!is.null(in.factor.grid))  fac0 = v0
            }
            
            # Add new line to the look-ahead buffer:
            v = rbind( v[2:(2*radius+1),], t(c( rep(NA,radius), v0, rep(NA,radius) )) )
            if (!is.null(in.factor.grid))
                fac = rbind( fac[2:(2*radius+1),], t(c( rep(NA,radius), fac0, rep(NA,radius) )) )
            
            # Apply the 'fun'ction to each grid column:
            res = matrix(NA,ncol=in.hdr$ncol,nrow=N.out)
            for (j in 1:in.hdr$ncol) {
                w = v[,j:(j+2*radius)]
                if (search.mode=="circle")  w[f] = NA
                if (!is.null(in.factor.grid)) {
                    facw = fac[,j:(j+2*radius)]
                    the.fac = centervalue(facw)
                    if (is.na(the.fac)) {
                        w = NA
                    } else
                        w[ facw != the.fac ] = NA
                }
                if (!all(is.na(w))) {
                    if (mw.to.vector) {
                        w = as.vector(w)
                        if (mw.na.rm) w = w[!is.na(w)]
                    }
                    r = do.call(fun,list(w,...))
                    res[,j] = r
                }
            }
            res[ is.na(res) ] = out.nodata.value
            for (k in 1:N.out) {
                txt = paste(sprintf(fmt,res[k,]),collapse=" ")
                if (dec!=".") txt = gsub(".",dec,txt,fixed=TRUE)
                writeLines(txt,con=out.files[[k]])
            }
        }
    } # end if (radius > 0)
    
    if (!quiet)  cat("\nDone.\n")
    return(out.filenames)
}



gapply = function(in.grid,fun,varnames,mw.to.vector=TRUE,mw.na.rm=TRUE,...) {
    # build output filenames:
    if (missing(varnames)) {
        # check if the function will return a vector with variable names
        # when called without arguments:
        varnames = try(do.call(fun,list()),silent=TRUE)
        if (class(varnames) == "try-error") {
            if (is.character(fun)) {
                varnames = gsub(".","",fun,fixed=TRUE)
            } else if (is.function(fun)) {
                varnames = deparse(substitute(fun))
            } else stop("unable to determine 'varnames' from 'fun'")
            varnames = abbreviate(varnames,6)
        }
    }
    focal.function(in.grid=in.grid,fun=fun,varnames=varnames,
        mw.to.vector=mw.to.vector,mw.na.rm=mw.na.rm,...)
}

local.function = function( ... ) {
    focal.function(..., radius=0,
        in.factor.grid=NULL, search.mode=NULL, is.pixel.radius=NULL,
        mw.to.vector=FALSE, mw.na.rm=FALSE )
}

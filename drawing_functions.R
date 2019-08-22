## return the points of an arc centered on x, y, from a.beg to a.end and with a radious
## of r
arcPoints <- function( x, y, r, a.beg, a.end, a.sep=0.01, a.n=(a.end-a.beg)/a.sep, degrees=FALSE){
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    if(a.end < a.beg)
        a.end <- a.end + 2 * pi
        
    angles <- seq(a.beg, a.end, a.sep)
    if( angles[ length(angles) ] != a.end )
        angles <- c(angles, a.end)
    
    a.x <- x + sin( angles ) * r
    a.y <- y + cos( angles ) * r
    pts <- matrix( c(a.x, a.y), ncol=2 )
    colnames(pts) <- c('x', 'y')
    pts
}

## draw an arc, as a line
## centered on x,y, and with radius of r+depth and r-depth..
## the angles should not be less than -pi though, more are possible.. 
lineArc <- function(x, y, r, a.beg, a.end, a.sep=NA, degrees=FALSE, label=NULL, label.col=1, ...){
    ## set the default here... 
    if(is.na(a.sep))
        a.sep = 0.01
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    if(a.beg > a.end)
        a.end <- a.end + 2 * pi
        
    angles <- seq(a.beg, a.end, by=a.sep)
    if(angles[ length(angles) ] != a.end )
        angles <- c(angles, a.end)
    a.x <- x + sin( angles ) * r
    a.y <- y + cos( angles ) * r

    lines( a.x, a.y, ... )

    if(!is.null(label)){
        mid.a <- (a.beg + a.end)/2
        ## but the rotation angle we need for the text needs to be in degrees
        rot.a <- -180 * mid.a / pi
        text( x + sin( mid.a ) * r, y + cos( mid.a ) * r,
              label, srt=rot.a, col=label.col )
    }
}

rad2pi <- function(r){
    180 * r/pi
}

clamp <- function(v, min=-1, max=1){
    v <- ifelse( v < min, min, v )
    ifelse( v > max, max, v )
}

## this wont work with angles that are greater than 2pi..
## we could do that by making a while loop, but that wouldn't
## be compatible with vector arithmetic
internalAngle <- function(a1, a2){
    a1 <- ifelse( a1 > pi, a1 - 2*pi, a1 )
    a2 <- ifelse( a2 > pi, a2 - 2*pi, a2 )
    a <- sort(c(a1, a2))
    mid.angle <- mean(a)
    angle <- a[2] - a[1]
    if( angle > pi){
        angle <- 2*pi - (a[2]-a[1])
        a <- rev(a)
    }
    if(mid.angle < a[1])
        mid.angle <- mid.angle + pi
    return(c('angle'=angle, 'a1'=a[1], 'a2'=a[2], mid=mid.angle ))
}

## x, y, the center of a circle
## r radius of the circle
## the begining and end angles to connect
connectingArc <- function(x, y, r, a1, a2, draw=TRUE, ...){
    o1 <- c(x, y)
    a1.p <- o1 + c( r * sin(a1), r * cos(a1) )
    a2.p <- o1 + c( r * sin(a2), r * cos(a2) )

    ## the internal angle is needed to determine the location
    ## of the connecting circle from which the arc is drawn
    i.a <- internalAngle(a1, a2)  ## c(angle, a1, a2, mid.angle)

    ## the length of the hypotheneuse connecting two tangents
    ## from the two points specified by a1 and a2
    h2 <- r / abs(cos( i.a[1]/2 ))
    r2 <- sqrt( h2^2 - r^2 )
    o2 <- o1 + c(h2 * sin(i.a[4]), h2 * cos(i.a[4]) )

    b1.a = asin( clamp((a1.p - o2)[1] / r2) )
    b1.b = acos( clamp((a1.p - o2)[2] / r2) )
    b1 <- ifelse( b1.a > 0, b1.b, -b1.b )
    
    b2.a = asin( clamp((a2.p - o2)[1] / r2) )
    b2.b = acos( clamp((a2.p - o2)[2] / r2) )
    b2 <- ifelse( b2.a > 0, b2.b, -b2.b )

    b.i <- internalAngle(b1, b2)
    if(draw)
        lineArc( o2[1], o2[2], r2, b.i[2], b.i[3], ... )
    invisible( c('x'=o2[1], 'y'=o2[2], 'r'=r2, 'a.beg'=b.i[2], 'a.end'=b.i[3]) )
}

## draw an arc, as a polygon..
## centered on x,y, and with radius of r+depth and r-depth..
polygArc <- function(x, y, r, depth, a.beg, a.end, a.sep=NA, degrees=FALSE, label=NULL, label.col=1, label.cex=1, ...){
    ## set the default here... 
    if(is.na(a.sep))
        a.sep = 0.01
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    if(a.beg > a.end)
        a.end <- a.end + 2 * pi
        
    angles <- seq(a.beg, a.end, by=a.sep)
    if(angles[length(angles)] != a.end)
        angles <- c(angles, a.end)
    
    a.x <- sin( angles )
    a.y <- cos( angles )
    outer.x <- x + (a.x * (r + depth/2))
    outer.y <- y + (a.y * (r + depth/2))

    inner.x <- x + (a.x * (r - depth/2))
    inner.y <- y + (a.y * (r - depth/2))

    polygon( c(outer.x, rev(inner.x)), c(outer.y, rev(inner.y)), ... )

    if(!is.null(label)){
        mid.a <- (a.beg + a.end)/2

        ## but the rotation angle we need for the text needs to be in degrees
        rot.a <- -180 * mid.a / pi
        text( x + sin( mid.a ) * r, y + cos( mid.a ) * r,
              label, srt=rot.a, col=label.col, cex=label.cex )
    }
}

## the first and last points are taken as anchored points
## the amount of distortion on any point is a function of its distance
## from either of these points along the curve specified by the points
## pts is a matrix containing two columns giving the x and y positions of the
## points
anchoredDistort <- function( pts, dist.v ){
    x.diff <- diff( pts[,1] )
    y.diff <- diff( pts[,2] )
    dists <- sqrt( x.diff^2 + y.diff^2 ) 
    fwd.dist <- c(0, cumsum( dists ))
    rev.dist <- rev(c(0, cumsum( rev(dists) )))
    a.dist <- sqrt( fwd.dist * rev.dist )
    ## and then simply take,,
    pts[,1] <- pts[,1] + dist.v[1] * (a.dist / max(a.dist))
    pts[,2] <- pts[,2] + dist.v[2] * (a.dist / max(a.dist))
    pts
}

## x is taken as positions along an arc running from angle, a.beg to a.end
plotPolar <- function(o.x, o.y, x, y, r, y.depth, a.beg, a.end, degrees=FALSE, drawBorders=TRUE, drawCenter=TRUE, ...){
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    x.r <- range(x)
    x.o <- (x - x.r[1]) / (x.r[2] - x.r[1])
    a <- a.beg + x.o * (a.end - a.beg)

    y.r <- range(y)
    y.o <- (y - mean(y.r)) / (y.r[2] - y.r[1])

    y.x <- o.x + sin( a ) * (r + y.o * y.depth)
    y.y <- o.y + cos( a ) * (r + y.o * y.depth)

    if(drawCenter)
        lineArc(o.x, o.y, r, a.beg, a.end, lty=2)
    if(drawBorders){
        lineArc(o.x, o.y, r + y.depth/2, a.beg, a.end )
        lineArc(o.x, o.y, r - y.depth/2, a.beg, a.end )
    }
    
    points(y.x, y.y, ...)
    
}

## if useNormals is true then the user may specify two additional points; one point
## extending from the first and last position for calculating the normals.
pathLetters <- function( x, y, word, cex=NA, cex.adj=0.75, useNormals=FALSE, ... ){
    chars <- unlist(strsplit(word, ''))
    ## to work out a reasonable cex we need to know the space between the letters
    ## that is the distances between the positions in x and y
    l <- length(chars)
    if(useNormals){
        ## we could consider using lowess / loess / smooth.spline / predict
        ## to get smoother transitions, but I think this is pretty OK to start with
        if(length(x) == length(chars) + 2){
            x.x <- x
            y.x <- y
            x <- x[2:(l+1)]
            y <- y[2:(l+1)]
        }else{
            ## add a point before and a point after that is extended in
            ## the same direction as the adjacent point
            x.x <- c( x[1] - (x[2] - x[1]), x, x[l] + (x[l] - x[l-1]) )
            y.x <- c( y[1] - (y[2] - y[1]), y, y[l] + (y[l] - y[l-1]) )
        }

        x.d <- x.x[3:(l+2)] - x.x[1:l]
        y.d <- y.x[3:(l+2)] - y.x[1:l]
        normals <- 1 * 180/pi * acos( x.d / sqrt(x.d^2 + y.d^2) ) ## we need
        normals <- ifelse( y.d < 0, -1 * normals, normals )
    }
    d <-  sqrt( diff(x)^2 + diff(y)^2 )
    def.cex <- par("cex")
    if(is.na(cex)){
        w.ratios <- c(d[1], d) / strwidth( chars )
        cex <- cex.adj * def.cex * min( w.ratios )
    }
    ## Then if we have not defined any normals, then we simply do
    if(!useNormals){
        text( x, y, chars, cex=cex, ... )
    }else{
        ## srt can only be a single value.. 
        for(i in 1:l){
            text( x[i], y[i], chars[i], cex=cex, srt=normals[i], ... )
        }
    }
}

## wraps text using usr coordinates roughly translated to columns
## does not use strwrap in order to work better with non-monospaced
## fonts
## returns the string with newlines added and width and height
## required
usrStrWrap <- function(w, txt, ...){
    txt <- as.character(txt)
    words <- unlist(strsplit(txt, ' |\t'))
    current.line = words[1]
    txt.f <- current.line
    i <- 2
    while(i <= length(words)){
        if( strwidth( paste( current.line, words[i] ), ... ) <= w ){
            current.line <- paste(current.line, words[i])
            txt.f <- paste(txt.f, words[i])
        }else{
            current.line <- words[i]
            txt.f <- paste(txt.f, words[i], sep='\n')
        }
        i <- i + 1
    }
    list('w'=strwidth(txt.f, ...), 'h'=strheight(txt.f, ...), 's'=txt.f)
}

## r is the locations of a bounding box
## left, right, bottom, top (the same as you get from par("usr")
## this function seems to work OK, but is really rather messy.. 
## note that I could probably use strwrap here; but that wraps on
## columns.. 
boxText <- function(r, txt, max.cex=NA, min.cex=NA, margin=0.1, ...){
    r <- as.numeric(r)
    w = r[2] - r[1]
    h = r[4] - r[3]
    ## split the txt into words
    ## we can only handle one set of words to start with.. 
    words <- unlist(strsplit(txt, ' '))
    words.nchar <- nchar(words)
    ## word.widths <- strwidth( words )
    txt.nchar <- nchar(txt)
    char.width <- strwidth( txt ) / txt.nchar ## an approximation
    char.height <- strheight( txt )  ## should be the same for all
    ## work out the ideal number of chars per line on the basis of the dimensions of the box
    ## we intentionally skew this towards allowing a longe text per line than necessary,
    ## since no line is likely to give the exact number.
    line.nchar <- sqrt( (char.height / char.width) * (txt.nchar * w / h) )
    line.n <- floor(sqrt( (h/w) * (char.width/char.height) * txt.nchar ))
    ### UGLY KLUDGE BODGE
    ### to avoid doing something re-iterative; as that is the only way that I can
    ### thing of getting things to work. 
    line.nchar <- ceiling(txt.nchar / line.n) + mean(words.nchar) * line.n / 3

    ## an approximation of the space under the current cex
    line.width <- line.nchar * char.width 
    
    ## for filling the box
    txt.prt <- words[1]
    i <- 2
    while(i <= length(words)){
        current.line <- words[i-1]
        line.length <- nchar( words[i-1] )
        while(i <= length(words) && strwidth(paste(current.line, words[i])) <= line.width ){
#        while( line.length + nchar(words[i]) <= line.nchar && i <= length(words)  ){
            txt.prt <- paste(txt.prt, words[i]) ## sep is a space..
            current.line <- paste(current.line, words[i])
            line.length <- line.length + nchar(words[i]) + 1
            i <- i + 1
        }
        if(i <= length(words)){
            txt.prt <- paste(txt.prt, words[i], sep='\n')
            i <- i + 1
        }
    }
    ## now I should have something reasonable..
    txt.width.r <- (1 + margin) * strwidth(txt.prt) / w
    txt.height.r <- (1 + margin) * strheight(txt.prt) / h
    ## simply scale the cex appropriately
    max.r <- max( c(txt.width.r, txt.height.r) )
    cex <- par("cex") / max.r
    if(!is.na(max.cex)) cex <- min(cex, max.cex)
    if(!is.na(min.cex)) cex <- max(cex, min.cex)
    text( r[1] + w * margin/2, mean(r[3:4]), txt.prt, cex=cex, adj=c(0,0.5), ... )
    invisible(cex)
}


## some ideas as to how to output tables in R..
## there are a whole load of things, but these generally markdown / latex / html
## output, rather than simply drawing a tabe using the plot functions..
## say we would like to plot a table,,
## this is still a bit messy, but it works OK.
## if we wish to have seperate justification for individual columns, then
## we need to draw each column seperately as we cannot specify adj as a list of vectors?
plotTable <- function(x, y, df, c.widths=NULL, num.format=NA,
                      row.margin=1, col.margin=0.1, doPlot=TRUE,
                      row.bg=NA, column.bg=NA, cell.bg=NA, text.col=1,
                      text.adj=c(0,0.5),
                      ...){
    df.m <- as.matrix(df)
    if(length(num.format) > 1 || !is.na(num.format)){
#        num.count <- 0
        for(i in 1:ncol(df)){
            if(is.numeric(df[,i])){
                df.m[,i] <- sprintf(num.format[ 1 + (i-1) %% length(num.format) ], df[,i])
                ##num.count <- num.count + 1
            }
        }
    }
    if(is.null(c.widths)){
        c.widths <- (apply( df.m, 2, function(x){ max( strwidth(x, ...) ) } ))
    }
    ## determine r.heights using the usrStrWrap function
    ## but this cannot wrap the actual text as it uses apply, which cannot
    ## modify the df.m..
    r.heights <- vector(mode='numeric', length=nrow(df.m ))
    for(i in 1:length(r.heights)){
        x.f <- vector(mode='list', length=ncol(df.m))
        for(j in 1:length(x.f))
            x.f[[j]] <- usrStrWrap( c.widths[j], df.m[i,j], ... )
        r.heights[i] <- max( sapply(x.f, function(y){ y$h } ) )
        df.m[i,] <- sapply( x.f, function(y){ y$s })
    }
        
    ## r.heights <- apply( df.m, 1,
    ##                    function(x){
    ##                        x.f <- vector(mode='list', length=length(x))
    ##                        for(i in 1:length(x))
    ##                            x.f[[i]] <- usrStrWrap( c.widths[i], x[i], ... )
    ##                        max( sapply(x.f, function(y){ y$h } ) )
    ##                    })
    
    v.margin <- min( r.heights ) * row.margin/2
    r.heights <- r.heights + (v.margin) * min(r.heights)

    h.margin <- min( c.widths ) * col.margin
    c.widths <- c.widths + h.margin
#    c.widths <- c.widths + mean( c.widths ) * col.margin
### then we know where to place things, starting at the top and using adj=c(0,1)
### 
    y.bot <- y - cumsum(r.heights)
    y.top <- c(y, y.bot[ -length(y.bot) ])
    x.right <- x + cumsum(c.widths)
    x.left <- c(x, x.right[ -length(x.right) ]) 
    
    ## redo these so that we can have a matrix of each positions.
    y.top.m <- matrix( rep(y.top, ncol(df.m)), nrow=length(y.top) )
    y.bot.m <- matrix( rep(y.bot, ncol(df.m)), nrow=length(y.bot) )
    x.left.m <- matrix( rep(x.left + text.adj[1] * (c.widths - h.margin*1.5), nrow(df.m)), ncol=length(x.left), byrow=TRUE )
    c.widths.m <- matrix( rep(c.widths, nrow(df.m)), ncol=length(c.widths), byrow=TRUE )
    
    ## then simply,,
    if(doPlot){
        if(length(row.bg) > 1 || !is.na(row.bg))
            rect( x.left[1], y.bot, rev(x.right)[1], y.top, col=row.bg, border=NA )
        if(length(column.bg) > 1 || !is.na(column.bg))
            rect( x.left, rev(y.bot)[1], x.right, y.top[1], col=column.bg, border=NA )
        if(is.matrix(cell.bg) && nrow(cell.bg) == nrow(df.m) && ncol(cell.bg) == ncol(df.m))
            rect( x.left.m, y.bot.m, x.left.m + c.widths.m, y.bot.m + r.heights, col=cell.bg, border=NA )
        text( x.left.m + h.margin/2, (y.top.m + y.bot.m)/2, df.m, adj=text.adj, col=text.col, ... )
    }
    invisible( list('r'=x.right, 'l'=x.left, 't'=y.top, 'b'=y.bot, 'h.m'=h.margin, 'v.m'=v.margin) )
}


## A rotate function
rotate.pts <- function(pts, a=pi/2, origin=apply(pts, 2, function(x){mean(range(x))}), preserve.aspect=FALSE){
    rot=matrix(c(cos(a), -sin(a), sin(a), cos(a)), nrow=2, byrow=TRUE)
    tfd <-t( t(pts) - origin)
    if(!preserve.aspect){
        tfd <- tfd %*% rot
    }else{
        ## the amount of device space used per x..
        asp <- with(par(), (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4])) )
        tfd[,1] <- tfd[,1] * asp
        tfd <- tfd %*% rot
        tfd[,1] <- tfd[,1] / asp
    }
    tfd <- t( t(tfd) + origin )
    colnames(tfd) <- colnames(pts)
    tfd
}

scale.pts <- function(pts, x.scale, y.scale=x.scale, origin=colMeans(pts)){
    tfd <- t( t(pts) - origin)
    colnames(tfd) <- colnames(pts)
    tfd[,1] <- tfd[,1] * x.scale
    tfd[,2] <- tfd[,2] * y.scale
    t( t(tfd) + origin )
}

translate.pts <- function(pts, x, y){
    t( t(pts) + c(x,y) )
}

### A somewhat prettier arrow than the usual one
### makes an upward pointing arrow
### scale by aspect ?
### by pointing upwards, the arrow width is defined by
### x coordinates, whereas the length is by y-coordinates
arrow.x2.pts <- function(a.l, a.w, ah.l=0.2*a.l, ah.w=3*a.w, p.n=30){
    x1 <- seq(-3, 0, length.out=p.n)
    y1 <- (4 + x1)^2
    y1 <- y1 - min(y1)
    x2 <- seq(-3, min(x1) * a.w/ah.w, length.out=p.n)
    y2 <- ((4+x2)*0.5)^2
    y2 <- y2 - min(y2)

    c.w <- max(abs(x1)) * 2 ## current width
    x1 <- x1 * ah.w / c.w
    x2 <- x2 * ah.w / c.w

    c.h <- max(y1)
    y1 <- y1 * ah.l / c.h
    y2 <- y2 * ah.l / c.h
    
    x <- c(rev(x1), x2, -a.w/2, 0)
    y.min <- max(y1) - a.l
    y <- c(rev(y1), y2, y.min, y.min)
    x <- c(x, -rev(x))
    y <- c(y, rev(y))
    y <- y - min(y)
    cbind('x'=x, 'y'=y)
}

## 
arrow.x2 <- function(x1, y1, x2, y2, a.w, ah.w, ah.l, ah.l.prop=(ah.l < 1), p.n=30,
                     preserve.aspect=TRUE){
    asp <- ifelse( preserve.aspect,
                  with(par(), (pin[1]/pin[2]) / (diff(usr[1:2])/diff(usr[3:4])) ),
                  1)
    l <- sqrt( (asp * (x2-x1))^2 + (y2-y1)^2 )
    ## I suspect that there should be a better way of getting the angle,
    ## but I don't know it... (maybe using complex numbers would do it?)
    a1 <- asin( asp*(x2-x1) / l )
    a2 <- acos( (y2-y1) / l )
    a <- ifelse( a1 > 0, a2, 2 * pi - a2 )
    if(ah.l.prop)
        ah.l <- l * ah.l
    pts <- arrow.x2.pts(l, a.w, ah.l, ah.w, p.n)
    pts[,2] <- pts[,2] - max(pts[,2])
    pts <- rotate.pts(pts, a, origin=c(0, 0), preserve.aspect=preserve.aspect)
    pts <- translate.pts(pts, x2, y2)
    pts
}

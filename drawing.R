## return the points of an arc centered on x, y, from a.beg to a.end and with a radious
## of r
arcPoints <- function( x, y, r, a.beg, a.end, a.n=20, degrees=FALSE){
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
        
    angles <- seq(a.beg, a.end, length.out=a.n)
    a.x <- x + sin( angles ) * r
    a.y <- y + cos( angles ) * r
    pts <- matrix( c(a.x, a.y), ncol=2 )
    colnames(pts) <- c('x', 'y')
    pts
}

## draw an arc, as a line
## centered on x,y, and with radius of r+depth and r-depth..
lineArc <- function(x, y, r, a.beg, a.end, a.sep=NA, degrees=FALSE, label=NULL, label.col=1, ...){
    ## set the default here... 
    if(is.na(a.sep))
        a.sep = 0.01
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
        
    angles <- seq(a.beg, a.end, by=a.sep)
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

## draw an arc, as a polygon..
## centered on x,y, and with radius of r+depth and r-depth..
polygArc <- function(x, y, r, depth, a.beg, a.end, a.sep=NA, degrees=FALSE, label=NULL, label.col=1, ...){
    ## set the default here... 
    if(is.na(a.sep))
        a.sep = 0.01
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
        
    angles <- seq(a.beg, a.end, by=a.sep)
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
              label, srt=rot.a, col=label.col )
    }
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

### so taken together we can do..



gene.pos <- matrix(ncol=2, byrow=TRUE,
                   data=c(10, 90,
                          120, 150,
                          151, 300,
                          301, 320) )
colnames(gene.pos) <- c('beg', 'end')
rownames( gene.pos ) <- c('gene 1', 'gene 2', 'gene 3', 'gene 4') 
genome.size <- 330

measure.x <- sample( 1:genome.size, 42 )
measure.y <- rnorm( length(measure.x) )

## set up a plottin area with no labels or anything else
fig.r <- 100
pdf('polar_plot.pdf', width=7, height=7, title='Polar plot')
plot( 1, type='n', axes=FALSE, xlim=c(-fig.r,fig.r), ylim=c(-fig.r,fig.r), xlab='', ylab='' )

g.r <- 85
g.w <- 10

lineArc(0, 0, g.r, 0, 2*pi, lty=2)
sapply( rownames(gene.pos), function(x){
    b <- 2 * pi * ( gene.pos[x, 'beg'] / genome.size )
    e <- 2 * pi * ( gene.pos[x, 'end'] / genome.size )
    polygArc( 0, 0, g.r, g.w, b, e, label=x, col=rgb(0, 0, 0.8), label.col='white')
})

## and then lets plot the points along an inner axis..
m.r <- 50
m.w <- 20
#lineArc(0, 0, m.r, 0, 2 * pi, lty=2 )
#polygArc( 0, 0, m.r, m.w, 0, 2 * pi )
plotPolar(0, 0, measure.x, measure.y, m.r, m.w, 0, 2*pi, col=hsvScale(1:length(measure.x)), pch=19 )
dev.off()

### we can also do something a bit silly,,,
### and draw pie-charts when we want to.. with offsets pies.. 
plot( 1, type='n', axes=FALSE, xlim=c(-fig.r,fig.r), ylim=c(-fig.r,fig.r), xlab='', ylab='' )
polygArc(0, 0, 25, 50, 0, pi/3, degrees=FALSE, col='red' )
polygArc( 2 * sin( (pi/3 + 2.5*pi/3)/2), 2 * cos( (pi/3 + 2.5*pi/3)/2 ), 25, 50, pi/3, 2.5 * pi/3, degrees=FALSE, col='green' )
polygArc(0, 0, 25, 50, 2.5*pi/3, 1.2 * pi, degrees=FALSE, col='blue' )
polygArc(0, 0, 25, 50, 1.2 * pi, 1.7 * pi, degrees=FALSE, col='grey' )
polygArc(0, 0, 25, 50, 1.7 * pi, 2*pi, degrees=FALSE, col='cyan' )

polygArc(-75, 50, 25, 10, 0, 2*pi, col='blue', border=NA)

## but can we do..
polygArc(-75, 50, 25, 10, -pi/2, pi/2, col='green', border=NA)


## lets draw letters on a set of points. First let us take our string...
pdf("text_along_paths.pdf", width=7, height=7, title='Text along paths')
plot( 0,0, xlim=c(-100, 100), ylim=c(-100, 100), type='n', axes=FALSE, xlab='', ylab='' )
txt <- 'HELLO there stranger'
txt.pts <- arcPoints( 0, 0, 40, 0, 180, a.n=nchar(txt), degrees=TRUE )
pathLetters( txt.pts[,1], txt.pts[,2], txt, cex.adj=0.75  )
txt.pts.2 <- arcPoints( 0, 0, 60, 0, 180, a.n=nchar(txt), degrees=TRUE )
pathLetters( txt.pts.2[,1], txt.pts.2[,2], txt, cex.adj=0.75, useNormals=TRUE )

txt.pts.3 <- arcPoints( 0, 0, 80, 0, 270, a.n=nchar(txt), degrees=TRUE )
pathLetters( txt.pts.3[,1], txt.pts.3[,2], txt, cex.adj=0.75, useNormals=TRUE )

pathLetters( 1:nchar(txt) * 10 - 100, (1:nchar(txt))^2 -100, txt, cex.adj=1, useNormals=TRUE, col='blue' )

x <- seq(-100, 100, length.out=(nchar(txt)+2))
y <- 50 * sin( pi * x/100 )
pathLetters( x, y, txt, useNormals=TRUE, col='red', cex.adj=1 )
pathLetters( x, y, txt, useNormals=TRUE, col='brown', cex=1 )
dev.off()

## some ideas as to how to output tables in R..
## there are a whole load of things, but these generally markdown / latex / html
## output, rather than simply drawing a tabe using the plot functions..

## say we would like to plot a table,,

plotTable <- function(df, c.widths=NULL){
    if(is.null(c.widths)){
        c.widths <- apply( df, 2, function(x){ max( strwidth(x) ) } )
    }
    r.heights <- apply( df, 1, function(x){ max( strheight(x) ) } )
    max.width <- max( c.widths )
    max.height <- max( r.heights )
    w.m <- 0.1 * max.width
    r.m <- 0.1 * max.height
    x.pos <- cumsum( c.widths + w.m )
    y.pos <- cumsum( r.heights + r.m )

    ## then in the simplest possible way..
    text( rep(x.pos, nrow(df)), t( matrix(rep(y.pos, ncol(df)), nrow=nrow(df))), df )
}

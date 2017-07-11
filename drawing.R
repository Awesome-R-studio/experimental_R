## return the points of an arc centered on x, y, from a.beg to a.end and with a radious
## of r
arcPoints <- function( x, y, r, a.beg, a.end, a.n=20, degrees=FALSE){
    if( degrees ){
        a.beg <- pi * a.beg / 180
        a.end <- pi * a.end / 180
    }
    if(a.end < a.beg)
        a.end <- a.end + 2 * pi
        
    angles <- seq(a.beg, a.end, length.out=a.n)
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
polygArc <- function(x, y, r, depth, a.beg, a.end, a.sep=NA, degrees=FALSE, label=NULL, label.col=1, ...){
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
boxText <- function(r, txt, max.cex=NA, min.cex=NA, margin=0.1){
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
    text( r[1] + w * margin/2, mean(r[3:4]), txt.prt, cex=cex, adj=c(0,0.5) )
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
                      row.bg=NA, column.bg=NA, cell.bg=NA,
                      ...){
    df.m <- as.matrix(df)
    if(length(num.format) > 1 || !is.na(num.format)){
        for(i in 1:ncol(df)){
            if(is.numeric(df[,i]))
                df.m[,i] <- sprintf(num.format[ 1 + (i-1) %% length(num.format) ], df[,i])
        }
    }
    if(is.null(c.widths)){
        c.widths <- (apply( df.m, 2, function(x){ max( strwidth(x, ...) ) } ))
    }
    ## determine r.heights using the usrStrWrap function
    r.heights <- apply( df.m, 1,
                       function(x){
                           x.f <- vector(mode='list', length=length(x))
                           for(i in 1:length(x))
                               x.f[[i]] <- usrStrWrap( c.widths[i], x[i], ... )
                           max( sapply(x.f, function(y){ y$h } ) )
                       })
    
    v.margin <- mean( r.heights ) * row.margin/2
    r.heights <- r.heights * (1 + row.margin)

    h.margin <- mean( c.widths ) * col.margin / 2
    c.widths <- c.widths + mean( c.widths ) * col.margin
### then we know where to place things, starting at the top and using adj=c(0,1)
### 
    y.bot <- y - cumsum(r.heights)
    y.top <- c(y, y.bot[ -length(y.bot) ])
    x.right <- x + cumsum(c.widths)
    x.left <- c(x, x.right[ -length(x.right) ])
    
    ## redo these so that we can have a matrix of each positions.
    y.top.m <- matrix( rep(y.top, ncol(df.m)), nrow=length(y.top) )
    y.bot.m <- matrix( rep(y.bot, ncol(df.m)), nrow=length(y.bot) )
    x.left.m <- matrix( rep(x.left, nrow(df.m)), ncol=length(x.left), byrow=TRUE )
    c.widths.m <- matrix( rep(c.widths, nrow(df.m)), ncol=length(c.widths), byrow=TRUE )
    
    ## then simply,,
    if(doPlot){
        if(length(row.bg) > 1 || !is.na(row.bg))
            rect( x.left[1], y.bot, rev(x.right)[1], y.top, col=row.bg, border=NA )
        if(length(column.bg) > 1 || !is.na(column.bg))
            rect( x.left, rev(y.bot)[1], x.right, y.top[1], col=column.bg, border=NA )
        if(is.matrix(cell.bg) && nrow(cell.bg) == nrow(df.m) && ncol(cell.bg) == ncol(df.m))
            rect( x.left.m, y.bot.m, x.left.m + c.widths.m, y.bot.m + r.heights, col=cell.bg, border=NA )
        text( x.left.m + h.margin, y.top.m - v.margin, df.m, adj=c(0,1), ... )
    }
    invisible( list('r'=x.right, 'l'=x.left, 't'=y.top, 'b'=y.bot) )
}

## test the above..
plot(1,1, type='n', xlim=c(-100, 100), ylim=c(-100, 100))

xleft <- -80
xright <- -20
ybot <- -80
ytop <- -50
rect( xleft, ybot, xright, ytop )
boxText( c(xleft, xright, ybot, ytop), 'how is this bla bla going to look then, long enough or and then some more?')

xleft <- -80
xright <- -0
ybot <- 0
ytop <- 45
rect( xleft, ybot, xright, ytop )
boxText( c(xleft, xright, ybot, ytop), "Here is a longer piece of text. This doesn't have anyting funny in it like newlines or anything, but these really will not work properly. If a newline is encountered the function would have to be a bit more clever. Not that much more, but a bit")

## try the string_wrap...
txt <- 'Hello there. This is a piece of text with some stuff in it. I\'m not really sure how it will come out when I format it, but we\'ll soon find out'
txt.f <- usrStrWrap( 75, txt )
txt.f <- usrStrWrap( 75, txt, cex=0.5 )
text(25, 0, txt.f$s, adj=c(0,1), cex=0.5 )
rect( 25, 0-txt.f$h, 100, 0, col='red' )

## we can now try to use the function we have to make a table of sorts ..
## note that this is not smart enough to sort out problems with words that don't fit
## but we can perhaps combine some of our functions for this..
table <- read.table( 'RRBS_annotation_table.csv', sep='\t', stringsAsFactors=FALSE, header=TRUE )
sub.table <- table[ , c('Species', 'Cell_type', 'Generation', 'Feed', 'Reads_all', 'Alignment_rate', 'meanCoverage')]

## the problem is pretty much that the cex parameter to usr coordinates ratio changes depending on
## the plot width. But whatever,,
## we have 61 rows and 7 columns..
col.widths <- c(10, 7.5, 5, 10, 10, 5, 5)
## note that the legality of '%05.2.f' seems to be questionable. Might be undefined behaviour.
num.format <- c(NA, NA, NA, NA, '%.2e', NA, '%05.2f')
plot(1, type='n', xlim=c(0,sum(col.widths)), ylim=c(0,200), axes=FALSE, xlab='', ylab='')
plotTable( 0, 200, sub.table, c.widths=col.widths, num.format=num.format, margin=0.5, cex=0.75 )

o <- with(sub.table, order( Cell_type, Generation, Feed ))
cell.cols <- matrix(rgb(0, 0, 0, 0), nrow=nrow(sub.table), ncol=ncol(sub.table))
cell.cols[,4] <- ifelse( sub.table[o,4] == 'Control', rgb(0, 0.4, 0.4, 0.3), rgb(0, 0, 0, 0) )

pdf('table_example.pdf', width=7, height=14, title='A table from R core functions')
par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(1, type='n', xlim=c(0,sum(col.widths)), ylim=c(0,200), axes=FALSE, xlab='', ylab='')
p.pos <- plotTable( 0, 200, sub.table[o,], num.format=num.format, col.margin=0.75, row.margin=1.2, cex=0.8, family='sans',
#                   column.bg=c(rgb(0.5,0.5,0.5,0.5), rgb(0.9, 0.9, 0.9, 0.5)),
                   row.bg=c(rgb(0.5,0.5,0.5,0.5), rgb(0.9, 0.9, 0.9, 0.5)), cell.bg=cell.cols )
segments( p.pos$l[-1], rev(p.pos$b)[1], p.pos$l[-1], p.pos$t[1], lty=2 )
dev.off()


segments( rep(p.pos$l[1], length(p.pos$t)), p.pos$b, rep(rev(p.pos$r)[1], length(p.pos$t)), p.pos$b )

rect( rep(p.pos$l[1], length(p.pos$t)), p.pos$b, rep(rev(p.pos$r)[1], length(p.pos$t)), p.pos$t, col=c(rgb(0.5,0.5,0.5,0.5), rgb(0,0,0,0)) )
rect( p.pos$l[1], p.pos$b, rev(p.pos$r)[1], p.pos$t, col=c(rgb(0.5,0.5,0.5,0.5), rgb(0,0,0,0)) )

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

plot( 1,1, type='n', xlim=c(-200,200), ylim=c(-200,200), xlab='',  ylab='' )
a <- seq(0, 2*pi, 0.01)
r <- 120
lines( r * cos(a), r * sin(a) )

#a2 <- 1.3*pi
#a1 <- 1.1 * pi # -pi/3 #2*pi
a1 <- -0.1
a2 <- 0.1
lineArc( 0, 0, r, a1, a2, lwd=2, col='green')
connectingArc( 0, 0, r, a1, a2, col='blue' )

a <- seq(0.1, pi, 0.1)
a.col <- hsvScale(a)
for(i in 1:length(a)){
    a1 <- a[i]
    a2 <- -a1
    lineArc( 0, 0, r, a1, a2, lwd=2, col='red')
    connectingArc( 0, 0, r, a1, a2, col=a.col[i], lwd=1 )
}

a <- seq(0.1, 2*pi, 0.1)
a.col <- hsvScale(a)
for(i in 1:length(a)){
    a1 <- a[i]
    a2 <- a1 - 1
    lineArc( 0, 0, r, a1, a2, lwd=2, col='red')
    connectingArc( 0, 0, r, a1, a2, col=a.col[i], lwd=1 )
}

a <- seq(0.1, pi, 0.1)
a.col <- hsvScale(a)
for(i in 1:length(a)){
    a1 <- pi/2 + a[i]
    a2 <- pi/2 - a[i] 
#    lineArc( 0, 0, r, a1, a2, lwd=2, col='red')
    connectingArc( 0, 0, r, a1, a2, col=a.col[i], lwd=1 )
}

lineArc( 0, 0, r, 1.8*pi, 0.2*pi, lwd=3, col='blue' )

plot( 1,1, type='n', xlim=c(-200,200), ylim=c(-200,200), xlab='',  ylab='' )
a <- seq(0, 2*pi, 0.01)
r <- 120
lines( r * cos(a), r * sin(a) )

pts <- arcPoints( 0, 0, 90, 0.3*pi, 0.75*pi, a.n=100 )
pts.distorted <- anchoredDistort( pts, c(0, -100) )
lines( pts[,1], pts[,2], col='red' )
lines( pts.distorted[,1], pts.distorted[,2], col='green', type='b', cex=0.5 )

## a little bit of playing around with circles and connecting paths between different locations.
plot( 1,1, type='n', xlim=c(-100,100), ylim=c(-100,100), xlab='',  ylab='' )
a <- seq(0, 2*pi, by=0.01)
r1 <- 30
a1 <- -0.6*pi
a2 <- 0.6*pi
a1.p <- c( r1 * sin(a1), r1 * cos(a1) )
a2.p <- c( r1 * sin(a2), r1 * cos(a2) )
text(a1.p[1], a1.p[2], 'a1', pos=3)
text(a2.p[1], a2.p[2], 'a2', pos=1)
lines( r1 * sin(a), r1 * cos(a), lwd=1 )
lineArc( 0, 0, r1, a1, a2, lwd=3, col='red' )
lines( c( 0, r1 * sin(a1) ), c(0, r1 * cos(a1)), lty=2 )
lines( c( 0, r1 * sin(a2) ), c(0, r1 * cos(a2)), lty=2 )
lines( c( r1 * sin(a1), r1 * sin(a2) ), c(r1 * cos(a1), r1 * cos(a2)), lty=2 )

#lines( c( 0, 2 * r1 * cos(mean(a[a1:a2])) ), c( 0, 2 * r1 * sin(mean(a[a1:a2]))) )
#a1.p.a2.p.dist <- dist( rbind( a1.p, a2.p ) )[1]
## if we want to join lines at 90 degrees from the points a1.p and a2.p, then we
i.a <- internalAngle(a1, a2)
h2 <- r1 / abs(cos( i.a[1]/2 ))
r2 <- sqrt( h2^2 - r1^2 )
o2 <- c(h2 * sin(i.a[4] ), h2 * cos(i.a[4]) )
lines( c(0, o2[1]), c(0, o2[2]) , col='green', lwd=2 )
lines( c(a1.p[1], o2[1]), c(a1.p[2], o2[2]), col='brown' )
lines( c(a2.p[1], o2[1]), c(a2.p[2], o2[2]), col='brown' )

## to the the startin angle here, we can consider that we have a cirle
## with an origin at o2, and an
## (a1.p - o2)[1] = r2 * cos(b1)
## (a1.p - o2)[2] = r2 * sin(b1)
## in other words, angle b1 is
b1.a = asin( (a1.p - o2)[1] / r2 )
b1.b = acos( (a1.p - o2)[2] / r2 )
b1 <- ifelse( b1.a > 0, b1.b, 2*pi -b1.b )

b2.a = asin( (a2.p - o2)[1] / r2 )
b2.b = acos( (a2.p - o2)[2] / r2 )
b2 <- ifelse( b2.a > 0, b2.b, 2 * pi-b2.b )

lineArc( o2[1], o2[2], r2, b1, b2, lwd=2, col='cyan' )


a1 <- 0.2*pi
a2 <- 0.5*pi
a1.p <- c( r1 * sin(a1), r1 * cos(a1) )
a2.p <- c( r1 * sin(a2), r1 * cos(a2) )

lines( c( 0, r1 * sin(a1) ), c(0, r1 * cos(a1)), lty=2 )
lines( c( 0, r1 * sin(a2) ), c(0, r1 * cos(a2)), lty=2 )
lines( c( r1 * sin(a1), r1 * sin(a2) ), c(r1 * cos(a1), r1 * cos(a2)), lty=2 )

i.a <- internalAngle(a1, a2)
h2 <- r1 / abs(cos( i.a[1]/2 ))
r2 <- sqrt( h2^2 - r1^2 )
o2 <- c(h2 * sin(i.a[4] ), h2 * cos(i.a[4]) )
lines( c(0, o2[1]), c(0, o2[2]) , col='green', lwd=2 )
lines( c(a1.p[1], o2[1]), c(a1.p[2], o2[2]), col='brown' )
lines( c(a2.p[1], o2[1]), c(a2.p[2], o2[2]), col='brown' )

## to the the startin angle here, we can consider that we have a cirle
## with an origin at o2, and an
## (a1.p - o2)[1] = r2 * cos(b1)
## (a1.p - o2)[2] = r2 * sin(b1)
## in other words, angle b1 is
b1.a = asin( (a1.p - o2)[1] / r2 )
b1.b = acos( (a1.p - o2)[2] / r2 )
b1 <- ifelse( b1.a > 0, b1.b, 2*pi -b1.b )

b2.a = asin( clamp((a2.p - o2)[1] / r2) )
b2.b = acos( clamp((a2.p - o2)[2] / r2) )
b2 <- ifelse( b2.a > 0, b2.b, 2 * pi-b2.b )

b.i <- internalAngle(b1, b2)
lineArc( o2[1], o2[2], r2, b.i[2], b.i[3], lwd=2, col='cyan' )
lineArc( o2[1], o2[2], r2, b1, b2, lwd=2, col='cyan' )





r1 <- 30
a1 <- 270
a2 <- 500
a1.p <- c( r1 * sin(a[a1]), r1 * cos(a[a1]) )
a2.p <- c( r1 * sin(a[a2]), r1 * cos(a[a2]) )
lines( r1 * sin(a), r1 * cos(a), lwd=1 )
lines( r1 * sin(a[a1:a2]), r1 * cos(a[a1:a2]), lwd=5, col='blue' )
lines( c( 0, r1 * sin(a[a1]) ), c(0, r1 * cos(a[a1])), lty=2 )
lines( c( 0, r1 * sin(a[a2]) ), c(0, r1 * cos(a[a2])), lty=2 )
lines( c( r1 * sin(a[a1]), r1 * sin(a[a2]) ), c(r1 * cos(a[a1]), r1 * cos(a[a2])), lty=2 )
## if we want to join lines at 90 degrees from the points a1.p and a2.p, then we
h2 <- r1 / abs(cos( (a[a2] - a[a1])/2 ))
r2 <- sqrt( h2^2 - r1^2 )
o2 <- c(h2 * sin((a[a1] + a[a2])/2), h2 * cos((a[a1] + a[a2])/2) )
lines( c(0, o2[1]), c(0, o2[2]) , col='green', lwd=2 )
lines( c(a1.p[1], o2[1]), c(a1.p[2], o2[2]), col='brown' )
lines( c(a2.p[1], o2[1]), c(a2.p[2], o2[2]), col='brown' )

## to the the startin angle here, we can consider that we have a cirle
## with an origin at o2, and an
## (a1.p - o2)[1] = r2 * cos(b1)
## (a1.p - o2)[2] = r2 * sin(b1)
## in other words, angle b1 is
b1.a = asin( (a1.p - o2)[1] / r2 )
b1.b = acos( (a1.p - o2)[2] / r2 )
b1 <- ifelse( b1.a > 0, b1.b, 2*pi -b1.b )

b2.a = asin( (a2.p - o2)[1] / r2 )
b2.b = acos( (a2.p - o2)[2] / r2 )
b2 <- ifelse( b2.a > 0, b2.b, 2*pi -b2.b )

lineArc( o2[1], o2[2], r2, b1, b2, lwd=2, col='red' )

r1 <- 30
a1 <- 540
a2 <- 580
a1.p <- c( r1 * sin(a[a1]), r1 * cos(a[a1]) )
a2.p <- c( r1 * sin(a[a2]), r1 * cos(a[a2]) )
lines( r1 * sin(a), r1 * cos(a), lwd=1 )
lines( r1 * sin(a[a1:a2]), r1 * cos(a[a1:a2]), lwd=5, col='blue' )
lines( c( 0, r1 * sin(a[a1]) ), c(0, r1 * cos(a[a1])), lty=2 )
lines( c( 0, r1 * sin(a[a2]) ), c(0, r1 * cos(a[a2])), lty=2 )
lines( c( r1 * sin(a[a1]), r1 * sin(a[a2]) ), c(r1 * cos(a[a1]), r1 * cos(a[a2])), lty=2 )
## if we want to join lines at 90 degrees from the points a1.p and a2.p, then we
h2 <- r1 / abs(cos( (a[a2] - a[a1])/2 ))
r2 <- sqrt( h2^2 - r1^2 )
o2 <- c(h2 * sin((a[a1] + a[a2])/2), h2 * cos((a[a1] + a[a2])/2) )
lines( c(0, o2[1]), c(0, o2[2]) , col='green', lwd=2 )
lines( c(a1.p[1], o2[1]), c(a1.p[2], o2[2]), col='brown' )
lines( c(a2.p[1], o2[1]), c(a2.p[2], o2[2]), col='brown' )

## to the the startin angle here, we can consider that we have a cirle
## with an origin at o2, and an
## (a1.p - o2)[1] = r2 * cos(b1)
## (a1.p - o2)[2] = r2 * sin(b1)
## in other words, angle b1 is
b1.a = asin( (a1.p - o2)[1] / r2 )
b1.b = acos( (a1.p - o2)[2] / r2 )
b1 <- ifelse( b1.a > 0, b1.b, 2*pi -b1.b )

b2.a = asin( (a2.p - o2)[1] / r2 )
b2.b = acos( (a2.p - o2)[2] / r2 )
b2 <- ifelse( b2.a > 0, b2.b, 2*pi -b2.b )

lineArc( o2[1], o2[2], r2, b1, b2, lwd=2, col='red' )


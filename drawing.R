source("drawing_functions.R")
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
rect( 25, 0-txt.f$h, 100, 0, col=rgb(0.8, 0.5, 0.8, 0.1) )

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

png('table_example.png', width=800, height=2000, title='A table from R core functions')
par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(1, type='n', xlim=c(0,sum(col.widths)), ylim=c(0,200), axes=FALSE, xlab='', ylab='')
p.pos <- plotTable( 0, 200, sub.table[o,], num.format=num.format, col.margin=0.75, row.margin=1.5, cex=1.5, family='sans',
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
pdf("silly_pie_plot.pdf", width=7, height=7, title='Silly pie plots')
plot( 1, type='n', axes=FALSE, xlim=c(-fig.r,fig.r), ylim=c(-fig.r,fig.r), xlab='', ylab='' )
polygArc(0, 0, 25, 50, 0, pi/3, degrees=FALSE, col='red' )
for(i in seq(2, 40, by=4)){
    polygArc( i * sin( (pi/3 + 2.5*pi/3)/2), i * cos( (pi/3 + 2.5*pi/3)/2 ), 25, 50, pi/3, 2.5 * pi/3, degrees=FALSE, col=hsv(0.3, i/40, 0.8) )
}
polygArc(0, 0, 25, 50, 2.5*pi/3, 1.2 * pi, degrees=FALSE, col='blue' )
polygArc(0, 0, 25, 50, 1.2 * pi, 1.7 * pi, degrees=FALSE, col='grey' )
polygArc(0, 0, 25, 50, 1.7 * pi, 2*pi, degrees=FALSE, col='cyan' )
## some segments.. 
polygArc(-75, 50, 25, 10, 0, 2*pi, col='blue', border=NA)
polygArc(-75, 50, 25, 10, -pi/2, pi/2, col='green', border=NA)
dev.off()

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

comp <- c('A'='T', 'C'='G', 'G'='C', 'T'='A')
txt <- sample(c('A', 'C', 'G', 'T'), 100, replace=TRUE )
txt[81:100] <- comp[ txt[20:1] ]
txt <- paste(txt, collapse='')
                   
txt.points <- cbind('x'=1:100, 'y'=rep(10, 100) )

radius = 60 / (2 * pi)
txt.points[21:80,] <- arcPoints( 20.5 + radius, 9.5, (60 / (2 * pi)),  1.5 * pi + (1/60) * 2*pi, 1.5 * pi + (59/60) * 2 * pi, a.n=60 )
txt.points[81:100,'x'] <- rev( txt.points[1:20, 'x'] )
txt.points[81:100,'y'] <- rep( 9, 20 )

plot(txt.points[,'x'], txt.points[,'y'] )

pdf("hairpin.pdf", width=7, height=7, title='Hairpin')
par(mar=c(0,0,0,0))
plot(1, 1, axes=FALSE, type='n', xlab='', ylab='', xlim=c(0,40), ylim=c(-9,30) )
#points(txt.points[,'x'], txt.points[,'y'] )
pathLetters( txt.points[,'x'], txt.points[,'y'], txt, useNormals=TRUE, col='brown', cex.adj=0.9 )
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


pdf('connecting_arcs.pdf', width=7, height=7, title='arcs connecting points on a circle')
plot( 1,1, type='n', xlim=c(-150,150), ylim=c(-150,150), xlab='',  ylab='', axes=FALSE )
a <- seq(0, 2*pi, 0.01)
r <- 140
lines( r * cos(a), r * sin(a) )
a <- seq(0.1, pi, 0.1)
a.col <- hsvScale(a)
for(i in 1:length(a)){
    a1 <- a[i]
    a2 <- -a1
#    lineArc( 0, 0, r, a1, a2, lwd=2, col='red')
    connectingArc( 0, 0, r, a1, a2, col=a.col[i], lwd=1 )
}

a <- seq(0.1, 2*pi, 0.1)
a.col <- hsvScale(a)
for(i in 1:length(a)){
    a1 <- a[i]
    a2 <- a1 - 1
#    lineArc( 0, 0, r, a1, a2, lwd=2, col='red')
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
dev.off()

lineArc( 0, 0, r, 1.8*pi, 0.2*pi, lwd=3, col='blue' )

plot( 1,1, type='n', xlim=c(-200,200), ylim=c(-200,200), xlab='',  ylab='' )
a <- seq(0, 2*pi, 0.01)
r <- 120
lines( r * cos(a), r * sin(a) )

plot(1,1, type='n', xlab='', ylab='', xlim=c(-200, 200), ylim=c(-200, 200))
pts <- arcPoints( 0, 0, 90, 0.3*pi, 0.75*pi, a.n=100 )
pts.distorted <- anchoredDistort( pts, c(-100, -100) )
lines( pts[,1], pts[,2], col='red' )
lines( pts.distorted[,1], pts.distorted[,2], col='green', type='b', cex=0.5 )

plot( pts[,2] - pts.distorted[,2] )

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


## lets experiment with drawing an arrow..
plot(1,1, type='n', xlim=c(-10, 10), ylim=c(-20, 20), xlab='', ylab='' )
grid()
x1 <- seq(-3, 0, length.out=30)
y1 <- (4 + x1)^2
y1 <- y1 - min(y1)
lines(x1, y1)
x2 <- seq(-3, -0.5, length.out=30)
y2 <- ((4+x2)*0.5)^2
y2 <- y2 - min(y2)
lines(x2, y2, col='red')

x <- c(rev(x1), x2, -0.5, -0.5, 0)
y <- c(rev(y1), y2, max(y2), -20, -20)
lines(x, y)

x <- c(x, -rev(x))
y <- c(y, rev(y))
lines(x,y)

polygon(x, y, border=NA, col=rgb(0.5, 0.5, 0))
pts <- cbind(x, y)
pts.2 <- rotate.pts(pts, a=pi)
polygon(pts.2, border=NA, col=rgb(0, 0.5, 0.5))

plot(1,1, type='n', xlim=c(-10, 10), ylim=c(-20, 20), xlab='', ylab='', asp=1 )
grid()
polygon(pts, border=NA, col=rgb(0.5, 0.5, 0))


pts.2 <- rotate.pts(pts, a=pi/2, origin=c(0,-20) )
polygon(pts.2, border=NA, col=rgb(0, 0.5, 0.5))

pts.3 <- scale.points( pts, 0.2 )
polygon(pts.3, border=NA, col='red')

pts.3[,2] <- pts.3[,2] - min(pts.3[,2])
polygon(pts.3, border=NA, col='green')

sapply( seq(0, 2*pi, length.out=30), function(a){
    p <- rotate.pts( pts.3, a, origin=c(0,0) )
    polygon(p, border=NA, col=rgb(a/(2*pi), 0, 1-a/(2*pi)) )
    invisible(1)
})

plot(1,1, type='n', xlim=c(-10, 10), ylim=c(-20, 20), xlab='', ylab='' )
grid()
polygon(pts, border=NA, col=rgb(0.5, 0.5, 0))

polygon(pts.2)
pts.4 <- rotate.pts( pts, a=pi/4)
## pts.2 has lost changed it's dimensions due to the current aspect ratio
## can we fix this by scaling
polygon(pts.4)

## do some simple things
plot(1,1, type='n', xlim=c(-10, 10), ylim=c(-20, 20), xlab='', ylab='' )
grid()

pts.5 <- rbind(c(0,0), c(-5,10))
lines(pts.5)
invisible(sapply(seq(0, 2*pi, length.out=200), function(a){ lines( rotate.pts(pts.5, a=a, origin=c(0,0)) )}))

pts.6 <- lapply(seq(0, 2*pi, length.out=200), function(a){ rotate.pts(pts.5, a=a, origin=c(0,0), preserve.aspect=TRUE) })
invisible(sapply(pts.6, lines, col='blue', lwd=2))

plot(1,1, type='n', xlim=c(-10, 10), ylim=c(-20, 20), xlab='', ylab='' )
grid()
pts.5 <- cbind(c(-5,5,5,-5), c(5,5,-5,-5))
polygon(pts.5)

pts.6 <- rotate.pts(pts.5, a=pi/4, origin=c(0,0))
pts.7 <- rotate.pts(pts.5, a=pi/4, origin=c(0,0), preserve.aspect=TRUE)
polygon(pts.6, border='blue', lwd=2)
polygon(pts.7, border='red', lwd=2)

plot(1,1, type='n', xlim=c(-10, 10), ylim=c(-20, 20), xlab='', ylab='' )
grid()
polygon(pts, col=rgb(0.4, 0, 0.7, 0.5))
#pts.7 <- rotate.pts( pts, a=pi/2, origin=c(0,0), preserve.aspect=TRUE )
pts.7 <- rotate.pts( pts, a=pi/2, preserve.aspect=TRUE )
polygon(pts.7, col=rgb(0, 0.5, 0.7, 0.5))

pts.8 <- scale.points(pts, 0.2)
polygon(pts.8, col=rgb(0.8, 0.5, 0.3), border=NA)
pts.8 <- translate.points(pts.8, -5, -5)
polygon(pts.8, col=rgb(0.8, 0.5, 0.3), border=NA)
invisible( sapply(seq(0, 2*pi, length.out=16), function(a){ polygon( rotate.pts( pts.8, a, preserve.aspect=TRUE ), col='blue') }))


## test out presentation in R...
source("presentation.R")

pdevs <- setup.devices(pdf.name="pres2")

plot.new()
plot.window(xlim=c(0,1024), ylim=c(0,768))
#pts <- translate.pts(pts, 500, 350)
#pts <- scale.pts(pts, 4)
polygon(pts, col=rgb(0.4, 0.5, 0.1), border=NA)
#pts <- translate.pts(pts, -500, -350)
#polygon(pts, col=rgb(0.4, 0.5, 0.1), border=NA)
#pts <- scale.pts(pts, 0.25)
#polygon(pts, col=rgb(0.9, 0.5, 0.1), border=NA)

sapply(seq(0, 2*pi, length.out=36), function(a){
    polygon( rotate.pts(pts, a=a), border=NA, col=rgb(a/(2*pi), 0, 1-(a/(2*pi)))) })

add.page(pdevs)


plot.new()
plot.window(xlim=c(0,1024), ylim=c(0,768))

text(1024/2, 768/2, "Hello World", cex=25)
text(1024/2, 768/3, "Hello World", cex=25, font=3)
text(1024/2, 768/4, "Hello World", cex=20, font=3, family='serif')
text(1024/2, 768*0.75, "Hello World", cex=15, font=1, family='serif')

add.page(pdevs)

pdevs <- restart.devices(pdevs)

finish.pdf(pdevs)


pdevs <- setup.devices(pdf.name="presentation")

plot.new()
plot.window(xlim=c(0,1024), ylim=c(0,768))
polygon(pts)
text( mean(pts[,1]), max(pts[,2]), "The moon", adj=c(0.5, 0), cex=20)
abline(v=mean(pts[,1]), lty=3)

add.page(pdevs)

pts.r <- rotate.pts(pts, a=pi/4, origin=c(mean(pts[,1]), max(pts[,2])) )
polygon(pts.r, col=rgb(0.4, 0.2, 0.8, 0.5))
text( min(pts.r[,1]), min(pts.r[,2]), "Dont't look at finger\nor you miss", adj=c(0.5, 1))

add.page(pdevs)

rect( 800, 200, 1000, 350 )

## after restarting.. R
## my device drivers are no longer active...
## and so I cannot extend the directory anymore...

c <- 1
increment.counter(c)

for(i in 1:10)
    increment.counter(c)


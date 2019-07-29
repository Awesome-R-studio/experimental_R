## This should be a mixture of R and Markdown

```R
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
```

This contains a number of functions. 

1. `arcPoints` Points along an arc
2. `lineArc` draws an arc
3. `polygArc` draws an arc as a polygon
4. `plotPolar` plots coordinates around a circle or arc
5. `pathLetters` draws text on the points specified within an arc
6. `usrStrWrap` wraps text on the basis of user coordinates (`par("usr")`)
7. `boxText` wraps text and resizes it to fit within a specified rectangle
8. `plotTable` plots a dataframe as a table
9. `plotTreeTable` plots tree based information as tables to allow complex
   multi cell entries. Only an idea so far.
   
These functions make use of only base-R to allow the user to visualise data in
novel ways.

![figure 1](polar_plot.pdf "a title for the figure")\

And here are some more things that we can have and which are nice.

![figure 2](text_along_paths.pdf)

## arcPoints
Returns equally spaced points from an arc specified by a center position, a
radius and beg and end angles.

```R
## return the points of an arc centered on x, y, 
## from a.beg to a.end and with a radious of r
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
```
The arguments are:

* `x` The center x coordinate of the arc
* `y` The center y coordinate of the arc
* `r` The radius of the arc
* `a.beg` The first position (angle) of the arc
* `a.end` The end position (angle) of the arc
* `a.n` The number of points
* `degrees` A logical indicating whether the angles have been specified as
  radians (default) or degrees.
  

## lineArc
Draws an arc using lines
```R
lineArc <- function(x, y, r, a.beg, a.end, a.sep=NA, degrees=FALSE,
                    label=NULL, label.col=1, ...){
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
```
The arguments are:

* `x` The center x coordinate of the arc
* `y` The center y coordinate of the arc
* `r` The radius of the arc
* `a.beg` The starting angle of the arc
* `a.end` The end angle of the arc
* `a.n` The number of points (i.e. how smooth the arc is drawn)
* `degrees` A logical indicating whether the angles are in degrees (default
  `FALSE`)
* `label` An optional label to print on the center point of the arc (the color
  of this can be controlled with label.col, other parameters are taken from
  `par()`)
* `label.col` The color of the label
* `...` Additional arguments passed to `lines()` that control the drawing of
  the arc.
  
## polygArc
Draws a filled arc using polygon.
```R
polygArc <- function(x, y, r, depth, a.beg, a.end, a.sep=NA, degrees=FALSE,
                     label=NULL, label.col=1, ...){
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
```

The arguments are:

* `x, y` The x and y coordinates of the center of the arc
* `r` The radius of the arc
* `depth` The width of the arc. The arc will be drawn from `r - depth/2` to
  `r + depth/2`.
* `a.beg, a.end` The beginning and end angles of the arc.
* `a.sep` The angular seperation between points of the arc. This defaults to
  0.01 and should be set to something more reasonable if the arc angles are
  defined in degrees.
* `label` An optional label to draw on the arc segment.
* `label.col` The color of the label.
* `...` Additional arguments passed to `polygon`

## plotPolar
Plots y coordinates along an x-axis specified as an arc.
```R
plotPolar <- function(o.x, o.y, x, y, r, y.depth, a.beg, a.end,
                      degrees=FALSE, drawBorders=TRUE, drawCenter=TRUE, ...){
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
```
The arguments:

* `o.x, o.y` The x and y positions of the center of the arc.
* `x` The x positions to be plotted; these will be transformed into angles
  along the specified arc.
* `y` The y values to be plotted. These will be transformed into radius values
  centered on the radius specified.
* `r` The base radius of the arc.
* `y.depth` The depth of the plot. This value provides the maximum deviation
  from the base radius.
* `a.beg, a.end` The begin and end angles of the arc.
* `degrees` A logical specifying if the angles have been specified as degrees
  (default: `FALSE`).
* `drawBorders` A logical specifying whether to draw the border along the max
  and minimal depth of the plot (default: `TRUE`).
* `drawCenter` A logical specifying whether to draw a line indiciating the
  center of the plot (a line along the base radius).
* `...` Additional arguments passed to the points command specifying how the
  points are plotted.
  
## pathLetters
Draws text along paths specifed as a set of points.
```R
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
```
The arguments are:

* `x,y` The positions at which to draw each letter of the text (including
  spaces). These should either be the same length as `nchar(txt)` or 2 longer
  if normals are to be used.
* `word` The text to written along the path.
* `cex` The character expansion to be used for the letters. Currently only a
  single value can be specified. If `cex` is `NA`, then the function will
  attempt to work out a reasonable `cex` value (as big as possible without
  having overlaps of letters). Currently, only a single `cex` value can be
  specified. Defaults to `NA`.
* `cex.adj` A value used to adjust the size calculated by the function when
  `cex` is set to `NA`. Bigger values give bigger characters. Defaults to
  `0.75`.
* `useNormals` A logical value indicating whether the function should attempt
  to rotate the letters along the path. If `useNormals` is `TRUE`, then the
  letters will be drawn at 90&deg; to a line drawn between the two neighbouring
  points. If the number of points is the same as the number of characters the
  function will interpolate additional points at the beginning and end, if
  there are two more points than characters then the first and last points
  will only be used to calculate reasonable normals.
* `...` Optional arguments passed to `text` used to draw the letters. Note
  that like `cex` you cannot specify vector values, to for example colour
  encode individual characters.
  
## usrStrWrap
This wraps text to fit within a width specified in user coordinates
(`par("usr")`). This can be useful when you want to draw text on plots or to
create more complex tables. This is similar to the built-in function
`strwrap`, but may work better when non-monospaced fonts are used. Note that
the function currently does not break individual words with hyphens, but this would
be fairly trivial to add.

```R
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
```

The arguments are:

* `w` A width specified in user coordinates. This requires that a plotting
  surface has been initialised.
* `txt` The text to be wrapped.
* `...` Optional parameters passed to `strwidth()` and `strheight()` used to
  calculate the amount of space needed for each line.
  
The function returns a list containing the width and height of the bounding
box needed to print the text as well as the text formatted for printing
(i.e. with inserted newlines).

## boxText
Draws the specified text within the specified box after wrapping the text to
fit. The function first formats the text to have approximately the correct
proportions and then adjusts the `cex` factor to fit within the specified
box. 
This function has some horrible bodges within it and could do with a bit
of a rethink as to how it decides how long to make the lines, but it seems to
work, sort of OK at the moment. The internal loops can also be rewritten much
better.

```R
boxText <- function(r, txt, max.cex=NA, min.cex=NA, margin=0.1){
    w = r[2] - r[1]
    h = r[4] - r[3]
    ## split the txt into words
    ## we can only handle one set of words to start with.. 
    words <- unlist(strsplit(txt, ' '))
    words.nchar <- nchar(words)
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
```
The arguments are:

* `r` The location of the box expressed as a vector (left, right, bottom, top)
  of values.
* `txt` The text to be printed.
* `max.cex, min.cex` The max and min `cex` to allow for the
  printing. Currently these are not used.
* `margin` The margin to use, expressed as a proportion of the space required
  for the text.
  
## plotTable
Plots a dataframe as a table on the currently open plotting device.
This seems to work well enough for simple tables where the cell entries have a
reasonably uniform size. For use with more variable data the user may need to
play around with the `c.widths` argument.

```R
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
```
The arguments are:

* `x,y` The left and topmost position of the table.
* `df` A dataframe containing the data to be plotted.
* `c.widths` A vector specifyint the width of each column of the table. If
  `NULL` the function calculates reasonable widths using the `strwidth()`
  function. Defaults to `NULL`.
* `num.format` A vector containing the formatting for numbers; if specified
  this should contain one entry per column. The formatting uses `sprintf()`
  and the formats should be specifed using the format text (eg. `%0.2e`).
* `row.margin` How much space between rows. Defaults to 1.
* `col.margin` How much extra horizontal space. Defaults to 0.1 (10% of the
  average column width).
* `doPlot` A logical indicating whether or not the plot should be
  plotted. This can be used do determine if the plot will fit within the
  available space.
* `row.bg` A vector of colours to be used for the individual rows. This will
  be recycled as needed; hence if two colours are specified the rows will have
  alternating backgrounds which can help to distinguish rows of the table.
* `column.bg` Like `row.bg`, but for columns.
* `cell.bg` A matrix specifying background colors for the individual
  cells. This can be used to highlight specific regions of the plot and if
  used with alpha values it is easy to combine row, column and cell specified
  background colours to make the table easier to read.
  
## plotTreeTable
This is, as of yet, simply an idea. The plotTable function does not allow
sophistications like merged cells and so on. This is because such things are
not easy to specify within a dataframe. However, such structures are often
tree-like (eg. gene, transcript, exons) or gene ontologies. In R, trees are
defined as nested lists. And one could parse such a structure to do something
useful. But since I do not have an urgent need for that I'm simply mentioning
this as a possibility.

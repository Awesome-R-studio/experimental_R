## some functions for drawing genomic data...
## These rely on drawing functions in drawing_functions.R

source("~/R/experimental_R/drawing_functions.R")
source("~/R/experimental_R/general_functions.R")

## orderChromosomesByOrthology
## This orders chromosomes or contigs from two species on the basis of the number
## of homologous sequences between pairs.
## It takes as an input a table of orthologues and their positions as well as
## lists of chromosome sizes within each species.
## it returns the chromosomes in a reasonable order along with
## offsets specifiying suitable plot locations
## there are lots of ways of doing this, so this function will probably end
## up being renamed when I can work out a better naming convention.

## ortho: a dataframe containing the following columns:
## chr.s1, s2.chr, start.s1, start.s1, end.s1, end.s2,
## 
## where s1 and s2 are the identifiers of the species
## s1.chr and s2.chr should be named vectors giving the sizes of the
## chromosomes in each species.
orderByOrthology <- function( ortho, s1.chr, s2.chr, s1.id='s1', s2.id='s2',
                              chr.s1=paste('chr.', s1.id, sep=''),
                              chr.s2=paste('chr.', s2.id, sep=''),
                              stt.s1=paste('start.', s1.id, sep=''),
                              stt.s2=paste('start.', s2.id, sep=''),
                              end.s1=paste('end.', s1.id, sep=''),
                              end.s2=paste('end.', s2.id, sep=''),
                              reverse.s2=TRUE, chr.sep=1e6, consecutive.offsets=TRUE
                             )
{
    s1.s2.sims <- tapply( ortho[,chr.s2], ortho[,chr.s1],
                          function(x){ rle( sort(x) ) })
    ## prepare three vectors which will contain pairwise scoring
    ## these can also be done with judicious use of sapply, rep, tapply
    ## and so on, but I do not find that more readable than to specify the
    ## loop. Note though that the use of c(), is pretty horrible..
    ## we can get set the length from length( unlist(s1.s2.sims)) / 2
    ## but for now we keep this horrible style.. 
    s1.c <- vector(mode='character', length=0)
    s2.c <- vector(mode='character', length=0)
    syn.score <- vector(mode='numeric', length=0)
    
    for(i in 1:length(s1.s2.sims)){
        s1.c <- c(s1.c, rep(names(s1.s2.sims)[i], length( s1.s2.sims[[i]]$values )))
        s2.c <- c(s2.c, s1.s2.sims[[i]]$values)
        syn.score <- c(syn.score, (s1.s2.sims[[i]]$lengths - mean(s1.s2.sims[[i]]$lengths)) / sd( s1.s2.sims[[i]]$lengths ) )
    }

    s1.c <- s1.c[ !is.na(syn.score) ]
    s2.c <- s2.c[ !is.na(syn.score) ]
    syn.score <- syn.score[ !is.na(syn.score) ]
    o <- order(syn.score, decreasing=TRUE)

    ## the chromosome names
    s1.chr.n <- unique( s1.c )
    s2.chr.n <- unique( s2.c )
    ## the chromosomes ordered
    s1.chr.o <- vector(mode='character', length=0)
    s2.chr.o <- vector(mode='character', length=0)
    i <- 1
    while(i <= length(syn.score) && (!all(s1.chr.n %in% s1.chr.o) || !all(s2.chr.n %in% s2.chr.o)) ){
        if(!s1.c[o[i]] %in% s1.chr.o)
            s1.chr.o <- c(s1.chr.o, s1.c[o[i]])
        if(!s2.c[o[i]] %in% s2.chr.o)
            s2.chr.o <- c(s2.chr.o, s2.c[o[i]])
        i <- i + 1
    }
    ## add on any missing chromosomes in the order they appear
    if( length(setdiff(names(s1.chr), s1.chr.o )) > 0 )
        s1.chr.o <- c( s1.chr.o, setdiff( names(s1.chr), s1.chr.o )) 
    if( length(setdiff(names(s2.chr), s2.chr.o)) > 0 )
        s2.chr.o <- c( s2.chr.o, setdiff( names(s2.chr), s2.chr.o))
    ## s1.chr.o is now the order of species one
    ## s2.chr.o and the same for the second species
    if( reverse.s2 )
        s2.chr.o <- rev( s2.chr.o )
    s1.offsets <- rep(0, length( s1.chr.o ))
    s2.offsets <- rep(0, length( s2.chr.o ))

    offset <- 0
    for(i in 1:length(s1.chr.o)){
        s1.offsets[i] <- offset
        offset <- offset + s1.chr[ as.character( s1.chr.o[i] ) ] + chr.sep
    }
    offset <- offset + chr.sep
    if(!consecutive.offsets)
        offset <- 0

    for(i in 1:length(s2.chr.o)){
        s2.offsets[i] <- offset
        offset <- offset + s2.chr[ as.character( s2.chr.o[i] ) ] + chr.sep
    }
    names( s1.offsets ) <- s1.chr.o
    names( s2.offsets ) <- s2.chr.o
    list( 's1.off'=s1.offsets, 's2.off'=s2.offsets, 'l'=offset,
          's1.chr'=s1.chr[names(s1.offsets)], 's2.chr'=s2.chr[names(s2.offsets)])
}


## ortho is a table of orthologies
## chr.o is a a list containing the offsets and order (not used directly)
## 
drawCircularLinks <- function( ortho, chr.o, s1.id='s1', s2.id='s2',
                              radius, o.x=0, o.y=0, a.beg=0, a.end=2*pi,
                              perimeter.col=1, perimeter.lty=1,
                              chr.s1=paste('chr.', s1.id, sep=''),
                              chr.s2=paste('chr.', s2.id, sep=''),
                              stt.s1=paste('start.', s1.id, sep=''),
                              stt.s2=paste('start.', s2.id, sep=''),
                              end.s1=paste('end.', s1.id, sep=''),
                              end.s2=paste('end.', s2.id, sep=''),
                              alpha=0.1,
                              chr.col=hsvScale( as.numeric(as.factor(ortho[,chr.s1])), alpha=alpha ),
                              i.leave=1,
                              ...
                              ){
    lineArc( o.x, o.y, radius, a.beg, a.end, col=perimeter.col, lty=perimeter.lty)
    m <- (a.end - a.beg) / chr.o$l
    for(i in 1:nrow(ortho)){
        if(i %% i.leave == 0){
            if( ortho[i, chr.s1] %in% names( chr.o$s1.off ) ){
                a1 <- m * ( chr.o$s1.off[ ortho[ i, chr.s1 ] ] + ortho[ i, stt.s1 ] )
                a2 <- m * ( chr.o$s2.off[ ortho[ i, chr.s2 ] ] + ortho[ i, stt.s2 ] )
                connectingArc( o.x, o.y, radius, a1, a2, col=chr.col[i], ... )
            }
        }
    }
}

## makes fancy rotated labels
## auto scales labels...
## chr.o, is a list as returned by orderByOrthology
## containing the offsets for s1, s2, the full length,
## and the lengths in named lists (s1.chr, s2.chr)
drawRadialLabels <- function( chr.o, radius, depth, o.x=0, o.y=0, a.beg=0, a.end=2*pi,
                               bg=par('bg'), fg=par('fg'), label.border=par('bg'), min.cex=0.2, rad.cex=0.5, ... ){
    ## units to angles conversion
    m <- (a.end-a.beg) / chr.o$l

    ## and the resulting label widths
    label.offsets <- c( chr.o$s1.off, chr.o$s2.off ) * m
    label.angle.widths <- c( chr.o$s1.chr, chr.o$s2.chr ) * m
    label.widths <- label.angle.widths * radius
    ## we first try to choose a cex based on the depth of the labels
    cex <- par('cex') * (depth * 0.8 / strheight('A'))

    ## then we work out the label that has the smallest amount of size and
    ## see if that fits.. 
    label.text <- c(names( chr.o$s1.chr ), names(chr.o$s2.chr) )
    text.widths <- strwidth( label.text, cex=cex )
    min.width.r <- min(label.widths / text.widths)
    if( min.width.r < 1 )
        cex <- cex * min.width.r

    ## lets then do something to work out what is happening with the colours...
    same.colors <- all(label.border == bg)
    if( length(bg) == 1 ){
        bg <- rep(bg, length(label.offsets))
    }
    if( length(bg) == 2 ){
        bg <- c( rep(bg[1], length(chr.o$s1.off)), rep(bg[2], length(chr.o$s2.off)) )
    }
    ### the following should have some work done on it.. probably best to make a local function.. 
    if(same.colors){
        label.border = bg
    }else{
        label.border = rep(label.border[1], length(label.offsets))
    }
    
    ## now we have a size of the characters, but we do not know what angle to use
    ## for plotting them. It might be best to calculate that for each one as we can
    ## then use the native kerning... hmm.
    ## this turns out to be a little more complicated than expected. But seems to work.
    for(i in 1:length(label.offsets)){
        polygArc( o.x, o.y, radius, depth, label.offsets[i], label.offsets[i] + label.angle.widths[i], col=bg[i], border=label.border[i] )
        if(cex > min.cex){
            chars <- strsplit(label.text[i], '')[[1]]
            char.angles <- 0.5 * strwidth(chars, cex=cex) / radius
            char.angles <- c(0, char.angles[ -length(char.angles) ] + char.angles[-1])
            ## add an additional angle... 
            char.angles <- c( mean(strwidth(chars, cex=cex))/radius, char.angles, mean(strwidth(chars, cex=cex))/radius )
            char.angular.width <- sum(char.angles)
            char.angles <- cumsum( char.angles )
            ## then put into the correct positions.. 
            char.angles <- char.angles + label.offsets[i] + label.angle.widths[i] / 2 - char.angular.width/2
            pathLetters( o.x + radius * sin(char.angles), o.y + radius * cos(char.angles),
                        label.text[i], col=fg, useNormals=TRUE, cex=cex, ... )
        }else{
            a <- label.offsets[i] + label.angle.widths[i]/2
            if( a < pi ){
                srt <- rad2pi(pi/2 - a)
                adj <- c(0, 0.5)
            }else{
                srt <- rad2pi(1.5*pi - a)
                adj <- c(1, 0.5)
            }
            r <- 1.01 * radius + depth/2
            text(o.x + r * sin(a), o.y + r * cos(a), label.text[i], srt=srt, adj=adj, cex=rad.cex)
        }
    }
}

## some functions that should make it easier to create presentations
## in R..

## a set of general drawing functions for text / plots / etc..
source("drawing_functions.R")

## It is probably better to create a new directory and to initially create
## a single file for each slide, as this makes it easier to save the intermediate
## pdfs. One would not normally expect a presentation to be finished in a single
## setting and one would 
setup.devices <- function(screen.m=2, screen.w=1024*screen.m, screen.h=768*screen.m,
                          pdf.name='slide', pdf.title='A presentation made in R',
                          pdf.dpi=72, pdf.w=screen.w/pdf.dpi, pdf.h=screen.h/pdf.dpi,
                          x11.type='cairo', slide.numbering="_%03d"){
    devs <- dev.list()
    x11(type=x11.type, width=screen.w/pdf.dpi, height=screen.h/pdf.dpi)
    x11.dev <- setdiff(dev.list(), devs)
    devs <- dev.list()
    counter <- 1;
    pdf.name <- sub("\\..+$", "", pdf.name) 
    pdf.name <- sub("%\\d+d", "", pdf.name)
    dir.name <- pdf.name
    pdf.name <- sprintf("%s_%03d", pdf.name, counter);
    pdf.name <- paste(pdf.name, slide.numbering, ".pdf", sep="")
    if(!dir.exists(dir.name)){
        if(!dir.create(dir.name))
            stop(paste("Fatal error. Unable to create:", dir.name))
    }
    pdf.name <- paste(dir.name, "/", pdf.name, sep="")
    pdf(pdf.name, width=pdf.w, height=pdf.h, title=pdf.title, onefile=FALSE)
    pdf.dev <- setdiff(dev.list(), devs)
    dev.set(x11.dev)
    ## instead of returning a value we could do use <<- to set a global
    ## variable, but this is not recommended
    list('x'=x11.dev, 'pdf'=pdf.dev, 's.w'=screen.w, 's.h'=screen.h,
      'p.w'=pdf.w, 'p.h'=pdf.h, 'pdf.name'=pdf.name, 'dir'=dir.name, 'p.title'=pdf.title,
      'counter'=counter, 'x11.type'='cairo', 'dpi'=pdf.dpi,
      'slide.numbering'=slide.numbering)
}


## necessary after a save / reload to make new devices
restart.devices <- function(pdevs){
    if(!dir.exists(pdevs$dir))
        stop(paste("Directory", pdevs$dir, "does not exist"))
    counter <- pdevs$counter + 1
    devs <- dev.list()
    x11(type=pdevs$x11.type, width=pdevs$s.w/pdevs$dpi, height=pdevs$s.h/pdevs$dpi)
    x11.dev <- setdiff(dev.list(), devs)
    devs <- dev.list()
    pdf.name <- sub(sprintf("_\\d+(%s.pdf)", pdevs$slide.numbering), 
                    sprintf("_%03d\\1", counter), pdevs$pdf.name )
    pdf(pdf.name, width=pdevs$p.w, height=pdevs$p.h, title=pdevs$p.title, onefile=FALSE)
    pdf.dev <- setdiff(dev.list(), devs)
    dev.set(x11.dev)
    ## instead of returning a value we could do use <<- to set a global
    ## variable, but this is not recommended
    pdevs$x <- x11.dev
    pdevs$pdf <- pdf.dev
    pdevs$counter <- counter
    pdevs$pdf.name <- pdf.name
    pdevs
}


add.page <- function(devs, set.x.dev=FALSE){
    cur <- dev.cur()
    if(set.x.dev)
        dev.set(devs['x'])
    plt <- recordPlot()
    dev.set(devs['pdf'])
    replayPlot(plt)
    dev.set(cur)
}

finish.pdf <- function(devs){
    dev.set( devs['pdf'])
    dev.off()
}

## this may not be useful..
slide.layout <- function(pdevs, mat=matrix(1, nrow=1, ncol=1),
                       widths=rep(1, ncol(mat)), heigths=rep(1, nrow(mat))){
    dev.set(pdevs$pdf)
    layout(mat, widths, heigths)
    dev.set(pdevs$x)
    layout(mat, widths, heigths)
}

#### Functions for handling text
#### The biggest problem with this is that text handling is
#### device dependent. That is we cannot be sure that we get
#### the same margins on different machines. This is problematic
### since at the point when we are experimenting we have not called
### plot.new, etc.. for the things. We can maybe add functions for
### initing plots... 
verb.text <- function(text.lines, left, top, w, h, min.cex, max.cex, pdevs, line.spacing=1.5, margin=0.1, ...){
    ## space for the pdf device..
    line.h <- max( strheight(text.lines, ...)) * line.spacing
    text.w <- max(strwidth(text.lines, ...))
    cex.w <- (1-margin)*w / text.w
    cex.h <- (1-margin)*h / (line.h * length(text.lines))
    cex <- min(c(cex.w, cex.h))
    line.h <- mean( strheight(text.lines, cex=cex, ...)) * line.spacing
    text.w <- max(strwidth(text.lines, cex=cex, ...))
    y <- top - h + ((1:length(text.lines)-1) * line.h)
    y <- y + ( h - line.h * length(text.lines) )/2
    x <- (margin + 1) * left
    text(x, y, rev(text.lines), adj=c(0, 0), cex=cex, ...)
    invisible(list('y'=y, 'x'=x, 'line.h'=line.h, 'text.w'=text.w))
}

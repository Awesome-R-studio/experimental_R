
## use functions here..
source("~/R/experimental_R/genomic_drawing.R")

## For preparing suitable orthologue tables.. 
make.orthologue.table <- function( al.table, genes, suf.1, suf.2 ){
    al <- al.table
    al[,3] <- sub("\\.\\d*$", "", al[,3])
    colnames(al) <- c('species', 'mRNA_id', 'Ens_id', 'rec_rank')
    ortho.t <- merge( al[ al[,'rec_rank'] < 3 & al[,'rec_rank'] > 0, 2:4], genes, by.x=2, by.y='protein_id' )
    ortho.t <- merge( mRNA, ortho.t, by='mRNA_id', suffixes=c(suf.1, suf.2) )
    ortho.t
}


## A function which probably should be moved to genomic_drawing.R
plotCircular <- function( s1, s2, s1.chr, s2.chr, ortho.t, chr.sep=1e7, main='', label.depth=4,
                         sel.chr=NULL, radius=100, label.radius=103, ... ){
    s1.length <- s1.chr[,'length']
    names(s1.length) <- s1.chr[,'name']
    s2.length <- s2.chr[,'length']
    names(s2.length) <- s2.chr[,'name']
    c.o <- orderByOrthology( ortho.t, s1.length, s2.length, s1.id=s1, s2.id=s2, chr.sep=chr.sep )
    plot(1,1, type='n', axes=FALSE, xlim=c(-110, 110), ylim=c(-110, 110), xlab='', ylab='', main=main)
    b <- rep(TRUE, nrow(ortho.t))
    if(!is.null( sel.chr ))
        b <- ortho.t[ ,'chr.gadMor2' ] %in% sel.chr
    drawCircularLinks( ortho.t[b,], c.o, s1.id=s1, s2.id=s2, radius=radius, ... )
    drawRadialLabels( c.o, radius=label.radius, depth=label.depth, bg=c('blue', 'red'), fg='white'  )
}

## The above tables were created using the following sql queries
gene.query <- 'SELECT a.stable_id as gene_id, b.display_label as gene, c.name as chr,
       a.seq_region_start as start, a.seq_region_end as end, a.seq_region_strand as strand,
       d.stable_id as transcript_id, e.stable_id as protein_id from
       gene a
       inner join xref b on a.display_xref_id=b.xref_id
       inner join seq_region c on a.seq_region_id=c.seq_region_id
       inner join coord_system f on c.coord_system_id = f.coord_system_id and f.rank=1
       inner join transcript d on a.gene_id=d.gene_id
       inner join translation e on e.transcript_id=d.transcript_id order by c.name, a.seq_region_start;'

chr.query <- 'SELECT a.* from
       seq_region a
       inner join coord_system b on a.coord_system_id=b.coord_system_id and b.rank=1
       order by a.length desc;'


## mRNA is used by make.orthologue.table
mRNA <- read.table('mRNA.txt', sep="\t", stringsAsFactors=FALSE )

gadMor2.chr <- read.table("gadMor2.lengths", header=FALSE, sep="\t", stringsAsFactors=FALSE)
colnames(gadMor2.chr) <- c('name', 'length')
gadMor2.chr <- cbind( gadMor2.chr, 'off'=(c(0, cumsum(gadMor2.chr[,'length']))[1:nrow(gadMor2.chr)]) )

stickle.chr <- dbGetQuery( stickleback.db, chr.query )
require(RMySQL)
stickleback.db <- dbConnect(MySQL(), user='lmj', dbname='gasterosteus_aculeatus_core_89_1')
stickle.genes <- dbGetQuery( stickleback.db, gene.query )
stickle.chr <- dbGetQuery( stickleback.db, chr.query )

align2.list <- read.table("alignments.txt", header=FALSE, sep="\t", stringsAsFactors=FALSE)
stickle.ortho <- make.orthologue.table( align2.list, stickle.genes, '.gadMor2', '.stickle' )

plotCircular( 'gadMor2', 'stickle', gadMor2.chr, stickle.chr,
             stickle.ortho, main='Gasterosteus aculateus (stickleback)' ) ## amazing!!

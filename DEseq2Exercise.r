#library( DESeq )


quick_scatter <- function( x, y, left, right, title = "plot")
{
  
  plot( log( 1 + x), log( 1 + y), xlim = c( left, right ), ylim = c( left, right ), 
        main = title, pch = ".", cex = 2 )
  lines( c( left, right ), c( left, right), col = "red", lwd = 2 )
}


norm_versus_non_norm <- function( cds, col1, col2, left = 2, right = 8 ) {
  dr = counts( cds, normalized = F )
  dn = counts( cds, normalized = T )
  
  par( mfrow = c( 1,2 ) )
  quick_scatter( dr[,col1], dr[,col2], left, right, title = "raw" )
  quick_scatter( dn[,col1], dn[,col2], left, right, title = "normalized" )
  par( mfrow = c( 1,1 ) )  
}

volcano <- function( res ) { 
   plot( res$log2FoldChange, -log10( res$pval ), xlim = c(-10,10 ), ylim = c(0,20), pch = ".", cex = 2 )
   ssp = res[ res$padj <= 0.01, ]
   points( ssp$log2FoldChange, -log10( ssp$pval ),  pch = ".", cex = 2, col = "red" )
}

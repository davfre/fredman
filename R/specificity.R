#' Methods for expression specificity calculations, based on Schug et al, Genome Biology 2005 (doi:10.1186/gb-2005-6-4-r33)

#' shannon_entropy
#' 
#' numeric vector -> numeric
#'
#' calculate information content in bits on numeric vector
shannonEntropy <- function(p)
{
  if (min(p) < 0 || sum(p) <= 0 || is.na(sum(p))){
    return(NA) 
  }
  p.norm <- p[p>0]/sum(p)
  entropy = -sum(log2(p.norm)*p.norm)
  return(entropy)
}


#' temporal_specificity
#' 
#' numeric vector -> numeric
#' 
#' inverted normalized measure of information content (bits)
#' range 0..1
#' 1 - entropy/log2(N)

temporalSpecificity <- function(expression_vector)
{
    e=shannon.entropy(expression_vector)
    N=length(expression_vector)
    specificity = 1 - e/log2(N)
    return(specificity)
}

#' categorical_specificity
#' 
#' expression vector -> vector categorical specificities for each stage
#' 
#' categorical specificity is a measure of he degree to which a gene's
#' expression pattern is skewed toward expression at each of stages provided
#' Ranges from 0 (high contribution from that tissue) to unbounded (inf, no contribution from that tissue)
#' (Q in Schug et al)

categoricalSpecificity = function(expression_vector){
    s = temporal_specificity(expression_vector)
    categorical_specificity = s - log2(expression_vector/sum(expression_vector))
    names(cat_specificity)=names(expression_matrix)
    return(categorical_specificity)    
}


#' calcSpecificityPerRow
#'
#' numeric matrix -> list(shannon_entropy: vector, temporal_specificity: vector, categorical_specificity: matrix)
#' 
#' convenience function that calculates shannon entropy,
#' gene specificity and categorical specificity across
#' a numeric matrix (e.g. expression values)

calcSpecificityPerRow = function(expression_matrix){
    shannon_entropy=apply(expression_matrix,1,shannonEntropy)
    gene_specificity=apply(expression_matrix,1,temporalSpecificity)
    cat_specificity= apply(expression_matrix,1,categoricalSpecificity)
    return(list(shannon_entropy = shannon_entropy,
                gene_specificity = gene_specificity,
                cat_specificity = cat_specificity))
}
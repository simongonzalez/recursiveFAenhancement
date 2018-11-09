#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Extract carrying words of especified segments
#author Simon Gonzalez - simon.gonzalez@anu.edu.au
#Last date updated: 22 October 2018
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

findWords = function(data = NULL, segNumber = NULL, tierLabel = NULL, wordTier = NULL){
  finBegin = data[[tierLabel]]$t1[segNumber]
  finEnd = data[[tierLabel]]$t2[segNumber]
  
  allInds = which(data[[wordTier]]$t1 <= finBegin)
  
  uniqueIndex = allInds[length(allInds)]
  
  wordLabel = data[[wordTier]]$label[uniqueIndex]
  
  return(wordLabel)
}
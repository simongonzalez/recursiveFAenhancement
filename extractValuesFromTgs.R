#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Recursive forced-alignment implementation

#Data value extraction form MFA outputs for implementing Recursive enhancement and automatic error detection
#If you use this script either partially or totally, please cite:

#Gonzalez, Simon, Catherine E. Travis, James Grama, Danielle Barth and Sunkulp Ananthanarayan. 2018. Recursive forced alignment: A test on a minority language. Paper submitted to: 17th Speech Science and Technology Conference, University of New South Wales, Sydney.

#author Simon Gonzalez - simon.gonzalez@anu.edu.au
#Last date updated: 11 Feb 2019
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


library(rPraat)

source('./functions/findSegments.R')
source('./functions/findTranscription.R')
source('./functions/findWords.R')
source('./functions/get_name_parameter.R')

fls <- list.files('./files', full.names = T)

for(i in fls){
  print('-------------------------------------------')
  print(i)
  
  folderName <- basename(i)
  
  flsi <- list.files(i, full.names = T)
  
  for(tgi in flsi){
    
    filename <- basename(tgi)
    
    tg <- tg.read(tgi)

    tmpSegsLocs <- which(!tg[[2]]$label %in% c('', 'sp', 'sil'))
    tmpSegs <- tg[[2]]$label[tmpSegsLocs]
    tmpSegsPrev <- tg[[2]]$label[tmpSegsLocs-1]
    tmpSegsFoll <- tg[[2]]$label[tmpSegsLocs+1]
    tmpSegsOnset <- tg[[2]]$t1[tmpSegsLocs]
    tmpSegsOffset <- tg[[2]]$t2[tmpSegsLocs]
    tmpSegsDur <- tmpSegsOffset - tmpSegsOnset
    tmpSegMid <- tmpSegsOnset + ((tmpSegsOffset - tmpSegsOnset)/2)
    
    transcriptTierLabel <- names(tg)[1]
    segmentTierLabel <- names(tg)[2]
    
    
    #get words
    wordlabel = NULL
    wordOnset = NULL
    wordOffset = NULL
    wordMid = NULL
    wordDur = NULL
    wordLoc <- NULL
    
    for(j in 1:length(tmpSegsLocs)){
      wordlabel[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = transcriptTierLabel)[[1]]
      wordOnset[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = transcriptTierLabel)[[2]]
      wordOffset[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = transcriptTierLabel)[[3]]
      wordMid[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = transcriptTierLabel)[[4]]
      wordDur[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = transcriptTierLabel)[[5]]
      wordLoc[j] = findWords(data = tg, segNumber = tmpSegsLocs[j], tierLabel = segmentTierLabel, wordTier = transcriptTierLabel)[[6]]
    }
    
    wordLocationFrequency <- as.data.frame(table(wordLoc))
    wordLocationFrequency$wordToken <- 1:nrow(wordLocationFrequency)
    names(wordLocationFrequency) <- c('tgWordLoc', 'phonemeN', 'wordToken')
    
    #get previous words
    prevWord <- tg[[transcriptTierLabel]]$label[wordLoc-1]
    
    #get following words
    follWord <- tg[[transcriptTierLabel]]$label[wordLoc+1]
    
    tmpdf <- data.frame(folder = folderName, file = filename, 
                        previous = tmpSegsPrev, segment = tmpSegs, following = tmpSegsFoll, 
                        onset = tmpSegsOnset, offset = tmpSegsOffset, dur = tmpSegsDur, mid = tmpSegMid, tgLocation = tmpSegsLocs,
                        prevWord, word = wordlabel, follWord,
                        wordOnset, wordOffset, wordDur, wordMid = wordMid, tgWordLoc = wordLoc)
    
    tmpdf <- merge(tmpdf, wordLocationFrequency, merge = tgWordLoc)
    
    tmpdf$token <- 1:nrow(tmpdf)
    
    if(i == fls[1] & tgi == flsi[1]){
      df <- tmpdf
    }else{
      df <- rbind(df, tmpdf)
    }
    
    write.csv(df, 'data.csv', row.names = F)
  }
  
}
#identify the position of the segment in the word

df$position <- ifelse(df$onset == df$wordOnset, 'initial', 'medial')
df[df$offset == df$wordOffset, 'position'] <- 'final'
df[df$onset == df$wordOnset & df$offset == df$wordOffset, 'position'] <- 'both'

df <- df[unlist(strsplit('folder file token previous segment following position onset mid offset dur tgLocation prevWord word follWord wordOnset wordMid wordOffset wordDur phonemeN wordToken tgWordLoc', ' '))]

write.csv(df, 'data.csv', row.names = F)

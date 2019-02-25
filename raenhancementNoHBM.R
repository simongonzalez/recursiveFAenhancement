#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Recursive forced-alignment implementation

#Data value extraction form MFA outputs for implementing Recursive enhancement and automatic error detection
#If you use this script either partially or totally, please cite:

#Gonzalez, Simon, Catherine E. Travis, James Grama, Danielle Barth and Sunkulp Ananthanarayan. 2018. Recursive forced alignment: A test on a minority language. Paper submitted to: 17th Speech Science and Technology Conference, University of New South Wales, Sydney.

#author Simon Gonzalez - simon.gonzalez@anu.edu.au
#Last date updated: 25 Feb 2019
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
    
    maxTime <- as.numeric(attr(tg, "class"))[3]

    tmpSegsLocs <- which(!tg[[2]]$label %in% c('', 'sp', 'sil', 'spn'))
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
    
    tmpdf <- data.frame(folder = folderName, file = filename, fileDuration = maxTime,
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

df <- df[unlist(strsplit('folder file fileDuration token previous segment following position onset mid offset dur tgLocation prevWord word follWord wordOnset wordMid wordOffset wordDur phonemeN wordToken tgWordLoc', ' '))]

write.csv(df, 'data.csv', row.names = F)


#create textGrids

dir.create('./enhancedTGs')


for(i in unique(df$file)){
  tmpFile <- df[df$file == i,]
  tmpFileName <- as.character(unique(tmpFile$file))
  tmpFileDuration <- unique(tmpFile$fileDuration)

  #calculate means per token
  storeDf <- tmpFile[tmpFile$folder == unique(tmpFile$folder)[1],]
  
  #delete any inconsistent label
  tokensToKeep <- NULL
  
  storeDf$onsetMeans <- NULL
  storeDf$onsetSds <- NULL
  storeDf$offsetMeans <- NULL
  storeDf$offsetSds <- NULL
  
  storeDf$onsetMeansWord <- NULL
  storeDf$onsetSdsWord <- NULL
  storeDf$offsetMeansWord <- NULL
  storeDf$offsetSdsWord <- NULL
  
  numberFolderPerToken <- length(unique(tmpFile$folder))
  
  for(j in 1:length(tmpFile$token)){
    if(length(tmpFile[tmpFile$token == j,'segment']) == numberFolderPerToken){
      
      if(length(unique(tmpFile[tmpFile$token == j,'segment'])) == 1){
        tokensToKeep <- append(tokensToKeep, j)
        
        storeDf[storeDf$token == j,'onsetMeans'] <- mean(tmpFile[tmpFile$token == j,'onset'], na.rm = T)
        storeDf[storeDf$token == j,'onsetSds'] <- sd(tmpFile[tmpFile$token == j,'onset'], na.rm = T)
        storeDf[storeDf$token == j,'offsetMeans'] <- mean(tmpFile[tmpFile$token == j,'offset'], na.rm = T)
        storeDf[storeDf$token == j,'offsetSds'] <- sd(tmpFile[tmpFile$token == j,'offset'], na.rm = T)
      }
    }
  }
  
  
  for(j in 1:length(storeDf$wordToken)){
        storeDf[storeDf$wordToken == j,'onsetMeansWord'] <- mean(tmpFile[tmpFile$wordToken == j,'wordOnset'], na.rm = T)
        storeDf[storeDf$wordToken == j,'onsetSdsWord'] <- sd(tmpFile[tmpFile$wordToken == j,'wordOnset'], na.rm = T)
        storeDf[storeDf$wordToken == j,'offsetMeansWord'] <- mean(tmpFile[tmpFile$wordToken == j,'wordOffset'], na.rm = T)
        storeDf[storeDf$wordToken == j,'offsetSdsWord'] <- sd(tmpFile[tmpFile$wordToken == j,'wordOffset'], na.rm = T)
  }
  
  storeDf <- storeDf[storeDf$token %in% tokensToKeep,]
  
  tg <- tg.createNewTextGrid(0, tmpFileDuration)
  tg <- tg.insertNewIntervalTier(tg, 1, "word")
  tg <- tg.insertNewIntervalTier(tg, 2, "segment")
  
  #add word boundaries
  for(j in unique(storeDf$wordToken)){
    startTime <- as.numeric(unique(storeDf[storeDf$wordToken == j,'onsetMeansWord']))
    endTime <- as.numeric(unique(storeDf[storeDf$wordToken == j,'offsetMeansWord']))
    tmpWordLabel <- as.character(unique(storeDf[storeDf$wordToken == j,'word']))
    tg <- tg.insertInterval(tg, "word", startTime, endTime, tmpWordLabel)
  }
  
  #add segment boundaries
  for(j in unique(storeDf$token)){
    startTime <- as.numeric(unique(storeDf[storeDf$token == j,'onsetMeans']))
    endTime <- as.numeric(unique(storeDf[storeDf$token == j,'offsetMeans']))
    tmpWordLabel <- as.character(unique(storeDf[storeDf$token == j,'segment']))
    tg <- tg.insertInterval(tg, "segment", startTime, endTime, tmpWordLabel)
  }
  
  tg.write(tg, paste0('./enhancedTGs/', tmpFileName))
  
}
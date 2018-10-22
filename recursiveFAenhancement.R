#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Recursive forced-alignment implementation

#This script averages boundary locations from output forced-aligned data.
#The purpose is to create a boundary location informed from different data sized fed into a forced-alignment
#If you use this script either partially or totally, please cite:

#Gonzalez, Simon, Catherine E. Travis, James Grama, Danielle Barth and Sunkulp Ananthanarayan. 2018. Recursive forced alignment: A test on a minority language. Paper submitted to: 17th Speech Science and Technology Conference, University of New South Wales, Sydney.

#The structure of this script was as used for the above mentioned paper
#author Simon Gonzalez - simon.gonzalez@anu.edu.au
#Last date updated: 22 October 2018
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

#import libraries
#...............................................................
library(rPraat)
library(dplyr)
library(ggplot2)
library(plotly)

#import functions
#...............................................................
source('findWords.R')

#gets list of the ThextGrid files in the path
fls <- list.files('./comparisonFiles', full.names = T)

#imports values from TextGrids and creates the main dataframe
#...............................................................
#creates an empty dataframe
df <- data.frame(fileName = NULL, speaker = NULL, output = NULL, segment = NULL, startTime = NULL, endTime = NULL, word = NULL)

#creates a type mapping for the segments in the TextGrid
typeMap <- setNames(unlist(strsplit('vowel consonant consonant vowel consonant consonant consonant vowel consonant consonant consonant consonant consonant vowel consonant consonant consonant consonant vowel consonant consonant', ' ')), unlist(strsplit('a b d e f g h i k l m n ng o p r s t u w y', ' ')))

#gets information for each segment interval across all TextGrids
for(i in fls){
  #imports the TextGrid
  tg <- tg.read(i)
  
  #gets location of non-empty intervals
  segLoc <- which(tg[[2]]$label != "")
  #gets all the segments
  segments <- tg[[2]]$label[segLoc]
  
  #gets the beginning times for each interval
  startTime <- tg[[2]]$t1[segLoc]
  #gets the end times for each interval
  endTime <- tg[[2]]$t2[segLoc]
  
  #initiates a vector to store the word level in which each interval appears
  wordlabel = NULL
  
  #populates the word vector from the TextGrid
  for(j in 1:length(segLoc)){
    wordlabel[j] = findWords(data = tg, segNumber = segLoc[j], tierLabel = names(tg)[2], wordTier = names(tg)[1])
  }
  
  #gets the speaker ID form the TextGrid name
  speakerID <- gsub('.TextGrid', '', basename(i))
  #splits the speaker ID and gets the speaker label
  speakerID_split <- unlist(strsplit(speakerID, '_'))
  
  #stores all the information in a temporal dataframe within the iteration
  tmpdf <- data.frame(fileName = speakerID, speaker = speakerID_split[1], output = as.numeric(speakerID_split[2]), segment = segments, startTime = startTime, endTime = endTime, word = wordlabel)
  
  #adds the temporal dataframe to the main dataframe
  df <- rbind(df, tmpdf)
}

#classifies each segment according to type: either consonant or vowel
df$type <- typeMap[as.character(df$segment)]

#sets the time m idpoint for each interval
df$midpoint <- df$startTime + ((df$endTime - df$startTime)/2)

#creates the basis dataframe, i.e. the Human Bechmark (HBM)
#...............................................................
#subsets the maindataframe to the HBM values
dfbasis <- df[df$output == 000,]
#creates an empty colum to store the midpoint values in ms
dfbasis$mid <- NULL
#creates a duration reference value for all segments
dfbasis$dur_ref <- dfbasis$endTime - dfbasis$startTime

#creates the comparison dataframe (CDF)
#...............................................................
dfcomp <- df[df$output != 000,]
#creates an onset column to store the onset mean values
dfcomp$onset <- NULL
#creates an onset column to store the offset mean values
dfcomp$offset <- NULL
#creates an onset column to store the midpoint mean values
dfcomp$mid <- NULL
#creates an onset column to store the maximum duration
dfcomp$dur_max <- NULL
#creates an onset column to store the common duration
dfcomp$common_dur <- NULL
#calculates the automatic duration, i.e. the duration of the forced-align segment
dfcomp$dur_auto <- dfcomp$endTime - dfcomp$startTime

#Creates Recursive means for each boundary
#calculates the components elements to calculate Overlap Rate as well as it calculates mean bounday locations
#...............................................................
for(i in unique(dfcomp$speaker)){
  #subsets the HBM dataframe to the iterated speaker
  tmp_dfBasis <- dfbasis[dfbasis$speaker == i,]
  
  #subsets the CDF to the iterated speaker
  dfi <- dfcomp[dfcomp$speaker == i,]
  #sorst all the output (the data chunks)
  allOutput <- sort(unique(dfcomp$output))
  
  #iterates through each data oputput chunks
  for(j in 1:length(allOutput)){
    
    #subsets the data to the iterated output
    dfj <- dfi[dfi$output == allOutput[j],]
    
    #if the iterated speaker has data in this data output
    if(nrow(dfj) != 0){

      #gets all the start times of all segments from the HBM
      tmp_basis_start <- tmp_dfBasis$startTime
      #gets all the end times of all segments from the HBM
      tmp_basis_end <- tmp_dfBasis$endTime
      #gets all the midpoint times of all segments from the HBM
      tmp_basis_mid <- tmp_dfBasis$midpoint
      
      #gets all the start times of all segments from the CDF
      tmp_comp_start <- dfi[dfi$output == allOutput[j], 'startTime']
      #gets all the end times of all segments from the CDF
      tmp_comp_end <- dfi[dfi$output == allOutput[j], 'endTime']
      #gets all the midpoint times of all segments from the CDF
      tmp_comp_mid <- dfi[dfi$output == allOutput[j], 'midpoint']
      
      #calculates the absolute difference between HBM onset and CDF onset
      dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'onset'] <- abs(tmp_basis_start - tmp_comp_start)
      #calculates the absolute difference between HBM offset and CDF offset
      dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'offset'] <- abs(tmp_basis_end - tmp_comp_end)
      #calculates the absolute difference between HBM midpoint and CDF midpoint
      dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'mid'] <- abs(tmp_basis_mid - tmp_comp_mid)
      
      #calculate mean boundary values across all outputs
      subsetDf <- dfj
      
      if(nrow(subsetDf) != 0){
        #creates an empty dataframe to store all onset values in the iterated output
        insideDf_onset <- as.data.frame(matrix(nrow = nrow(subsetDf), ncol = j))
        #creates an empty dataframe to store all offset values in the iterated output
        insideDf_offset <- as.data.frame(matrix(nrow = nrow(subsetDf), ncol = j))
        #creates an empty dataframe to store all midpoint values in the iterated output
        insideDf_mid <- as.data.frame(matrix(nrow = nrow(subsetDf), ncol = j))
        
        #populates the emoty dataframes with the corresponding values from the iterated speaker dataframe
        for(k in 1:j){
          insideDf_onset[,k] <- dfi[dfi$output == allOutput[k],'startTime']
          insideDf_offset[,k] <- dfi[dfi$output == allOutput[k],'endTime']
          insideDf_mid[,k] <- dfi[dfi$output == allOutput[k],'midpoint']
        }
        
        #averages all start times across data subsets (output)
        dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'startTime'] <- rowMeans(insideDf_onset, na.rm = T)
        #averages all end times across data subsets (output)
        dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'endTime'] <- rowMeans(insideDf_offset, na.rm = T)
        
        #calculates the absolute difference between HBM onset and onset averages
        dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'onset'] <- abs(tmp_basis_start - rowMeans(insideDf_onset, na.rm = T))
        #calculates the absolute difference between HBM onset and offset averages
        dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'offset'] <- abs(tmp_basis_end - rowMeans(insideDf_offset, na.rm = T))
        #calculates the absolute difference between HBM onset and midpoint averages
        dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'mid'] <- abs(tmp_basis_mid - rowMeans(insideDf_mid, na.rm = T))
      }

      #calculates maximum total duration and common duration
      tmp_dur_max <- NULL
      tmp_common_dur <- NULL

      #subsets the comparison dataframe to themiterated speaker and the data output
      df_formula <- dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], ]
      
      #iterates through each of the dataframe with the iterated speaker and data output
      for(k in 1:nrow(dfj)){
        calc_dur_max <- max(c(tmp_dfBasis$endTime[k], df_formula$endTime[k])) - min(c(tmp_dfBasis$startTime[k], df_formula$startTime[k]))
        tmp_dur_max <- append(tmp_dur_max, calc_dur_max)
        
        #calculate common duration
        if(tmp_dfBasis$endTime[k] <= df_formula$startTime[k] | df_formula$endTime[k] <= tmp_dfBasis$startTime[k]){
          tmp_common_dur <- append(tmp_common_dur, 0)
        }else{
          if(tmp_dfBasis$startTime[k] == df_formula$startTime[k]){
            tmp_start_time <- tmp_dfBasis$startTime[k]
          }else{
            tmp_start_time <- max(c(tmp_dfBasis$startTime[k], df_formula$startTime[k]))
          }
          
          if(tmp_dfBasis$endTime[k] == df_formula$endTime[k]){
            tmp_end_time <- tmp_dfBasis$endTime[k]
          }else{
            tmp_end_time <- min(c(tmp_dfBasis$endTime[k], df_formula$endTime[k]))
          }
          
          tmp_common_dur <- append(tmp_common_dur, (tmp_end_time - tmp_start_time))
          
        }
        
      }
      
      #stores the maximum duration
      dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'dur_max'] <- tmp_dur_max
      #stores the common duration
      dfcomp[dfcomp$speaker == i & dfcomp$output == allOutput[j], 'common_dur'] <- tmp_common_dur
      
    }
  }
}

#calculates Overlap Rate based on the common duration and maximum duration
dfcomp$ovR <- dfcomp$common_dur / dfcomp$dur_max


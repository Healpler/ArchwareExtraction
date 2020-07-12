if(!require(tm)){
  install.packages("tm", dependencies = TRUE, INSTALL_opts = '--no-lock')
  library(tm)
}
if(!require(stringr)){
  install.packages("stringr", dependencies = TRUE, INSTALL_opts = '--no-lock')
  library(stringr)
}
if(!require(XML)){
  install.packages("XML", dependencies = TRUE, INSTALL_opts = '--no-lock')
  library(XML)
}
if(!require(methods)){
  install.packages("methods", dependencies = TRUE, INSTALL_opts = '--no-lock')
  library(methods)
}
if(!require(xml2)){
  install.packages("xml2", dependencies = TRUE, INSTALL_opts = '--no-lock')
  library(xml2)
}

howManyKw <- function(){
  lines <- readLines("keywords.txt")
  print(paste(c(length(lines)-3,"keywords"),collapse=" "))
}

extractWords <- function(){
  
  lines <- readLines("keywords.txt")
  #print(lines)
  kw1 <- NA
  kw2 <- NA
  kw3 <- NA
  
  modeF1 <- 0
  modeF2 <- 0
  modeF3 <- 0
  changeMode <- 0
  
  for(r in lines){
    #print(r)
    if(identical(r,"family_1")){
      modeF1 <- 1
      modeF2 <- 0
      modeF3 <- 0
      changeMode <- 1
    }else if(identical(r,"family_2")){
      modeF1 <- 0
      modeF2 <- 1
      modeF3 <- 0
      changeMode <- 1
    }else if(identical(r,"family_3")){
      modeF1 <- 0
      modeF2 <- 0
      modeF3 <- 1
      changeMode <- 1
    }
    if(changeMode == 0){
      #So this is a keyword and not a delimiter
      #In function of mode family 1,2 or 3, add or delete keywords
      #...
      if(modeF1 == 1){
        kw1 <- c(kw1,r)
      }else if(modeF2 == 1){
        kw2 <- c(kw2,r)
      }else if(modeF3 == 1){
        kw3 <- c(kw3,r)
      }
    }else{
      changeMode <- 0
      #And change word (returning to the for loop for changing word)
    }
  }
  kw1 <- kw1[-1]
  kw2 <- kw2[-1]
  kw3 <- kw3[-1]
  
  listKw <- list(kw1,kw2,kw3)
  return(listKw)
}

manageKeywords <- function(mode,word,family){
  if(mode == "delete" || mode == "del"){
    #Extract words already loaded
    listKw <- extractWords()
    keywords1 <- listKw[1]
    keywords2 <- listKw[2]
    keywords3 <- listKw[3]
    writenow <- 0
    #Then delete the word within these three lists.
    if(family == 1){
      index <- 1
      keywords1 <- unlist(keywords1)
      for(k in keywords1){
        if(identical(k,word) && !(identical(word,"family_1")) && !(identical(word,"family_2") && !(identical(word,"family_3")))){
          keywords1 <- keywords1[-index]
        }
        index <- index+1
      }
      writenow <- 1
    }else if(family == 2){
      index <- 1
      keywords2 <- unlist(keywords2)
      for(k in keywords2){
        if(identical(k,word)){
          keywords2 <- keywords2[-index]
        }
        index <- index+1
      }
      writenow <- 1
    }else if(family == 3){
      index <- 1
      keywords3 <- unlist(keywords3)
      for(k in keywords3){
        if(identical(k,word)){
          keywords3 <- keywords3[-index]
        }
        index <- index+1
      }
      writenow <- 1
    }else if(family != 1 && family != 2 && family != 3){
      print("Cannot find this family, unexcepted family.")
    }
    if(writenow == 1){
      writing1 <- paste(c("family_1"),collapse="\r")
      for(k in keywords1){
        writing1 <- paste(c(writing1,k),collapse="\r")
      }
      writing2 <- paste(c("family_2"),collapse="\r")
      for(k in keywords2){
        writing2 <- paste(c(writing2,k),collapse="\r")
      }
      writing3 <- paste(c("family_3"),collapse="\r")
      for(k in keywords3){
        writing3 <- paste(c(writing3,k),collapse="\r")
      }
      #Replacing keywords in the file (WRITING)
      write(paste(c(writing1,writing2,writing3),collapse="\r"),"keywords.txt")
    }
  }else if(mode == "add"){
    #Extract words already loaded
    listKw <- extractWords()
    
    keywords1 <- listKw[1]
    keywords2 <- listKw[2]
    keywords3 <- listKw[3]
    #print(keywords1)
    #print(keywords2)
    #print(keywords3)
    
    writing1 <- NA
    writing2 <- NA
    writing3 <- NA
    processed <- NA
    
    if(family == 1){
      writing1 <- paste(c("family_1",word),collapse="\r")
      for(k in keywords1){
        writing1 <- paste(c(writing1,k),collapse="\r")
      }
      processed <- 1
    }else if(family == 2){
      writing2 <- paste(c("family_2",word),collapse="\r")
      for(k in keywords2){
        writing2 <- paste(c(writing2,k),collapse="\r")
      }
      processed <- 2
    }else if(family == 3){
      writing3 <- paste(c("family_3",word),collapse="\r")
      for(k in keywords3){
        writing3 <- paste(c(writing3,k),collapse="\r")
      }
      processed <- 3
    }else if(family != 1 && family != 2 && family != 3){
      print("Cannot find this family, unexcepted family.")
    }
    
    #writing into the text file named keywords.txt
    if(processed == 1){
      writing2 <- paste(c("family_2"),collapse="\r")
      for(k in keywords2){
        writing2 <- paste(c(writing2,k),collapse="\r")
      }
      writing3 <- paste(c("family_3"),collapse="\r")
      for(k in keywords3){
        writing3 <- paste(c(writing3,k),collapse="\r")
      }
    }else if(processed == 2){
      writing1 <- paste(c("family_1"),collapse="\r")
      for(k in keywords1){
        writing1 <- paste(c(writing1,k),collapse="\r")
      }
      writing3 <- paste(c("family_3"),collapse="\r")
      for(k in keywords3){
        writing3 <- paste(c(writing3,k),collapse="\r")
      }
    }else if(processed == 3){
      writing1 <- paste(c("family_1"),collapse="\r")
      for(k in keywords1){
        writing1 <- paste(c(writing1,k),collapse="\r")
      }
      writing2 <- paste(c("family_2"),collapse="\r")
      for(k in keywords2){
        writing2 <- paste(c(writing2,k),collapse="\r")
      }
    }
    write(paste(c(writing1,writing2,writing3),collapse="\r"),"keywords.txt")
  }
}

#manageKeywords("D:/Documents/ARCHWARE/ArchwareExtraction","add","mot7",3)
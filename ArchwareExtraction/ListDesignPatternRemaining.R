#Ajusting working path
#setwd("D:/Documents/ARCHWARE/R")

#Ajusting packages
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

extractInDataFrame <- function(pathWorkDirectory, ids, writable){
  #Go on work directory
  setwd(pathWorkDirectory)
  
  #Download XML file
  if(!file.exists("capec.xml")){
    print("Loading URL...")
    url <- "https://capec.mitre.org/data/xml/views/1000.xml.zip"
    zipFile <- "capec.zip"
    print("Downloading XML from CAPEC ...")
    download.file(url,zipFile,mode="wb")
    print(paste(c("Unzip XML file in : ",getwd())))
    unzip(zipFile)
    file.rename("1000.xml","capec.xml")
  }
  
  #Read the XMLDocument to a vector
  x <- read_xml("capec.xml")
  
  #Will display : VIEW LIST: CAPEC-1000: Mechanisms of Attack"
  xml_attr(x,"Name")
  #kid is <Attack Patterns>
  kid <- xml_children(x)
  #kids are <Attack Pattern> in <Attack Patterns>
  kids <- xml_children(kid)
  #kids
  xml_attr(kids,"Name")
  
  titles <- ""
  capec_id <- ""
  #Get name and ID, match and store them into a text file
  for(i in 1:length(kids)){
    testGraph <- xml_attr(kids[i],"Type")
    if(is.na(testGraph)){
      for(id in ids){
        if(identical(id,xml_attr(kids[i],"ID"))){
          n <- xml_attr(kids[i],"Name")
          id <- xml_attr(kids[i],"ID")
          if(!is.na(n)){
            titles <- c(titles,n)
            capec_id <- c(capec_id,id)
            if(writable == 1){
              res <- paste(c(n,"_",id),collapse="")
              write(res,file="ListUnextractedAttacks.txt",append=TRUE)
            }
          }
        }
      }
    }
  }
  titles <- titles[-1]
  capec_id <- capec_id[-1]
  df <- data.frame(TITLES = titles, CAPEC_ID = capec_id)
  
  return(df)
}

extractIDfromXML <- function(pathWorkDirectory){
  #Go on work directory
  setwd(pathWorkDirectory)
  
  #Download XML file from CAPEC (if file doesn't exist yet)
  if(!file.exists("capec.xml")){
    print("Loading URL...")
    url <- "https://capec.mitre.org/data/xml/views/1000.xml.zip"
    zipFile <- "capec.zip"
    print("Downloading XML from CAPEC ...")
    download.file(url,zipFile,mode="wb")
    print(paste(c("Unzip XML file in : ",getwd())))
    unzip(zipFile)
    file.rename("1000.xml","capec.xml")
  }
  
  #Read the XMLDocument to a vector
  x <- read_xml("capec.xml")
  
  #Will display : VIEW LIST: CAPEC-1000: Mechanisms of Attack"
  xml_attr(x,"Name")
  #kid is <Attack Patterns>
  kid <- xml_children(x)
  #kids are <Attack Pattern> in <Attack Patterns>
  kids <- xml_children(kid)
  #kids
  xml_attr(kids,"Name")
  
  ids <- ""
  #Get ID's, match and store them into dataframe
  for(i in 1:length(kids)){
    testGraph <- xml_attr(kids[i],"Type")
    if(is.na(testGraph)){
      n <- xml_attr(kids[i],"Name")
      id <- xml_attr(kids[i],"ID")
      if(!is.na(id)){
        ids <- c(ids,id)
      }
    }
  }
  ids <- ids[-1]
  df <- data.frame(CAPEC_ID = ids)
  return(df)
}
########################################################################################
#The purpose of this script is to list all attack patterns which are not yet concerned by the extraction process
########################################################################################

listAttackRemaining <- function(pathworkdirectory){
  print("Please waiting... Loading extraction list...")
  source("extractVAprocess.R")
  extractionResults <- as.data.frame(extractVA(pathworkdirectory,3))
  idExtracted <- extractionResults$CAPEC_ID
  AllIds <- extractIDfromXML(getwd())
  #AllIds$CAPEC_ID
  AllIdsVector <- AllIds$CAPEC_ID
  
  result <- ""
  for(w1 in AllIdsVector){
    test <- 0
    for(w2 in idExtracted){
      if(identical(w2,w1)){
        test <- 1
      }
    }
    if(test == 0){
      #Pattern found and added
      result <- c(result,w1)
    }
  }
  
  result <- result[-1]
  print("Please waiting... Searching patterns not yet in the extraction list...")
  extractInDataFrame(getwd(),result,1)
  print("Done.")
  print("A list is available into the file : ListUnextractedAttacks.txt")
}

#listAttackRemaining("D:/Documents/ARCHWARE/R")

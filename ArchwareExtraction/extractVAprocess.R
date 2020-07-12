#Ajusting working path

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
source(paste(c(extractPath,"manageKeywords.R"),collapse=""),local=TRUE)
##############INITIALIZATION FUNCTIONS AND VARIABLES###############
###################################################################
##################FUNCTION SEARCHING LINE##########################
#Function which returns VA with specifying an attack pattern ID and a dataframe of VA already extracted
searchVAByID <- function(pathWorkDirectory,id,df_va){
  #Go on work directory
  setwd(pathWorkDirectory)
  newDF <- data.frame(CAPEC_ID = df_va$CAPEC_ID, VULNERABILITY_ASSET = df_va$VULNERABILITY_ASSET)
  vas <- subset(newDF, CAPEC_ID == id,warn=F)
  vas <- data.frame(VULNERABILITY_ASSET = vas$VULNERABILITY_ASSET)
  return(vas)
}

#Function which searchs an attack pattern title thanks a key word (manipulate for instance)
searchTitles4VA <- function(pathWorkDirectory,keyword,textfile){
  #Go on work directory
  setwd(pathWorkDirectory)
  
  #Creating results VA file if not exist
  if(!file.exists("resultsAtck_PattrnForVA.txt")){
    extractPrimary(pathWorkDirectory)
  }
  textfile <- "resultsAtck_PattrnForVA.txt"
  
  lines <- readLines(textfile)
  res <- ""
  for(r in lines){
    #cleaning and search the keyword
    nr <- gsub(pattern="\\d|_",replace="",r)
    nr <- str_split(nr,"\\s")
    #For each words in the cleaned tense
    for(w in nr){
      #print(w)
      if(keyword %in% w){
        #the keyword has been found.
        res<- c(res,r)
      }
    }
  }
  res <- res[-1]
  return(res)
}

#Function which searchs an attack pattern title thanks a key tense (manipulate for instance)
searchTitles4VAComposedWords <- function(pathWorkDirectory,keytense){
  #Go on work directory
  setwd(pathWorkDirectory)
  
  #Creating results VA file if not exist
  if(!file.exists("resultsAtck_PattrnForVA.txt")){
    extractPrimary(pathWorkDirectory)
  }
  textfile <- "resultsAtck_PattrnForVA.txt"
  lines <- readLines(textfile)
  res <- ""
  for(r in lines){
    #cleaning and search the keyword
    nr <- gsub(pattern="\\d|_",replace="",r)
    if(!is.na(str_extract(nr,keytense))){
      #the keytense has been found.
      res <- c(res,r)
    }
  }
  res <- res[-1]
  return(res)
}
###################################################################
#test <- searchTitles4VA("D:/Documents/ARCHWARE/R","Injection")
#test 
#testComposed <- searchTitles4VAComposedWords("D:/Documents/ARCHWARE/R","Spoofing of","resultats_xml.txt")
#testComposed

###################################################################
##################FUNCTION EXTRACT TITLES AND IDs##################
#Function which writes into an output file all titles and capec id of attackpatterns
#from an XML file.
extractPrimary <- function(pathWorkDirectory){
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
  
  #Creating result text file if not exist
  if(!file.exists("resultsAtck_PattrnForVA.txt")){
    file.create("resultsAtck_PattrnForVA.txt")
  }
  resultFile <- "resultsAtck_PattrnForVA.txt"
  #Delete the old content before add new content into our file
  my_file <- paste(readLines(resultFile),collapse="")
  if(my_file != ""){
    write("",file=resultFile)
  }
  
  #Get name and ID, match and store them into a text file
  for(i in 1:length(kids)){
    testGraph <- xml_attr(kids[i],"Type")
    if(is.na(testGraph)){
      n <- xml_attr(kids[i],"Name")
      id <- xml_attr(kids[i],"ID")
      res <- paste(c(n,"_",id),collapse="")
      if(!is.na(n)){
        write(res,file=resultFile,append=TRUE)
        #print(res)
      }
    }
  }
}
###################################################################
#extractPrimary("D:/Documents/ARCHWARE/R","resultats_xml.txt")



#########################IMPORTANT PROCESS#########################
###################################################################
#Découper les mots simple et comparer avec cette liste de mots clés. (VA + mot clé)
extractKeyword1 <- function(){
  listKw <- extractWords()
  return(unlist(listKw[1]))
}
#Ne pas découper le mots composés pour extractKeyword2, seulement localiser en text mining. (mot clé + VA)
extractKeyword2 <- function(){
  listKw <- extractWords()
  return(unlist(listKw[2]))
}
#Découper les mots simple et comparer avec cette liste de mots clés. (mot clé + VA)
extractKeyword3 <- function(){
  listKw <- extractWords()
  return(unlist(listKw[3]))
}
###################################################################
extractVA <- function(pathWorkDirectory,writeCSV){
  #Go on work directory
  setwd(pathWorkDirectory)
  
  #Initialize keywords
  kw1 <- extractKeyword1()
  kw2 <- extractKeyword2()
  kw3 <- extractKeyword3()
  
  #Initialize results attack design titles
  extractPrimary(pathWorkDirectory)
  
  #Initialize dataframes elements
  KEYWORD <- NULL
  VULNERABILITY_ASSET <- NULL
  CAPEC_ID <- NULL
  
  print("Extraction began, please wait ...")
  #Extraction process
  for(kw in kw3){
    part3 <- searchTitles4VA(pathWorkDirectory,kw)
    #Cleaning attack design title
    for(ws in part3){
      #Get the ID
      id <- str_extract(ws,"\\d+")
      ws <- gsub("_|\\d","",ws)
      boolean <- 0
      #Remove all before kewords (and keyword) of part3 (kw + VA so everything before kw is useless)
      pattern3 <- paste(c("(.*)",kw),collapse="")
      
      #Code anti-duplication (particular case : Bypassing and Bypassing of)
      if(identical("Bypassing",kw)){
        patternDupli <- paste(c(kw,"\\s","of"),collapse="")
        testDupli <- str_extract(ws,patternDupli)
        if(identical("Bypassing of",testDupli)){
          #duplicata spotted
          boolean <- 1
        }
      }
      if(boolean == 0){
        va <- gsub(pattern3,"",ws)
        va <- gsub("^(\\s)|($\\s)","",va)
        #####
        #if(identical(kw,"Overflow") && identical(id,"100")){
          #print(c("YES",va,id))
        #}
        #####
        #Append data to columns
        if(!identical(va,"")){
          KEYWORD <- c(KEYWORD,kw)
          VULNERABILITY_ASSET <- c(VULNERABILITY_ASSET,va)
          CAPEC_ID <- c(CAPEC_ID,id)
        }
      }
    }
  }
  
  for(kw in kw1){
    part1 <- searchTitles4VA(pathWorkDirectory,kw)
    #Cleaning attack design title
    for(ws in part1){
      #Get the ID
      id <- str_extract(ws,"\\d+")
      ws <- gsub("_|\\d","",ws)
      
      #Remove all after kewords (and keyword) of part1 (VA + kw so everything after kw is useless)
      pattern1 <- paste(c(kw,"(.*)"),collapse="")
      va <- gsub(pattern1,"",ws)
      
      #Remove useless words and spaces
      #va <- gsub("$via|$to","",ws)
      
      #Particular cases
      if(identical("Injection",kw) || identical("Insertion",kw) || identical("Inclusion",kw)){
        #Remove useless "malicious" word
        va <- gsub("[M|m]alicious","",va)
        va <- paste(c("Untested",va,"Input"),collapse=" ")
        va <- gsub("\\s\\s+"," ",va)
      }

      va <- gsub("^(\\s)|$\\s","",va)
      #Append data to columns
      if(!identical(va,"")){
        KEYWORD <- c(KEYWORD,kw)
        VULNERABILITY_ASSET <- c(VULNERABILITY_ASSET,va)
        CAPEC_ID <- c(CAPEC_ID,id)
      }
    }
  }
  
  #Composed word (keyword + VA)
  for(kw in kw2){
    part2 <- searchTitles4VAComposedWords(pathWorkDirectory,kw)
    #Cleaning attack design title
    for(ws in part2){
      #Get the ID
      id <- str_extract(ws,"\\d+")
      ws <- gsub("_|\\d","",ws)
      
      #Remove all before kewords (and keyword) of part1 (kw + VA so everything before kw is useless)
      pattern2 <- paste(c("(.*)",kw,"\\s"),collapse="")
      va <- gsub(pattern2,"",ws)
      #Remove useless spaces
      va <- gsub("^(\\s)|($\\s)","",va)

      #Append data to columns
      if(!identical(va,"")){
        KEYWORD <- c(KEYWORD,kw)
        VULNERABILITY_ASSET <- c(VULNERABILITY_ASSET,va)
        CAPEC_ID <- c(CAPEC_ID,id)
      }
    }
  }
  
  #Write into a CSV file
  df <- data.frame(KEYWORD,VULNERABILITY_ASSET,CAPEC_ID,stringsAsFactors = FALSE,check.rows=FALSE)
  csvfile <- "ExtractVA.csv"
  if(writeCSV == 1){
    print(paste(c("Writing into a CSV file called",csvfile)))
    if(!file.exists("ExtractVA.csv")){
      file.create("ExtractVA.csv")
    }
    write.table(df,csvfile,row.names=FALSE,sep=";")
  }else if(writeCSV == 0){
    print(df)
  }else if(writeCSV == 2){
    print(paste(c("Writing into a CSV file called",csvfile),collapse = " "))
    if(!file.exists("ExtractVA.csv")){
      file.create("ExtractVA.csv")
    }
    csvfile <- "ExtractVA.csv"
    write.table(df,csvfile,row.names=FALSE,sep=";")
    print(df)
  }else if(writeCSV == 3){
    print("Returning a dataframe...")
  }else if(writecsv != 0 || writecsv != 1 || writecsv != 2){
    print("Error : Wrong parameter writeCSV.")
    print("parameter writeCSV : 0 value display only the results on the console.")
    print("parameter writeCSV : 1 value only write the results into a CSV file.")
    print("parameter writeCSV : 2 value does both actions, display the results on screen and write into a CSV file.")
    print("parameter writeCSV : 3 value only return the dataframe (dont use this value usually)")
  }
  return(df)
}

#extractVA("D:/Documents/ARCHWARE/R",2)
#########################IMPORTANT PROCESS#########################
##############################END##################################
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

source(paste(c(extractPath,"extractVAprocess.R"),collapse=""),local=TRUE)

#Découper les mots simple et comparer avec cette liste de mots clés. (VA + mot clé)
extractKeyword1 <- function(){
  res <- c("Spoofing","Phishing","Hijacking","Overlay","Squatting","Monitoring","Flood","Splitting","Smuggling",
           "Tampering","Bypass","Abuse","Overflow","Poisoning","Disabling","Seizure","Jamming","Blocking","Alteration","Analysis",
           "Impersonation", "Manipulation", "Expansion", "Linking", "Blowup", "Fragmentation", "Misuse", "Exploitation", "Altered",
           "Injection", "Pollution", "Inclusion", "Insertion", "Scanning", "Discovery", "Footprinting", "Fingerprinting", "Probe")
  return(res)
}
#Ne pas découper le mots composés pour extractKeyword2, seulement localiser en text mining. (mot clé + VA)
extractKeyword2 <- function(){
  res <- c("Spoofing of ","Exploiting Incorrectly","Modification of","Collect Data from","Pretexting via","Bypassing of ")
  return(res)
}
#Découper les mots simple et comparer avec cette liste de mots clés. (mot clé + VA)
extractKeyword3 <- function(){
  res <- c("Manipulate","Leveraging","Manipulating","Disabling","Accessing","Intercepting","Modifying","Counterfeit",
           "Fake the", "Exploit","Using", "Leverage", "Poison", "Infected", "Contaminate", "Detect","Probe", "Capture", "Sniffing")
  return(res)
}
##################################

recursiveVAextraction <- function(design_pattern,res){
  #print(paste(c("res saving : ",res),collapse=" "))
  #Get the ID and delete useless parts
  ws <- design_pattern
  #Spliting words atoms
  title <- design_pattern
  splited <- unlist(strsplit(ws,"\\s"))
  
  #Initialize keywords
  kw1 <- extractKeyword1()
  kw2 <- extractKeyword2()
  kw3 <- extractKeyword3()
  
  alreadyFound <- 0
  alreadyProcessed <- 0
  findAWord <- 0
  va <- ""
  if(alreadyProcessed == 0){
    #print("Family 1 !")
    for(kw in kw1){
      if(alreadyFound == 0){
        for(w in splited){
          if(identical(kw,w)){
            #KW1 processing launched
            #va + kw
            alreadyFound <- 1
            ws <- paste(splited,collapse=" ")
            #print("find !")
            #Remove all after kewords (and keyword) of part1 (VA + kw so everything after kw is useless)
            pattern1 <- paste(c(w,"(.*)"),collapse="")
            va <- gsub(pattern1,"",ws)
            #print(paste(c(va,w)),collapse="-")
            
            #Particular cases
            if(identical("Injection",w) || identical("Insertion",w) || identical("Inclusion",w)){
              #Remove useless "malicious" word
              va <- gsub("[M|m]alicious","",va)
              va <- paste(c("Untested",va,"Input"),collapse=" ")
              va <- gsub("\\s\\s+"," ",va)
            }
            #Remove useless borderlines spaces
            va <- gsub("^(\\s)|$\\s","",va)
            findAWord <- 1
            alreadyProcessed <- 1
          }
        }
      }
    }
    #Reinitialize for-break
    alreadyFound <- 0
  }
  #print(paste(c("res saving prime: ",res),collapse=" "))
  if(alreadyProcessed == 0){
    #print("Family 2 !")
    for(kw in kw2){
      if(alreadyFound == 0){
        found <- str_extract(title,kw)
        #print(found)
        if(!is.na(found)){
          #KW2 processing launched
          #kw + va
          alreadyFound <- 1
          ws <- title
          ws <- gsub("_|\\d","",ws)
          
          #Remove all before kewords (and keyword) of part1 (kw + VA so everything before kw is useless)
          pattern2 <- paste(c("(.*)",kw),collapse="")
          #print(pattern2)
          va <- gsub(pattern2,"",ws)
          #Remove useless spaces
          va <- gsub("^(\\s)|($\\s)","",va)
          #va <- paste(c(kw,va,id),collapse="_")
          findAWord <- 1
          alreadyProcessed <- 1
        }
        
      }
    }
    alreadyFound <- 0
  }
  #print(paste(c("res saving prime : ",res),collapse=" "))
  if(alreadyProcessed == 0){
    #print("Family 3 !")
    for(kw in kw3){
      if(alreadyFound == 0){
        for(w in splited){
          #print(c(w,kw))
          if(identical(w,kw)){
            #KW3 processing launched
            #kw + va
            alreadyFound <- 1
            ws <- paste(splited,collapse=" ")
            #print(c("ws :",ws))
            ws <- gsub("_|\\d","",ws)
            boolean <- 0
            #Remove all before kewords (and keyword) of part3 (kw + VA so everything before kw is useless)
            pattern3 <- paste(c("(.*)",kw),collapse="")
            #Write here particular case processes thanks boolean
            #...
        
            if(boolean == 0){
              va <- gsub(pattern3,"",ws)
              va <- gsub("^(\\s)|($\\s)","",va)
            }
            findAWord <- 1
            alreadyProcessed <- 1
          }
        }
      }
    }
    alreadyFound <- 0
  }
  
  if(findAWord == 1 && !(identical(va,""))){
    #So check if we can find a new word again
    #print(paste(c("result : ",va),collapse=" "))
    #print(res)
    #if res vide alors on concatene sinon non
    res <- c(res,va)
    res <- recursiveVAextraction(va,res)
  }else{
    res <- res[-1]
  }
  return(res)
}

recursiveVA <- function(pathWorkDirectory,writeCSV){
  #Initialize results attack design titles
  extractPrimary(pathWorkDirectory)
  
  #Initialize keywords
  kw1 <- extractKeyword1()
  kw2 <- extractKeyword2()
  kw3 <- extractKeyword3()
  allFamilySingle <- c(kw1,kw3)
  
  #Initialize dataframes elements
  KEYWORD <- NULL
  VULNERABILITY_ASSET <- NULL
  CAPEC_ID <- NULL
  
  print("Extraction began, please wait ...")
  for(w in allFamilySingle){
    part <- searchTitles4VA(pathWorkDirectory,w)
    #Cleaning attack design title
    id <- NA
    res <- NA
    for(ws in part){
      
      #Get the ID
      id <- str_extract(ws,"\\d+")
      ws <- gsub("_|\\d","",ws)
      #Ici faire préposition (découpe en fonction des mots clés : via, into... et faire la recursive en fonction des découpes.)
      #Verify preposition presence:
      prepositionPresence <- str_extract(ws,"via|into")
      particularCancel <- str_extract(ws,"Pretexting via")
      #If preposition therefore:
      if(!is.na(prepositionPresence) && is.na(particularCancel)){
        #So we can cut the tense thanks preposition words
        regex <- "via|into"
        rez <- strsplit(ws,regex)
        #print(rez)
        rez <- unlist(rez)
        #for each sub tense, we'll check the presence of VA (in a recursive way)
        for(r in rez){
          #Clean useless spaces
          r <- gsub("^\\s|$\\s","",r)
          #print(c("R IS : ",r))
          res <- ""
          VAs <- recursiveVAextraction(r,res)
          for(va in VAs){
            KEYWORD <- c(KEYWORD,w)
            VULNERABILITY_ASSET <- c(VULNERABILITY_ASSET,va)
            CAPEC_ID <- c(CAPEC_ID,id)
          }
        }
      }else if(!is.na(particularCancel) || is.na(prepositionPresence)){
        #Else continue normal way
        res <- ""
        VAs <- recursiveVAextraction(ws,res)
        for(va in VAs){
          KEYWORD <- c(KEYWORD,w)
          VULNERABILITY_ASSET <- c(VULNERABILITY_ASSET,va)
          CAPEC_ID <- c(CAPEC_ID,id)
        }
      }
    }
  }
  for(w in kw2){
    part <- searchTitles4VAComposedWords(pathWorkDirectory,w)
    #print(part)
    #Cleaning attack design title
    id <- NA
    res <- NA
    for(ws in part){
      #Ici faire préposition (découpe en fonction des mots clés : via, into... et faire la recursive en fonction des découpes.)
      #ATTENTION : Faire exception à "Pretexting via"
      #Get the ID
      id <- str_extract(ws,"\\d+")
      ws <- gsub("_|\\d","",ws)
      res <- ""
      VAs <- recursiveVAextraction(ws,res)
      #print(VAs)
      for(va in VAs){
        KEYWORD <- c(KEYWORD,w)
        VULNERABILITY_ASSET <- c(VULNERABILITY_ASSET,va)
        CAPEC_ID <- c(CAPEC_ID,id)
      }
    }
  }
  #Building dataframe
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
#recursiveVA("D:/Documents/ARCHWARE/R",1)
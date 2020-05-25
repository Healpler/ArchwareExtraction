#Ajusting working path
#setwd("D:/Documents/ARCHWARE/R")

#Ajusting packages
if(!require(tm)){
  install.packages("tm")
  library(tm)
}
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
if(!require(XML)){
  install.packages("XML")
  library(XML)
}
if(!require(methods)){
  install.packages("methods")
  library(methods)
}
if(!require(xml2)){
  install.packages("xml2")
  library(xml2)
}
if(!require(XML)){
  install.packages("XML")
  library(XML)
}

source("extractVAprocess.R")
#############################################################
#############################################################

generateXML <- function(pathWorkDirectory){
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
  parsing <- xmlParse("capec.xml",useInternalNode=TRUE)
  r <- xmlRoot(parsing)

  #Remove useless sections
  removeNodes(r[names(r)=="Categories"])
  removeNodes(r[names(r)=="Views"])
  removeNodes(r[names(r)=="External_References"])
  
  #Load all attack_pattern (single)
  kids <- xmlChildren(r)
  attack_patterns <- kids$Attack_Patterns
  #first column = index of attack_pattern
  #second column = index of information into a signle attack pattern specify by first column
  
  #Get a Dataframe of extracted VA
  dfVa <- extractVA(pathWorkDirectory,3)
  
  #Processing XML
  for(i in 1:xmlSize(attack_patterns)){
    #Remove useless nodes
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Content_History"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Execution_Flow"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Prerequisites"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Likelihood_Of_Attack"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Description"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Typical_Severity"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Skills_Required"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Taxonomy_Mappings"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Resources_Required"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Consequences"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Mitigations"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Example_Instances"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="Indicators"])
    removeNodes(attack_patterns[[i]][names(attack_patterns[[i]])=="References"])
    #Remove useless attributes
    removeAttributes(attack_patterns[[i]],"Status")
    #Searching VA corresponding...
    vas <- searchVAByID(pathWorkDirectory,xmlAttrs(attack_patterns[[i]])[["ID"]],dfVa)
    for(va in vas){
      addChildren(attack_patterns[[i]],newXMLNode("Vulnerability_Asset",va))
    }
  }
  
  #Saving processed XML in a file
  if(!file.exists("Hierarchy_Attck_Pattrn.xml")){
    file.create("Hierarchy_Attck_Pattrn.xml")
  }
  saveXML(attack_patterns,"Hierarchy_Attck_Pattrn.xml")
  file.remove("capec.xml")
  file.remove("capec.zip")
  print("XML generated.")
}

#help(saveXML)
#Get attribute and specify its name of an XML node.
#print(xmlAttrs(attack_patterns[[1]])[["ID"]])
#print(attack_patterns[[1]])
#generateXML("D:/Documents/ARCHWARE/R")
########--------Homework 4-------#########
#name: Rongyao Huang
#UNI: rh2648

rm(list=ls())
setwd("C:/Users/Rongyao/SkyDrive (2)/2014Spring/Applied Data Science/HW/HW4")

if(!require("stringr")){
  install.packages("stringr")
  library(stringr)
}
if(!require("utils")){
  install.packages("utils")
  library(utils)
}


########--------Q1-------#########

#read in the sample phone number file, strip the blank lines
text1<-scan(file="problem1.txt",what="character",blank.lines.skip=T,sep="\n")

#since one record may be separated into two lines, I first concatenate all lines together with sep=" "
text2<-str_c(text1,collapse=" ")

#####extra credit for problem1########
#some area codes are in parenthesis,I simply remove "\\(" and ")" from the text because area code is not of particular interest
glob2rx("(")
glob2rx(")")
text3<-str_replace_all(text2,"\\(","")
text3<-str_replace_all(text3,")","")

#two type of phone numbers present in the sample:1-xxx[- ]xxx-xxxx(4 parts) or xxx[- ]xxx-xxxx(3 parts)
#in the original records,parts of one records can be separated into two lines;
#after concatenation, there can be extra space either in front of or after the [- ] sign

#also, I regard the letters before a phone number as its type info; if missing, then the number gets tagged "unclassified"

#I first deal with type 1-xxx[- ]xxx-xxxx
#with type info

phone1<-"[[:alpha:]]+:[ ]+1[ ]{0,1}-[ ]{0,1}([2-9][0-9]{2})[ ]{0,1}[- ][ ]{0,1}([0-9]{3})[ ]{0,1}[- ][ ]{0,1}([0-9]{4})"
num1<-str_extract_all(text3,phone1)[[1]]
#remove all the spaces
num1<-str_replace_all(num1," ","")
#extract type info
require(plyr)
book1<-ldply(str_split(num1,":"),function(x)data.frame(type=x[1],number=x[2]))

#without type info

phone2<-"[0-9][ ]+1[ ]{0,1}-[ ]{0,1}([2-9][0-9]{2})[ ]{0,1}[- ][ ]{0,1}([0-9]{3})[ ]{0,1}[- ][ ]{0,1}([0-9]{4})"
num2<-str_extract_all(text3,phone2)[[1]]
num2<-str_replace_all(num2,"^[0-9]","")
#remove all the spaces
num2<-str_replace_all(num2," ","")
#add type info
book2<-data.frame(type=rep("unclassified",length(num2)),number=num2)

#in quite the same light, I deal with type xxx[- ]xxx-xxxx
#with type info

phone3<-"[[:alpha:]]+:[ ]+([2-9][0-9]{2})[ ]{0,1}[- ][ ]{0,1}([0-9]{3})[ ]{0,1}[- ][ ]{0,1}([0-9]{4})"
num3<-str_extract_all(text3,phone3)[[1]]   #there is no such record

#without type info
phone4<-"([2-9][0-9]{2})[ ]{0,1}[- ][ ]{0,1}([0-9]{3})[ ]{0,1}[- ][ ]{0,1}([0-9]{4})"
num4<-str_extract_all(text3,phone4)[[1]]   
#remove all the space and -
num4<-str_replace_all(num4,"[ -]","")
#reformat the number
num4<-sapply(num4,function(x)paste(str_sub(x,1,3),"-",str_sub(x,4,6),"-",str_sub(x,7,10),collapse="",sep=""))
#add type info
book4<-data.frame(type=rep("unclassified",length(num4)),number=num4)
row.names(book4)<-NULL

#now we are ready to paste everything together 
phonebook<-rbind(book1,book2,book4)

write.csv(phonebook,"PhoneBook.csv")

#we can check each type of phone numbers
table(phonebook$type)





########--------Q2-------#########

#read in the sample expression file
exp<-scan(file="problem2.txt",what="character",blank.lines.skip=T,sep=" ")

#remove blank fields
exp<-exp[!(exp=="")]
#split exp into number operator number,and do the calculation

#####extra credit for problem1########

#this methods can deal with both interger and fractional decimals
result<-sapply(exp,function(x){
  split<-as.numeric(str_locate(x,"[[:digit:]][+-]")[1,2])
  num1<-as.numeric(str_sub(x,1,split-1))
  operator<-str_sub(x,split,split)
  num2<-as.numeric(str_sub(x,split+1,nchar(x)))
  if (operator=="+"){
    num1+num2
  }else{
    num1-num2
  }
})
names(result)<-NULL

print(result)



########--------Homework 4-------#########
#name: Rongyao Huang
#UNI: rh2648

rm(list=ls())
setwd("C:/Users/Rongyao/SkyDrive (2)/2014Spring/Applied Data Science/HW/HW4")

library(stringr)
library(utils)

########--------Q1-------#########

#modify the sample text given in Rlog to include more complicated cases
#e.g. blank lines, separate type and phone number by \n, uppercase and lowercase

#copy the sample text
text <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
          "387 287 6718", "apple", "233.398.9187 ", "482 952 3315", "239 923 8115",
          "842 566 4692", "Work: 579-499-7527", "$1000", "Home: 543.355.3679")
#write out as a .txt file and modify
writeLines(text,con="text.txt",sep="\n",useBytes=T)    #useBytes will transform UTF to character
#read the modified version in
text<-scan("text.txt",what=character(),sep="\n",blank.lines.skip=T)   #use the blank.lines.skip, I get rid of the blanks lines in the file
#remove the leading and trailing white spaces
text1<-str_trim(text)

#####code for extra credit########
#if some area codes are in parenthesis,
#I can simply remove "\(""\)" from the text
glob2rx()
str_replace_all()

#as type and phone number may be separated in two lines
#I first concatenate them together with a comma
text2<-str_c(text1,collapse=",")

#then I try to deal separately with phone numbers that have type info attached and those without

#######pick out the phone numbers with type info#########

typephone<-"[[:alpha:]]+:[ ,]([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

#check if the regexpr used is effective
str_extract_all(text2,typephone) 

#extract
loc<-str_locate_all(text2,typephone)[[1]]
phone_type<-str_sub(text2, loc[,"start"], loc[,"end"])

#substitute comma with a space
phone_type2<-str_replace_all(phone_type, ",", " ")

#change all letters to lower case
phone_type3<-tolower(phone_type2)

#reformat phone numbers
phone_type4<-str_replace_all(phone_type3,"-"," ")
phone_type4<-str_replace_all(phone_type4,"\\."," ")

#classify the phone numbers
phone_type5<-as.data.frame(sapply(seq_along(phone_type4),function(i)unlist(strsplit(phone_type4[i],":"))))

phone_type5<-t(phone_type5)
row.names(phone_type5)<-NULL
phone_type5[,2]<-str_replace_all(phone_type5[,2],"^[[:blank:]]","")

#########pick out the phone numbers with no type info#########

phone<-",([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4}),"

#check if the regexpr used is effective
phone_notype<-str_extract_all(text2,phone)[[1]]

#remove all the comma
phone_notype2<-str_replace_all(phone_notype, ",","")

#reformat phone numbers
phone_notype3<-str_replace_all(phone_notype2,"-"," ")
phone_notype3<-str_replace_all(phone_notype3,"\\."," ")

#add type "unknown"
phone_notype4<-rbind(rep("unclassified",length(phone_notype3)),phone_notype3)

phone_notype4<-t(phone_notype4)
row.names(phone_notype4)<-NULL

###########Now we can join the all the phone number together#########
phone_book<-rbind(phone_notype4,phone_type5)
colnames(phone_book)<-c("Type","Number")

write.csv(phone_book,"PhoneBook.csv")


########--------Q2-------#########


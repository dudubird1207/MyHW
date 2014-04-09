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

#modify the sample text given in Rlog to include more complicated cases
#e.g. blank lines, separate type and phone number by \n, uppercase and lowercase

#copy the sample text
text <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
          "387 287 6718", "apple", "233.398.9187 ", "482 952 3315", "239 923 8115",
          "842 566 4692", "Work: 579-499-7527", "$1000", "Home: 543.355.3679")

#modify the sample text to include more complicated cases
text<-c(text,"work:\n378 897 3792","home:\n (589)-893-8926","\n","(783)-372-8922","(387).897.9038")

#write out the text file
writeLines(text,"text.txt",sep="\n",useBytes=T)
#read it in, strip the blank lines
text1<-scan(file="text.txt",what="character",blank.lines.skip=T,sep="\n")
#remove the leading and trailing white spaces
text2<-str_trim(text1)

#####extra credit for problem1########
#if some area codes are in parenthesis,
#I can simply remove "\\(" and ")" from the text
glob2rx("(")
glob2rx(")")
text3<-str_replace_all(text2,"\\(","")
text3<-str_replace_all(text3,")","")

#as type and phone number may be separated in two lines
#I first concatenate them together with a comma
text4<-str_c(text3,collapse=",")

#then I try to deal separately with phone numbers that have type info attached and those without

#######pick out the phone numbers with type info#########

typephone<-"[[:alpha:]]+:[ ,]([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

#check if the regexpr used is effective
str_extract_all(text4,typephone) 

#extract
loc<-str_locate_all(text4,typephone)[[1]]
phone_type<-str_sub(text4, loc[,"start"], loc[,"end"])

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

print(phone_type5)
#########pick out the phone numbers with no type info#########

phone<-",([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4}),"

#check if the regexpr used is effective
phone_notype<-str_extract_all(text4,phone)[[1]]

#remove all the comma
phone_notype2<-str_replace_all(phone_notype, ",","")

#reformat phone numbers
phone_notype3<-str_replace_all(phone_notype2,"-"," ")
phone_notype3<-str_replace_all(phone_notype3,"\\."," ")

#add type "unknown"
phone_notype4<-rbind(rep("unclassified",length(phone_notype3)),phone_notype3)

phone_notype4<-t(phone_notype4)
row.names(phone_notype4)<-NULL

print(phone_notype4)
###########Now we can join the all the phone number together#########
phone_book<-rbind(phone_notype4,phone_type5)
colnames(phone_book)<-c("Type","Number")
print(phone_book)

write.csv(phone_book,"PhoneBook.csv")


########--------Q2-------#########

#create sample text with expression "a+-b" where a can be any real number

num<-round(runif(20,-10,10))
ope<-sample(c("+","-"),10,replace=T)
exp<-paste(num[1:10],ope,num[11:20],sep="")
#add sums that involve fractional decimals
fra<-runif(4,5,10)
exp2<-paste(fra[1:2],sample(c("+","-"),2,replace=T),fra[3:4],sep="")
exp<-c(exp,exp2)        #These 12 expressions are our sample.

#store the sample expression in a txt file
writeLines(exp,"Expression.txt",sep="\n")

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



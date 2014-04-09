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

#####extra credit for problem1########
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

#two cases present in the sample: 
#strings that start with a positive number and strings that start with a negtive number
#I will deal with these two cases separately

pos<-"^[[:digit:]]+"
neg<-"^-[[:digit:]]+"

#using str_detect, we devide the sample into subset
#with exp1, the operator is first [+-]; with exp2,it is the second
exppos<-exp[str_detect(exp,pos)]
expneg<-exp[str_detect(exp,neg)]

#split exppos into number operator number,and do the calculation
result1<-sapply(exppos,function(x){
  split<-as.numeric(str_locate(x,"[+-]")[1,1])
  num1<-as.numeric(str_sub(x,1,split-1))
  operator<-str_sub(x,split,split)
  num2<-as.numeric(str_sub(x,split+1,nchar(x)))
  if (operator=="+"){
    num1+num2
  }else{
    num1-num2
  }
})
names(result1)<-NULL

#split expneg into number operator number,and do the calculation
result2<-sapply(expneg,function(x){
  split<-as.numeric(str_locate_all(x,"[+-]")[[1]][2,1])
  num1<-as.numeric(str_sub(x,1,split-1))
  operator<-str_sub(x,split,split)
  num2<-as.numeric(str_sub(x,split+1,nchar(x)))
  if (operator=="+"){
    num1+num2
  }else{
    num1-num2
  }
})
names(result2)<-NULL



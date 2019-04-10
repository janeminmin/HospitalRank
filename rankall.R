rankall <- function(outcome, num = "best") {
       
        mydata<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Read outcome data
        if (outcome!="heart attack"&& outcome!="heart failure"&& outcome!="pneumonia"){
                print("invalid outcome")
                stop()
        }
        ## Check that outcome are valid
        ## For each state, find the hospital of the given rank
        myoutcome1<-mydata[,c(2,7)]
        myoutcome<-data.frame(myoutcome1,as.numeric(mydata[,11]),as.numeric(mydata[,17]),as.numeric(mydata[,23]))
        names(myoutcome)<-c("hospital","state","heart attack","heart failure","pneumonia")
        
        nb<-as.data.frame(table(myoutcome$state)) ##number of repetition of each state
        ttall<-data.frame("hospital"="NA","state"=nb$Var1,stringsAsFactors=F)
        for(i in 1:nrow(nb)){
                state<-nb$Var1[i]
                startid<-match(state,myoutcome$state)
                idnb<-nb[match(state,nb[,1]),2]
                endid<-startid+idnb-1
                srankls1<-myoutcome[startid:endid,c("hospital",outcome)]
                srankls2<-srankls1[order(srankls1[outcome],srankls1$hospital),]
                srankls3 <-srankls2[!is.na(srankls2[outcome]),]
                srankls <-data.frame(rep(state,times=nrow(srankls3)),srankls3$hospital,c(1:nrow(srankls3)),stringsAsFactors=F)
                names(srankls)<-c("State","hospital","Rank")
               
                if (isTRUE(num=="best")){
                        ttall$hospital[i]<-head(srankls$hospital,1)
                }else if (isTRUE(num=="worst")){
                        ttall$hospital[i]<-tail(srankls$hospital,1)
                }else if (num>nrow(srankls)){
                        ttall$hospital[i]<-NA
                }else if (num%in%srankls$Rank){
                        id<-match(num,srankls$Rank)
                        ttall$hospital[i]<-srankls[id,2]
                }
        }
          ttall  
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
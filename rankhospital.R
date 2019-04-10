rankhospital <- function(state, outcome, num = "best") {
        mydata<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Read outcome data
        if (outcome!="heart attack"&& outcome!="heart failure"&& outcome!="pneumonia"){
                print("invalid outcome")
                stop()
        }
        ## Check that state and outcome are valid
        stlist<-unique(mydata[,7])
        if(! state %in% stlist){
                print("invalid outcome")
                stop()
        }
        ## Return hospital name in that state with the given rank
        myoutcome1<-mydata[,c(2,7)]
        myoutcome<-data.frame(myoutcome1,as.numeric(mydata[,11]),as.numeric(mydata[,17]),as.numeric(mydata[,23]))
        names(myoutcome)<-c("hospital","state","heart attack","heart failure","pneumonia")
        
        nb<-as.data.frame(table(myoutcome$state)) ##number of repetition of each state
        startid<-match(state,myoutcome$state)
        idnb<-nb[match(state,nb[,1]),2]
        endid<-startid+idnb-1
        
        srankls1<-myoutcome[startid:endid,c("hospital",outcome)]
        srankls2<-srankls1[order(srankls1[outcome],srankls1$hospital),]
        srankls3 <-srankls2[!is.na(srankls2[outcome]),]
        srankls <-data.frame(srankls3,c(1:nrow(srankls3)))
        names(srankls)<-c("Hospital.Name","Rate","Rank")
        
        if (isTRUE(num=="best")){
                head(srankls$Hospital.Name,1)
        }else if (isTRUE(num=="worst")){
                tail(srankls$Hospital.Name,1)
        }else if (num>nrow(srankls)){
                NA
        }else if (num%in%srankls$Rank){
              id<-match(num,srankls$Rank)
              srankls[id,1]
        }
                
        ## 30-day death rate
}
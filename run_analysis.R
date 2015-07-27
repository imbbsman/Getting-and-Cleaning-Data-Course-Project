
DownloadDataSet = function(url) {
    if (!file.exists("data")) {
        message("create data folder...")
        dir.create("data")
    }
    if (!file.exists("data/UCI HAR Dataset")) {
       #sourceURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        zipfile="data/UCI_HAR_data.zip"
        message("data download...")
        download.file(url, destfile=zipfile, method="auto")
        unzip(zipfile, exdir="data")
    }
}



LoadMergeData = function() {
    message("load data...")
    path<<-paste(getwd(),"/data/UCI HAR Dataset/", sep = "")
    
    message("  read X_train.txt...")
    train.dat = read.csv(paste(path,"train/X_train.txt",sep=""), sep="", header=FALSE)
    
    message("  read Y_train.txt...")
    train.dat[,ncol(train.dat)+1] = read.csv(paste(path,"train/Y_train.txt",sep=""), sep="", header=FALSE)
    
    message("  read subject_train.txt...")
    train.dat[,ncol(train.dat)+1] = read.csv(paste(path,"train/subject_train.txt",sep=""), sep="", header=FALSE)
    
    message("  read X_test.txt...")
    test.dat = read.csv(paste(path,"test/X_test.txt",sep=""), sep="", header=FALSE)
    
    message("  read Y_test.txt...")
    test.dat[,ncol(test.dat)+1] = read.csv(paste(path,"test/Y_test.txt",sep=""), sep="", header=FALSE)
    
    message("  read subject_test.txt...")
    test.dat[,ncol(test.dat)+1] = read.csv(paste(path,"test/subject_test.txt",sep=""), sep="", header=FALSE)
    
    message("merge data...")
    rbind(train.dat, test.dat)
}




ExtractData=function(df){
    message("extract data...")
    features <- read.csv(paste(path,"features.txt", sep=""), sep="", header=FALSE)
    
    cols.in.scope <<- grep(".*-mean.*|.*-std.*", features[,2])
    
    
    features <<- features[cols.in.scope,]
    
    var.count = ncol(df)
    cols.in.scope <<- c(cols.in.scope, var.count-1, var.count)
    
    df<-df[,cols.in.scope]
    df
}




SetActivityNames = function(df){
    message("set activity labels...")
    activity.Labels = read.csv(paste(path,"activity_labels.txt", sep=""), sep="", header=FALSE)
    
    activity.ID = 1
    for (ActivityLabel in activity.Labels$V2) {
        df$activity <- gsub(activity.ID, ActivityLabel, df$activity)
        activity.ID <- activity.ID + 1
    }
    
    df
}




DescriptiveVariables = function(df){
    message("make descriptive variable names...")
    features[,2] <- gsub('-meanFreq()', '.mean.freq', features[,2]) 
    features[,2] <- gsub('-mean()', '.mean', features[,2]) 
    features[,2] <- gsub('-std()', '.std', features[,2]) 
    features[,2] <- gsub('[-]', '.', features[,2]) 
    features[,2] <- gsub('[()]', '', features[,2]) 
    
    colnames(df) <- c(features$V2, "Activity", "Subject")
    colnames(df) <- tolower(colnames(df))
    
    df
}

MakeTidy = function(df){
    message("tidy data...")
    df$activity <- as.factor(df$activity)
    df$subject <- as.factor(df$subject)
    
    countnndc = ncol(df)-2 
    nndc = c(1:countnndc) 
    
    tidy <- aggregate(df[,nndc], by=list(activity = df$activity, subject=df$subject), mean, na.rm=TRUE)
    tidy
}



DownloadDataSet("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")

Data <- LoadMergeData()

Data <- ExtractData(Data)

Data <- DescriptiveVariables(Data)

Data <- SetActivityNames(Data) 

Tidy.Data <- MakeTidy(Data)

message("write tidy.txt...")
write.table(Tidy.Data, "tidy.txt", sep="\t",row.names = F)
message("Done!")
message("Find the tidy data in file: \"",paste(getwd(),"/tidy.txt\"",sep=""))

write(names(Data), file = "variables.txt", ncolumns = 1)

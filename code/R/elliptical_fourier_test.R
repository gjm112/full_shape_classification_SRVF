#Elliptail Fourier Analysis test. 
library(Momocs)


#Load the reference file
rs <- read.csv("/Users/gregorymatthews/Dropbox/gladysvale/reference_file_20210622.csv")

#Load teeth as a list
#Loads teeth_BW_train
load("/Users/gregorymatthews/Dropbox/gladysvale/RData/teeth_BW_train_20210622.RData")

#Remove entries from the rs file that aren't in the data.
rs <- subset(rs, image %in% names(teeth_BW_train))

#Remove duplicates
rs <- rs[!duplicated(rs$image),]

#Do EFA on the teeth.  
efList <- ptsList <-list()
for (i in 1:length(teeth_BW_train)){print(i)
  efList[[i]] <- efourier(teeth_BW_train[[i]],nb.h=15)
  ptsList[[i]] <- c(efList[[i]]$an, 
                    efList[[i]]$bn, 
                    efList[[i]]$cn, 
                    efList[[i]]$dn) 
}

names(efList) <- names(ptsList) <- names(teeth_BW_train)

all <- do.call(rbind,ptsList)
row.names(all) <- names(ptsList)

#Now pull out LM1 
temp <- all[row.names(all) %in% rs$image[rs$type == "LM1"],]

#Now add PCs
#Add principle components
pc<-princomp(temp)$scores
temp<-cbind(temp,pc[,1:30])

#Now cut it in to folds like Nikki did.
#Create the folds to match what nikki did.  Then I can just run it through the same code that I alreayd have. 
folds_ref <-  read.csv("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/folds/LM1ref_folds.csv")

#Test
temp[folds_ref$image[folds_ref$folds_tribe == 1],]

#Train
temp[folds_ref$image[folds_ref$folds_tribe != 1],]







#Make a data frame.  
#Cut into folds.  
#Perform random forest and SVM to evaluate the methods on both tribe and species.  
#Evaluate accuracy 






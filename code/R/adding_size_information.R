#### Adding size information into the analysis.  
#Find size information for teeth
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

#approximate distance around curve. 
size <- list()
for (i in names(teeth_BW_train)){print(i)
size[[i]] <- sum(apply(diff(teeth_BW_train[[i]])^2,1,function(x){sqrt(sum(x))}))
}

size_df <- data.frame(image = names(size), size = unlist(size))

for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)

folds_ref <-
  read.csv(
    paste0(
      "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/folds/",
      toothtype,
      "ref_folds.csv"
    )
  )

folds_ref_with_size <- merge(folds_ref, size_df, by.x = "image", by.y = "image", all.x = TRUE)
write.csv(folds_ref_with_size, file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/folds/",toothtype,"ref_folds_with_size.csv"))

}

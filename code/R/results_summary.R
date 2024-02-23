library(tidyverse)
load("./results/results_rf_species_given_tribe.rda")
load("./results/results_svm_radial_species_given_tribe.rda")
load("./results/results_svm_linear_species_given_tribe.rda")


res <- data.frame()
for (proj in c("I", "OV", "I-PC", "OV-PC","EFA")) {print(proj)
  for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
    #Random Forest
    mat <- results_rf_species_given_tribe[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "RF", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species_given_tribe"))
    
    #SVM Linear
    mat <- results_svm_linear_species_given_tribe[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "SVM-L", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species_given_tribe"))
    
    
    #SVM Radial
    mat <- results_svm_radial_species_given_tribe[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "SVM-R", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species_given_tribe"))
    
    
    
  }
}


res$proj <- factor(res$proj, levels = c("EFA","I","OV","I-PC","OV-PC"))


png("./figures/species_given_tribe_accuracy.png", res = 300, units = "in", h = 5, w = 8)
library(ggplot2)
ggplot(aes(x = proj, y = accuracy, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method)) + xlab("Feature Generation Method") + ylab("Accuracy")
dev.off()

library(ggplot2)
ggplot(aes(x = proj, y = logloss, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))



# 
# load("./full_shape_classification_SRVF/results/results_rf_species.rda")
# load("./full_shape_classification_SRVF/results/results_svm_radial_species.rda")
# load("./full_shape_classification_SRVF/results/results_svm_linear_species.rda")
# 
# res <- data.frame()
# for (proj in c("I", "OV", "I-PC", "OV-PC","EFA")) {print(proj)
#   for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
#     #Random Forest
#     mat <- results_rf_species[[proj]][[tooth]]
#     #Accuracy
#     acc <- mean(mat$pred_class == mat$real_class)
#     #log loss
#     logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
#     res <- rbind(res,data.frame(method = "RF", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species"))
#     
#     #SVM Linear
#     mat <- results_svm_linear_species[[proj]][[tooth]]
#     #Accuracy
#     acc <- mean(mat$pred_class == mat$real_class)
#     #log loss
#     logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
#     res <- rbind(res,data.frame(method = "SVM-L", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species"))
#     
#     
#     #SVM Radial
#     mat <- results_svm_radial_species[[proj]][[tooth]]
#     #Accuracy
#     acc <- mean(mat$pred_class == mat$real_class)
#     #log loss
#     logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
#     res <- rbind(res,data.frame(method = "SVM-R", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species"))
#     
#     
#     
#   }
# }
# 
# 
# res$proj <- factor(res$proj, levels = c("EFA","I","OV","I-PC","OV-PC"))
# res$model <- factor(res$model)
# 
# png("./full_shape_classification_SRVF/figures/species_accuracy.png", res = 300, units = "in", h = 5, w = 8)
# library(ggplot2)
# ggplot(aes(x = proj, y = accuracy, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
#   geom_point(aes(group = method)) + xlab("Feature Generation Method") + ylab("Accuracy")
# dev.off()
# library(ggplot2)
# ggplot(aes(x = proj, y = logloss, colour = method, group = method, shape = model), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
#   geom_line(aes(group = method))


load("./results/results_rf_tribe.rda")
load("./results/results_svm_radial_tribe.rda")
load("./results/results_svm_linear_tribe.rda")

res <- data.frame()
for (proj in c("I", "OV", "I-PC", "OV-PC","EFA")) {print(proj)
  for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
    #Random Forest
    mat <- results_rf_tribe[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "RF", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "tribe"))
    
    #SVM Linear
    mat <- results_svm_linear_tribe[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "SVM-L", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "tribe"))
    
    
    #SVM Radial
    mat <- results_svm_radial_tribe[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "SVM-R", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "tribe"))
    
    
    
  }
}


res$proj <- factor(res$proj, levels = c("EFA","I","OV","I-PC","OV-PC"))
res$model <- factor(res$model)


#Overall classification rate
all <- do.call(rbind,results_svm_radial_tribe[["I"]])
mean(all$pred_class == all$real_class)


png("./figures/tribe_accuracy.png", res = 300, units = "in", h = 5, w = 8)
library(ggplot2)
ggplot(aes(x = proj, y = accuracy, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))+ xlab("Feature Generation Method") + ylab("Accuracy")
dev.off()


library(ggplot2)
ggplot(aes(x = proj, y = logloss, colour = method, group = method, shape = model), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))



#By tribe classification accuracy
load("./results/results_svm_radial_tribe.rda")
library(tidyverse)
out <- lapply(results_svm_radial_tribe[["I"]], function(x){x %>% group_by(real_class) %>% summarise(mn = mean(pred_class == real_class), n = n())})

acc_ind <- do.call(rbind,out)
acc_ind$toothtype <- rep(names(out), each = 7)

png("./figures/tribe_accuracy_by_tribe.png", res = 300, units = "in", h = 5, w = 8)
ggplot(aes(x = toothtype, y = mn, color = real_class, size = n), data = acc_ind) + geom_point()  + geom_line(aes(group = real_class), size = 0.5) + xlab("Tooth Type") + ylab("Accuracy") + scale_color_discrete(name = "True Tribe") + scale_size_continuous(name = "Sample Size")
dev.off()




####################################
#Cohen's Kappa Tribe
####################################
load("./results/results_rf_tribe.rda")
load("./results/results_svm_radial_tribe.rda")
load("./results/results_svm_linear_tribe.rda")
library(caret)
res <- data.frame()
for (proj in c("I", "OV", "I-PC", "OV-PC","EFA")) {print(proj)
  for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
    #Random Forest
    mat <- results_rf_tribe[[proj]][[tooth]]
    xtab <- table(mat$pred_class, mat$real_class)
    cm <- caret::confusionMatrix(xtab)
    kappa <- cm$overall["Kappa"]
    acc <- cm$overall["Accuracy"]
    #log loss
    res <- rbind(res,data.frame(method = "RF", proj = proj, acc = acc, tooth = tooth, kappa = kappa, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "tribe"))
    
    #SVM Linear
    mat <- results_svm_linear_tribe[[proj]][[tooth]]
    mat$pred_class <- factor(mat$pred_class, levels = levels(mat$real_class))
    xtab <- table(mat$pred_class, mat$real_class)
    cm <- caret::confusionMatrix(xtab)
    kappa <- cm$overall["Kappa"]
    acc <- cm$overall["Accuracy"]
    res <- rbind(res,data.frame(method = "SVM-L", proj = proj, acc = acc, tooth = tooth, kappa = kappa, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "tribe"))
    
    
    #SVM Radial
    mat <- results_svm_radial_tribe[[proj]][[tooth]]
    mat$pred_class <- factor(mat$pred_class, levels = levels(mat$real_class))
    xtab <- table(mat$pred_class, mat$real_class)
    cm <- caret::confusionMatrix(xtab)
    kappa <- cm$overall["Kappa"]
    acc <- cm$overall["Accuracy"]
    res <- rbind(res,data.frame(method = "SVM-R", proj = proj,acc = acc, tooth = tooth, kappa = kappa, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "tribe"))
    
    
    
  }
}

res$proj <- factor(res$proj, levels = c("EFA","I","OV","I-PC","OV-PC"))
#res <- res %>% filter(method != "XG")

png("./figures/tribe_kappa.png", res = 300, units = "in", h = 5, w = 8)
library(ggplot2)
ggplot(aes(x = proj, y = kappa, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method)) + xlab("Feature Generation Method") + ylab("Kappa")
dev.off()

########################################################################
#Cohens Kappa. Species given tribe
########################################################################
load("./results/results_rf_species_given_tribe.rda")
load("./results/results_svm_radial_species_given_tribe.rda")
load("./results/results_svm_linear_species_given_tribe.rda")
library(caret)
logloss <- NA
res <- data.frame()
for (proj in c("I", "OV", "I-PC", "OV-PC","EFA")) {print(proj)
  for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
    #Random Forest
    mat <- results_rf_species_given_tribe[[proj]][[tooth]]
    xtab <- table(mat$pred_class, mat$real_class)
    cm <- caret::confusionMatrix(xtab)
    kappa <- cm$overall["Kappa"]
    #log loss
    res <- rbind(res,data.frame(method = "RF", proj = proj, tooth = tooth, kappa = kappa, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species_given_tribe"))
    
    #SVM Linear
    mat <- results_svm_linear_species_given_tribe[[proj]][[tooth]]
    xtab <- table(mat$pred_class, mat$real_class)
    cm <- caret::confusionMatrix(xtab)
    kappa <- cm$overall["Kappa"]
    res <- rbind(res,data.frame(method = "SVM-L", proj = proj, tooth = tooth, kappa = kappa, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species_given_tribe"))
    
    
    #SVM Radial
    mat <- results_svm_radial_species_given_tribe[[proj]][[tooth]]
    xtab <- table(mat$pred_class, mat$real_class)
    cm <- caret::confusionMatrix(xtab)
    kappa <- cm$overall["Kappa"]
    res <- rbind(res,data.frame(method = "SVM-R", proj = proj, tooth = tooth, kappa = kappa, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species_given_tribe"))
    
    
  }
}

res$proj <- factor(res$proj, levels = c("EFA","I","OV","I-PC","OV-PC"))
#res <- res %>% filter(method != "XG")

png("./figures/species_given_tribe_kappa.png", res = 300, units = "in", h = 5, w = 8)
library(ggplot2)
ggplot(aes(x = proj, y = kappa, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method)) + xlab("Feature Generation Method") + ylab("Kappa")
dev.off()



####################################################################
###Tribe ROC curves
####################################################################
library(ggplot2)
library(pROC)
load("./results/results_rf_tribe.rda")
load("./results/results_svm_radial_tribe.rda")
load("./results/results_svm_linear_tribe.rda")

res <- data.frame()
for (proj in c("I", "OV", "I-PC", "OV-PC","EFA")) {print(proj)
  for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
    #Random Forest
    mat <- results_svm_radial_tribe[[proj]][[tooth]]
    for (tribe in c("Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini")){
      
      #define object to plot
      rocobj <- roc(mat$real_class == tribe, mat$Alcelaphini)
      res <- rbind(res,data.frame(tribe = tribe, method = "RF", proj = proj, tooth = tooth,tooth_num = substring(tooth,3,3), tooth_char= substring(tooth,1,2), x = 1-rocobj$specificities, y = rocobj$sensitivities))
      
      
    }
    
  }
}

png("./figures/tribe_roc.png", res = 300, units = "in", h = 5, w = 8)
ggplot(aes(x = x, y = y, color = tribe), data = res %>% filter(proj == "I" & method == "RF"))+ geom_abline(slope= 1, lty=2, alpha = 0.5) + geom_path() + facet_grid(tooth_num ~ tooth_char) + xlab("") + ylab("") + scale_x_continuous(breaks = c(0,0.5,1), labels = c(0,0.5,1)) + scale_y_continuous(breaks = c(0,0.5,1), labels = c(0,0.5,1)) 
dev.off()
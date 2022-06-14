load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_species_given_tribe.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_species_given_tribe.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_species_given_tribe.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_species_given_tribe.rda")


#names(results_rf_species_given_tribe[["I"]][["LM1"]])

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

library(ggplot2)
ggplot(aes(x = proj, y = accuracy, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))

library(ggplot2)
ggplot(aes(x = proj, y = logloss, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))




load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_species.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_species.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_species.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_species.rda")

res <- data.frame()
for (proj in c("I", "OV", "I-PC", "OV-PC","EFA")) {print(proj)
  for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {
    #Random Forest
    mat <- results_rf_species[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "RF", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species"))
    
    #SVM Linear
    mat <- results_svm_linear_species[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "SVM-L", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species"))
    
    
    #SVM Radial
    mat <- results_svm_radial_species[[proj]][[tooth]]
    #Accuracy
    acc <- mean(mat$pred_class == mat$real_class)
    #log loss
    logloss <- mean(-apply(mat, 1, function(x){log(as.numeric(x[x[["real_class"]]]))}))
    res <- rbind(res,data.frame(method = "SVM-R", proj = proj, tooth = tooth, accuracy = acc, logloss = logloss, toothchar = substring(tooth,1,2), toothnum = substring(tooth,3,3), model = "species"))
    
    
  }
}


res$proj <- factor(res$proj, levels = c("EFA","I","OV","I-PC","OV-PC"))
res$model <- factor(res$model)

library(ggplot2)
ggplot(aes(x = proj, y = accuracy, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_point(aes(group = method))

library(ggplot2)
ggplot(aes(x = proj, y = logloss, colour = method, group = method, shape = model), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))










load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_tribe.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_tribe.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_tribe.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_tribe.rda")

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

library(ggplot2)
ggplot(aes(x = proj, y = accuracy, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))

library(ggplot2)
ggplot(aes(x = proj, y = logloss, colour = method, group = method, shape = model), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~toothchar) + 
  geom_line(aes(group = method))




##################################################################################
# Article: Causal inference of the relationship between fluctuating asymmetry 
#        and fitness in humans
# Year: 2022
# Author: Arodi Farrera




####################################################################################
# A) ROBUSTNESS TESTS                                                           ####
# Tests  used 
# A.1 to detect causal misspecification in the proposed DAG                        
# A.2 to test whether the inferences are robust 
#                                                                                  
#                                                                                  
# Reference: 
# Textor et al., 2016. Robust causal inference using directed acyclic   
# graphs: the R package 'dagitty'.                                     


library("dagitty")
library("ggplot2")
library("gridExtra")

## DATA
d <- read.table("data.txt", header = T)
d <- d[d$sex == 1, -1] #only females
d <- na.omit(d) #deleting missing values


## CAUSAL MODEL
#latent = not observed
g <- dagitty('dag {
  bb="0,0,1,1"
  No_offspring [pos="0.544,0.227"]
  dev_noise [latent,pos="0.262,0.508"]
  face_FA [pos="0.539,0.499"]
  face_size [pos="0.464,0.349"]
  height [pos="0.261,0.227"]
  dev_noise -> face_FA
  face_size -> dev_noise
  height -> No_offspring
  height -> dev_noise
  height -> face_size
}
')

# Plotting the causal model
plot(g)



##### A.1: Evaluating DAG-dataset consistency ####


# This test evaluates whether the DAG specified is consistent with the dataset 
r <- localTests(g, d) # evaluates the d-separation implication of the DAG
r$p.value <- p.adjust(r$p.value) # statistical correction for multiple testing
r <- r[r$p.value < 0.05, ] # only significant results after correction


plotLocalTestResults(r, xlim = c(-0.5, 0.5)) # expected conditional independencies in the data, given the DAG
# this result means that: 
# the number of offspring and facial FA are conditionally dependent 
# given the adult height, in other words, that even though the specified DAG 
# implies that both variables are independent (i.e. no arrow), 
# they are not (estimate = 0.367, p-value < 0.05), when conditioning on adult height. 
plot(g)
# Therefore, it is needed a new DAG that takes into account this relationship.


## Second causal model 
g <- dagitty('dag {
  bb="0,0,1,1"
  No_offspring [pos="0.544,0.227"]
  dev_noise [latent,pos="0.262,0.508"]      
  face_FA [pos="0.539,0.499"]
  face_size [pos="0.464,0.349"]
  height [pos="0.261,0.227"]
  dev_noise -> face_FA
  face_FA -> No_offspring
  face_size -> dev_noise
  height -> No_offspring
  height -> dev_noise
  height -> face_size
}
')


plot(g)


r <- localTests(g, d)
r$p.value <- p.adjust(r$p.value)
r <- r[r$p.value < 0.05, ]
plotLocalTestResults(r, xlim = c(-0.5, 0.5))
# this result means that: 
## the new DAG is consistent with the data




##### A.2: Evaluating statistically equivalent DAGs ####

# This test evaluates the minimal set of adjustments (covariates) for
# different statistically equivalent DAGs.
# A robust set would be one that is similar to the set of all
# the statistically equivalent DAGs


# statistically equivalent DAGs
ec <- equivalentDAGs(g)

# plotting the statistically equivalent DAGs
for (i in seq_along(ec)) {
  plot(ec[[i]], main = "")
    title(main = paste("Equivalent DAG: ", i),
        cex.main = 1,   font.main= 2, 
        col.main= "red", line = 3)
  }



# minimal sufficient adjustments for each of these equivalent DAGs
# to test the influence of face asymmetry (face_FA) on reproductive success (No_offspring)
min_set <- c()
for (i in seq_along(ec)) {
  min_set[i] <- adjustmentSets(ec[[i]], "face_FA", "No_offspring")  
  }

print(min_set)
# 6 out of 8 DAGs have an equivalent set, which means
# that the set is robust and that if the DAG (theoretical assumptions) was slightly
# different, a different set of covariates would not be needed to test this hypothesis



##############################################################################################
# B) COVARIATE SELECTION                                                                  ####

### The "good genes" hypothesis: FA encoding  genetic quality
# minimal set of adjustments necessary to draw conclusions on this relationship, given the DAG:


# This hypothesis implies a slightly different DAG
g.1 <- dagitty('dag {
  bb="0,0,1,1"
  No_offspring [pos="0.544,0.227"]
  dev_noise [latent,pos="0.262,0.508"]
  face_FA [pos="0.539,0.499"]
  face_size [latent, pos="0.464,0.349"]
  height [latent, pos="0.261,0.227"]
  dev_noise -> face_FA
  face_FA -> No_offspring
  face_size -> dev_noise
  height -> No_offspring
  height -> dev_noise
  height -> face_size
}
')

# it is the same DAG as before but facial asymmetry and 
# number of offsprings are the only observed variables.
plot(g.1) 

# the effect of face asymmetry on number of offspring
adjustmentSets(g.1, "face_FA", "No_offspring") #no adjustment



### The "good development" hypothesis: FA encoding developmental plasticity
# minimal set of adjustments necessary to draw conclusions on this relationship, given the DAG:

# the effect of face asymmetry on number of offspring
adjustmentSets(g, "face_FA", "No_offspring") # adjust for height
# the effect of height on number of offspring
adjustmentSets(g, "height", "No_offspring") # no adjustment
# the effect of height on face asymmetry
adjustmentSets(g, "height", "face_FA") # no adjustment



### The "growth" hypothesis: FA as a by-product of the development of body size
# minimal set of adjustments necessary to draw conclusions on this relationship, given the DAG: 

# the effect of face assymmetry on number of offspring
adjustmentSets(g, "face_FA", "No_offspring") # adjust for height
# the effect of height on face size 
adjustmentSets(g, "height", "face_size") # no adjustment
# the effect of face size on face asymmetry
adjustmentSets(g, "face_size", "face_FA") # adjust for height



## DESCRIPTIVE STATISTICS: FA, height, and number of offspring
traits <- data.frame(face_FA = d$face_FA, 
                     height = d$height, 
                     No_offspring = d$No_offspring)

apply(traits, 2, mean)
apply(traits, 2, sd)
apply(traits, 2, max)
apply(traits, 2, min)

##############################################################################################
# C) STATISTICAL ANALYSES                                                                 ####
# C.1. The "good genes" hypothesis.
# C.2. The "good development" hypothesis: FA encoding developmental plasticity               #
# C.3. The "growth" hypothesis: FA as a by-product of the development of body size           #


# Continuous variables were standardized prior to the analysis
d$face_FA.s <- scale(d$face_FA, T, T)
d$height.s <- scale(d$height, T, T)
d$face_size.s <- scale(d$face_size, T, T)


#C.1 
# the  effect of facial FA on number of offspring
C.1 <- glm(No_offspring ~ 1 + face_FA.s, family="poisson", data = d)
# estimated parameter
format(coef(C.1)[c(2)], digits = 3, scientific = F)
# confidence intervals
format(confint(C.1, "face_FA.s"), digits = 3, scientific = F)


# C.2
# the effect of facial FA on number of offspring, conditional on height
C.2.1 <- glm(No_offspring ~ 1 + face_FA.s + height.s, family="poisson", data = d)
format(coef(C.2.1)[2], digits = 3, scientific = F)
format(confint(C.2.1, "face_FA.s"), digits = 3, scientific = F)

# the effect of height on number of offspring
C.2.2 <- glm(No_offspring ~ height.s, family="poisson", data = d)
format(coef(C.2.2)[2], digits = 3, scientific = F)
format(confint(C.2.2, "height.s"), digits = 3, scientific = F)

# the effect of height on facial FA
C.2.3 <- glm(face_FA.s ~ height.s, family="gaussian", data = d)
format(coef(C.2.3)[2], digits = 3, scientific = F)
format(confint(C.2.3, "height.s"), digits = 3, scientific = F)


# C.3
# the effect of facial FA on number of offspring (direct)
C.3.1 <- glm(No_offspring ~ face_FA.s, family="poisson", data = d)
format(coef(C.3.1)[2], digits = 3, scientific = F)
format(confint(C.3.1, "face_FA.s"), digits = 3, scientific = F)

# the effect of facial FA on number of offspring (indirect)
C.3.2 <- glm(No_offspring ~ face_FA.s + height.s, family="poisson", data = d)
format(coef(C.3.2)[2], digits = 3, scientific = F)
format(confint(C.3.2, "face_FA.s"), digits = 3, scientific = F)

# the effect of height on face size
C.3.3 <- glm(face_size.s ~ height.s, family = "gaussian", data = d)
format(coef(C.3.3)[2], digits = 3, scientific = F)
format(confint(C.3.3, "height.s"), digits = 3, scientific = F)

# the effect of face size on facial FA
C.3.4 <- glm(face_FA.s ~ face_size.s + height.s, family = "gaussian", data = d)
format(coef(C.3.4)[2], digits = 3, scientific = F)
format(confint(C.3.4, "face_size.s"), digits = 3, scientific = F)



### SUMMARY RESULTS ####
models <- list(C.1, C.2.1, C.2.2, C.2.3,C.3.1, C.3.2, C.3.3, C.3.4)
eff_size <- lapply(models, function(x) coef(x)[2])
conf_int <- lapply(models, function(x) confint(x)[2,])
pred_var <- lapply(models, function(x) names(coef(x))[2])
resp_var <- lapply(models, function(x) as.character(x$formula)[2])
MODEL <- c("C.1","C.2.1","C.2.2","C.2.3","C.3.1","C.3.2","C.3.3","C.3.4")
Hypothesis <- c("Good genes",
                "Good development","Good development","Good development",
                "Growth","Growth","Growth","Growth")

results <- as.data.frame(cbind(Hypothesis,MODEL,
  as.data.frame(do.call(rbind, pred_var)),
  as.data.frame(do.call(rbind, resp_var)),
  as.data.frame(do.call(rbind, eff_size)),
  as.data.frame(do.call(rbind, conf_int))))

colnames(results) <- c("Hypothesis", "Regression_model","Predictor","Response", 
                       "Effect_size","CI_low", "CI_hi")

results$Regression_model <- factor(results$Regression_model, 
                             levels = rev(c("C.1", "C.2.1", "C.2.2", "C.2.3",
                                            "C.3.1", "C.3.2", "C.3.3", "C.3.4")))

## Result summary
a <- tableGrob(results)

## Plotting the effect size for each hypothesis and 95% CI
b <- ggplot(results, 
       aes(x=Regression_model, y=Effect_size, ymin=CI_low, ymax=CI_hi))+
  geom_pointrange()+
  geom_hline(yintercept = 0, linetype=2)+
  coord_flip()+ylim(c(-1,1))+
  xlab('Effect size and 95% CI')

grid.arrange(b,a)



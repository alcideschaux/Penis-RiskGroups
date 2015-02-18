# DATASET #
# Loading the full dataset with 333 patients
# The full dataset is available at http://files.figshare.com/1868801/PenisSCC_333.csv
Data <- read.csv("PenisSCC_333.csv")

# CASE SELECTION #
        # Selecting patients with total/partial penectomy
                Data <- subset(Data, Procedure == "Total penectomy" | Procedure == "Partial penectomy")
        # Excluding cases with missing values for histologic grade, pT stage and cN stage
                Data <- Data[complete.cases(Data$Grade), ]
                Data <- Data[complete.cases(Data$pT), ]
                Data <- Data[complete.cases(Data$cN), ]
        # Excluding patients who were lost at follow-up
                Data <- subset(Data, Outcome != "Lost at follow-up")
        # Dropping unused levels
                Data <- droplevels(Data)
# RECODING NEW VARIABLES #
        # Creating a new variable for cN positivity (>cN0)
                Data$cN_Positive <- ifelse(Data$cN == "cN0", c("No"), c("Yes"))
                Data$cN_Positive <- as.factor(Data$cN_Positive)
        # Creating a new variable for cancer-related death
                Data$DOD <- ifelse(Data$Outcome == "Died of cancer", c("Yes"), c("No"))
                Data$DOD <- as.factor(Data$DOD)
        # Creating a new variable for the final nodal status. Positive cases where those with:
                # Nodal metastasis in lymphadenectomy
                Data$Final_Nodal <- ifelse(Data$Mets == "Yes", c("Positive"), c("Negative"))
                # Local relapse during follow-up
                Data$Final_Nodal[Data$Local == "Yes"] <- "Positive"
                # Unfavorable outcome, including alive with disease and death by cancer
                Data$Final_Nodal[Data$Outcome == "Alive with disease"] <- "Positive"
                Data$Final_Nodal[Data$Outcome == "Died of cancer"] <- "Positive"
                Data$Final_Nodal <- as.factor(Data$Final_Nodal)

# RISK GROUPS #
# SOLSONA risk groups
Data$Solsona[Data$Grade == "Grade 1" & Data$pT == "T1"] <- "Low risk"
Data$Solsona[Data$Grade == "Grade 2" & Data$pT == "T1"] <- "Intermediate risk"
Data$Solsona[Data$Grade == "Grade 3" & Data$pT == "T1"] <- "Intermediate risk"
Data$Solsona[Data$Grade == "Grade 1" & Data$pT == "T2"] <- "Intermediate risk"
Data$Solsona[Data$Grade == "Grade 1" & Data$pT == "T3"] <- "Intermediate risk"
Data$Solsona[Data$Grade == "Grade 2" & Data$pT == "T2"] <- "High risk"
Data$Solsona[Data$Grade == "Grade 3" & Data$pT == "T2"] <- "High risk"
Data$Solsona[Data$Grade == "Grade 2" & Data$pT == "T3"] <- "High risk"
Data$Solsona[Data$Grade == "Grade 3" & Data$pT == "T3"] <- "High risk"
Data$Solsona <- factor(Data$Solsona, levels = c("Low risk", "Intermediate risk", "High risk"), ordered = TRUE)
# EAU risk groups
Data$EAU[Data$Grade == "Grade 1" & Data$pT == "T1"] <- "Low risk"
Data$EAU[Data$Grade == "Grade 2" & Data$pT == "T1"] <- "Intermediate risk"
Data$EAU[Data$Grade == "Grade 3" & Data$pT == "T1"] <- "High risk"
Data$EAU[Data$pT == "T2"] <- "High risk"
Data$EAU[Data$pT == "T3"] <- "High risk"
Data$EAU <- factor(Data$EAU, levels = c("Low risk", "Intermediate risk", "High risk"), ordered = TRUE)
# HUNGERHUBER risk groups
Data$Hungerhuber[Data$Grade == "Grade 1" & Data$pT == "T1"] <- "Low risk"
Data$Hungerhuber[Data$Grade == "Grade 2" & Data$pT == "T1"] <- "Low risk"
Data$Hungerhuber[Data$Grade == "Grade 1" & Data$pT == "T2"] <- "Intermediate risk"
Data$Hungerhuber[Data$Grade == "Grade 1" & Data$pT == "T3"] <- "Intermediate risk"
Data$Hungerhuber[Data$Grade == "Grade 2" & Data$pT == "T2"] <- "Intermediate risk"
Data$Hungerhuber[Data$Grade == "Grade 2" & Data$pT == "T3"] <- "Intermediate risk"
Data$Hungerhuber[Data$Grade == "Grade 3"] <- "High risk"
Data$Hungerhuber <- factor(Data$Hungerhuber, levels = c("Low risk", "Intermediate risk", "High risk"), ordered = TRUE)
# RESULTS #
# Function for univariate tables
        # Table sould be table(var), x refers to var level
        T1 <- function(Table, x){
                Count <- Table[x]
                Percentage <- round(100*prop.table(Table))
                paste(Count, " (", Percentage[x], "\\%)", sep = "")
        }
# Function for bivariate tables
        # Table should be table(var_x, var_y), x refers to var_x level, y refers to var_y level
        T2 <- function(Table, x, y){
                Percentage <- round(100*(prop.table(Table, 1)))
                paste(Table[x, y], "/", sum(Table[x, ]), " (", Percentage[x, y], "\\%", ")", sep = "")
        }
# Final Nodal Status
        # Proportion table for cN stages
                cN_Table <- table(Data$cN)
                cN_Prop <- round(100*prop.table(cN_Table))
        # Proportion table for final nodal status
                FN_Table <- table(Data$Final_Nodal)
                FN_Prop <- round(100*prop.table(FN_Table))
        # Proportion table of metastatic rates by HISTOLOGIC GRADE
                Grade_Table <- with(Data, table(Grade, Final_Nodal))
        # Proportion table of metastatic rates by pT STAGE
                pT_Table <- with(Data, table(pT, Final_Nodal))
        # Proportion table of metastatic rates for the SOLSONA system
                Solsona_Table <- table(Data$Solsona)
                Solsona_FN_Table <- with(Data, table(Solsona, Final_Nodal))
                Solsona_FN_cN0 <- with(subset(Data, cN_Positive == "No"), table(Solsona, Final_Nodal))
                Solsona_FN_cN123 <- with(subset(Data, cN_Positive == "Yes"), table(Solsona, Final_Nodal))
        # Proportion table of metastatic rates for the EAU system
                EAU_Table <- table(Data$EAU)
                EAU_FN_Table <- with(Data, table(EAU, Final_Nodal))
                EAU_FN_cN0 <- with(subset(Data, cN_Positive == "No"), table(EAU, Final_Nodal))
                EAU_FN_cN123 <- with(subset(Data, cN_Positive == "Yes"), table(EAU, Final_Nodal))
        # Proportion table of metastatic rates for the HUNGERHUBER system
                Hungerhuber_Table <- table(Data$Hungerhuber)
                Hungerhuber_FN_Table <- with(Data, table(Hungerhuber, Final_Nodal))
                Hungerhuber_FN_cN0 <- with(subset(Data, cN_Positive == "No"), table(Hungerhuber, Final_Nodal))
                Hungerhuber_FN_cN123 <- with(subset(Data, cN_Positive == "Yes"), table(Hungerhuber, Final_Nodal))
# Survival Analysis
# Defining the survival.plot function for plotting survival curves
library(survival)
survival.plot <- function(x, fu, outcome, title, position = "topright", logrank = "bottomleft", ...){
        outcome <- as.numeric(outcome)
        survival.obj <- Surv(fu, outcome)
        survival.lr <- survdiff(survival.obj ~ x)
        survival.p <- pchisq(survival.lr$chisq, df = 1, lower = FALSE)
        survival.x <- survfit(survival.obj ~ x)
        plot(survival.x, main = title, xlab = "", ylab = "",
                col =c(1,2,4), mark = c(2,0,5), lty = c(2,1,3), ...)
        legend(x = position, legend = levels(x), pch = c(2,0,5), lty = c(2,1,3),
                col = c(1,2,4), bty = "n")
        legend(x = logrank, bty = "n", 
                paste("P value (log-rank test) =", format(survival.p, digits = 2, width = 6)))
}
# ROC analysis
library(pROC)
# Final nodal status
ROC_FN_Solsona <- round(with(Data, roc(Final_Nodal ~ Solsona, ci = TRUE)$ci), 2)
MW_FN_Solsona <- format(with(Data, wilcox.test(as.numeric(Solsona) ~ Final_Nodal)$p.value), digits = 2)
ROC_FN_EAU <- round(with(Data, roc(Final_Nodal ~ EAU, ci = TRUE)$ci), 2)
MW_FN_EAU <- format(with(Data, wilcox.test(as.numeric(EAU) ~ Final_Nodal)$p.value), digits = 2)
ROC_FN_Hungerhuber <- round(with(Data, roc(Final_Nodal ~ Hungerhuber, ci = TRUE)$ci), 2)
MW_FN_Hungerhuber <- format(with(Data, wilcox.test(as.numeric(Hungerhuber) ~ Final_Nodal)$p.value), digits = 2)
# Cancer related death
ROC_DOD_Solsona <- round(with(Data, roc(DOD ~ Solsona, ci = TRUE)$ci), 2)
MW_DOD_Solsona <- format(with(Data, wilcox.test(as.numeric(Solsona) ~ DOD)$p.value), digits = 2)
ROC_DOD_EAU <- round(with(Data, roc(DOD ~ EAU, ci = TRUE)$ci), 2)
MW_DOD_EAU <- format(with(Data, wilcox.test(as.numeric(EAU) ~ DOD)$p.value), digits = 2)
ROC_DOD_Hungerhuber <- round(with(Data, roc(DOD ~ Hungerhuber, ci = TRUE)$ci), 2)
MW_DOD_Hungerhuber <- format(with(Data, wilcox.test(as.numeric(Hungerhuber) ~ DOD)$p.value), digits = 2)

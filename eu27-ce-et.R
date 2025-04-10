
##############################################################################################################
######### **** PSTR Estimation of with Finas and CCUs as transitioning variable***** ##############
#########                                                                                        ##############
##############################################################################################################


install.packages("PSTR")
library(PSTR)
library(PSTR)
version()

#To look at all the available functions and data in the package
ls( grep("PSTR", search()) ) 
#The modelling procedure consists of three stages: Specification,Estimation and Evaluation. The package offers tools helping the usersto conduct model specification tests, to do PSTR model estimation, and to do model evaluation.
#Upload data
library(readxl)
eu27 <-read_excel("eu27_ce_et.xlsx", 
                  col_types = c("numeric", "text", "date", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))
View(eu27)
print(eu27.head)

##############################################################################################################
######### **** Step1_ create a new object of the class PSTR for the EU and Regions***** ##############
#########                                                                                        ##############
##############################################################################################################

### For EU###

pstr = NewPSTR(eu27, dep='LnET', indep=5:11, indep_k=c('LnURP','LnREIT','LnFins','LnCCUS'),
                tvars=c('LnFins','LnCCUS'), im=2, iT=27)
print(pstr)
##by region
### Step 1: Filter the dataset to include only regionid == 1, 2,3,4,

region4 <- subset(eu27, regionid == 4) # For Western Europe
region3 <- subset(eu27, regionid == 3)  #for Southern Europe
region2 <- subset(eu27, regionid == 2) #for Northern Europe
region1 <- subset(eu27, regionid == 1) # ForEastern Europe
# Step 2: Apply the NewPSTR model to the filtered data
region4 <- NewPSTR(
  region4, 
  dep = 'LnET',                # Dependent variable
  indep = 5:11,                # Independent variables (assuming columns 5 to 11)
  indep_k = c('LnURP', 'LnREIT', 'LnFins', 'LnCCUS'),  # Specific key independent variables
  tvars = c('LnFins', 'LnCCUS'),                      # Transition variables
  im = 2,                      # Number of transition functions
  iT = 6                       # Time period number of economies
)


region3 <- NewPSTR(
  region3, 
  dep = 'LnET',                # Dependent variable
  indep = 5:11,                # Independent variables (assuming columns 5 to 11)
  indep_k = c('LnURP', 'LnREIT', 'LnFins', 'LnCCUS'),  # Specific key independent variables
  tvars = c('LnFins', 'LnCCUS'),                      # Transition variables
  im = 2,                      # Number of transition functions
  iT = 8                     # Time period
)

region2 <- NewPSTR(
  region2, 
  dep = 'LnET',                # Dependent variable
  indep = 5:11,                # Independent variables (assuming columns 5 to 11)
  indep_k = c('LnURP', 'LnREIT', 'LnFins', 'LnCCUS'),  # Specific key independent variables
  tvars = c('LnFins', 'LnCCUS'),                      # Transition variables
  im = 2,                      # Number of transition functions
  iT = 7                     # Time period
)
region1 <- NewPSTR(
  region1, 
  dep = 'LnET',                # Dependent variable
  indep = 5:11,                # Independent variables (assuming columns 5 to 11)
  indep_k = c('LnURP', 'LnREIT', 'LnFins', 'LnCCUS'),  # Specific key independent variables
  tvars = c('LnFins', 'LnCCUS'),                      # Transition variables
  im = 2,                      # Number of transition functions
  iT = 6                     # Time period
)

##############################################################################################################
######### **** Step2_ linearity tests  for selection of transitioning variable and number of switches***** ##############
#########          Table A6                                                                              ##############
##############################################################################################################


LinTest_pstr_1 = LinTest(use=pstr) 
print(LinTest_pstr_1, "tests")

LinTest_region1_1 = LinTest(use=region1) 
print(LinTest_region1_1, "tests")

LinTest_region2_1 = LinTest(use=region2) 
print(LinTest_region2_1, "tests")

LinTest_region3_1 = LinTest(use=region3) 
print(LinTest_region3_1, "tests")

LinTest_region4_1 = LinTest(use=region4) 
print(LinTest_region4_1, "tests")



##############################################################################################################
######### **** Step3_ Estimation of PSTR model with selected  transitioning variable and number of switches***** ##############
#########      Table 5:                                                                                ##############
##############################################################################################################

#To determine which transition variable to use for the estimation, in this case 


pstr_Fins = EstPSTR(use=pstr,im=1,iq='LnFins',useDelta=T,par=c(-0.462,0), vLower=4, vUpper=4, method="CG")
print(pstr_Fins,"estimates")


pstr2_CCUS = EstPSTR(use=pstr,im=1,iq='LnCCUS',useDelta=T,par=c(-0.462,0), method="CG")
print(pstr2_CCUS,"estimates")


region1_CCUS = EstPSTR(use=region1,im=1,iq='LnCCUS',useDelta=T,par=c(-0.462,0), method="CG")
print(region1_CCUS,"estimates")

region2_Fins = EstPSTR(use=region2,im=1,iq='LnFins',useDelta=T,par=c(-0.462,0), method="CG")
print(region2_Fins,"estimates")


region3_Fins = EstPSTR(use=region3,im=1,iq='LnFins',useDelta=T,par=c(-0.462,0), method="CG")
print(region3_Fins,"estimates")

region4_Fins = EstPSTR(use=region4,im=1,iq='LnFins',useDelta=T,par=c(-0.462,0), method="CG")
print(region4_Fins,"estimates")



##############################################################################################################
######### **** Step4_ Evaluation tests***** ##############
#########   Table A8:                                                                                      ##############
##############################################################################################################

#These functions conduct the evaluation tests against two alternatives: 1. the parameters
#are time-varying and 2. there is remaining nonlinearity (remaining heterogeneity).


## evaluatio tests
pstr_Fins
Eve_pstr_Fins = EvalTest(use=pstr_Fins,vq=pstr_Fins$mQ[,1])
print(Eve_pstr_Fins, "eval")

Eve_pstr_CCUS = EvalTest(use=pstr2_CCUS,vq=pstr2_CCUS$mQ[,1])
print(Eve_pstr_CCUS, "eval")

Eve_region1 = EvalTest(use=region1_CCUS,vq=region1_CCUS$mQ[,1])
print(Eve_region1, "eval")

Eve_region2 = EvalTest(use=region2_Fins,vq=region2_Fins$mQ[,1])
print(Eve_region2, "eval")

Eve_region3 = EvalTest(use=region3_Fins,vq=region3_Fins$mQ[,1])
print(Eve_region3, "eval")

Eve_region4 = EvalTest(use=region4_Fins,vq=region4_Fins$mQ[,1])
print(Eve_region4, "eval")


##############################################################################################################
######### **** #Plotting***** ##############
#########         Figure 5:                                                                                ##############
##############################################################################################################
library(ggplot2)
library(gridExtra)
# # Set up the TIFF device for saving the plots
tiff("finalPSTR_modeles2.tiff", width = 12, height = 8, units = "in", res = 300, compression = "lzw")
plot_Fins <-plot_transition(pstr_Fins, fill = 'blue', xlim = c(-2, 20), color = "dodgerblue4", size = 2, alpha = .3) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = pstr_Fins$c - log(1 / 0.95 - 1) / pstr_Fins$gamma), color = 'blue') +
  ggplot2::labs(x = "Transition variable-financial structure", y = "Transition function",
                caption = "PSTR models estimated transition function based on Fins for the EU.")

plot_CCUS <- plot_transition(pstr2_CCUS, fill = 'blue', xlim = c(-2, 20), color = "dodgerblue4", size = 2, alpha = .3) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = pstr2_CCUS$c - log(1 / 0.95 - 1) / pstr2_CCUS$gamma), color = 'blue') +
  ggplot2::labs(x = "Transition variable-circular carbon technology innovation", y = "Transition function",
                caption = "PSTR models estimated transition function based on CCUS for the EU.")

plot_Eastern <- plot_transition(region1_CCUS, fill = 'blue', xlim = c(-2, 20), color = "dodgerblue4", size = 2, alpha = .3) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = region1_CCUS$c - log(1 / 0.95 - 1) / region1_CCUS$gamma), color = 'blue') +
  ggplot2::labs(x = "Transition variable-circular carbon technology innovation", y = "Transition function",
                caption = "PSTR models estimated transition function based on CCUS for Eastern Europe.")

plot_Northern <- plot_transition(region2_Fins, fill = 'blue', xlim = c(-2, 20), color = "dodgerblue4", size = 2, alpha = .3) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = region2_Fins$c - log(1 / 0.95 - 1) / region2_Fins$gamma), color = 'blue') +
  ggplot2::labs(x = "Transition variable-financial structure", y = "Transition function",
                caption = "PSTR models estimated transition function based on Fins for Northern Europe.")

plot_Southern <-plot_transition(region3_Fins, fill = 'blue', xlim = c(-2, 20), color = "dodgerblue4", size = 2, alpha = .3) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = region3_Fins$c - log(1 / 0.95 - 1) / region3_Fins$gamma), color = 'blue') +
  ggplot2::labs(x = "Transition variable-financial structure", y = "Transition function",
                caption = "PSTR models estimated transition function based on Fins for Southern Europe.")

plot_Western <- plot_transition(region4_Fins, fill = 'blue', xlim = c(-2, 20), color = "dodgerblue4", size = 2, alpha = .3) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = region4_Fins$c - log(1 / 0.95 - 1) / region4_Fins$gamma), color = 'blue') +
  ggplot2::labs(x = "Transition variable-financial structure", y = "Transition function",
                caption = "PSTR models estimated transition function based on Fins for Western Europe.")

# Arrange the two plots side by side using gridExtra::grid.arrange()
grid.arrange(plot_Fins, plot_CCUS,plot_Eastern, plot_Northern, plot_Southern, plot_Western, ncol = 2)

# Close the device to save the file
dev.off()
##############################################################################################################

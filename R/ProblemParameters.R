#----------Weibul_Exponential_MLE------------#START
#tets_data <- matrix(c(0.043721461,383,0.044863014,393,0.046004566,403,0.047146119,413,0.048287671,423), nrow = 5, ncol = 2, byrow = TRUE)
#----------Weibul_Exponential_MLE------------#START

#--Resilient and sustainable operations network design--START

#reading tables
ZONE_DATA <- read.csv("Data/Zone_Data.csv", header = TRUE, sep = ";", dec = ",")
SUPPLIERS_DATA <- read.csv("Data/Suppliers_Data.csv", header = TRUE, sep = ";", dec = ",")
INSTALLATIONS_DATA <- read.csv("Data/Installations_Data.csv", header = TRUE, sep = ";", dec = ",")
CUSTOMERS_DATA <- read.csv("Data/Customers_Data.csv", header = TRUE, sep = ";", dec = ",")
SM_DATA <- read.csv("Data/SM_Data.csv", header = TRUE, sep = ";", dec = ",")
MW_DATA <- read.csv("Data/MW_Data.csv", header = TRUE, sep = ";", dec = ",")
WC_DATA <- read.csv("Data/WC_Data.csv", header = TRUE, sep = ";", dec = ",")
SCENARIO_DATA <- read.csv("Data/Scenario_Data.csv", header = TRUE, sep = ";", dec = ",")
RR_DATA <- read.csv("Data/RecoveryResource_Data.csv", header = TRUE, sep = ";", dec = ",")

#Zones_Data
ZONES <- ZONE_DATA$X.Province[!is.na(ZONE_DATA$X.Province)]
UR <- ZONE_DATA$UR[!is.na(ZONE_DATA$UR)]
GDPI <- ZONE_DATA$GDPI[!is.na(ZONE_DATA$GDPI)]
Y <- ZONE_DATA$Yz[!is.na(ZONE_DATA$Yz)]

#Suppliers_Data
S <- length(SUPPLIERS_DATA$Supplier[!is.na(SUPPLIERS_DATA$Supplier)])
SUPPLIERS_CAPACITIES <- SUPPLIERS_DATA$Capacity[!is.na(SUPPLIERS_DATA$Capacity)]
PURCHASE_COSTS <- SUPPLIERS_DATA$Purchase.Costs[!is.na(SUPPLIERS_DATA$Purchase.Costs)]
FOOTPRINT_EMISSIONS <- SUPPLIERS_DATA$Footprint.Emissions[!is.na(SUPPLIERS_DATA$Footprint.Emissions)]
SUPPLIERS_ZONES <- SUPPLIERS_DATA$Province.Zone[!is.na(SUPPLIERS_DATA$Province.Zone)]
LJR_S <- SUPPLIERS_DATA$LJR[!is.na(SUPPLIERS_DATA$LJR)]

#Installations_Data
M <- 0
for(j in 1:length(INSTALLATIONS_DATA$Installation)) if(INSTALLATIONS_DATA$Type[j] == "Manufacturing") M <- M + 1
W <- length(INSTALLATIONS_DATA$Installation) - M
M1 <- 0
W1 <- 0
OF_INITIAL <- c(rep(1, M+W))
for(j in 1:M) if(INSTALLATIONS_DATA$Classification[j] == "New"){ 
  M1 <- M1 + 1
  OF_INITIAL[j] = 0
  }
for(j in (M+1):(M+W)) if(INSTALLATIONS_DATA$Classification[j] == "New"){
  W1 <- W1 + 1
  OF_INITIAL[j] = 0
}
OF_INVESTMENTS <- INSTALLATIONS_DATA$Open.Facilities.Investments[!is.na(INSTALLATIONS_DATA$Open.Facilities.Investments)]
INITIAL_CAPACITIES <- INSTALLATIONS_DATA$Initial.Capacities[!is.na(INSTALLATIONS_DATA$Initial.Capacities)]
MIN_ADDITIONAL_CAPACITIES <- INSTALLATIONS_DATA$Minimum.Additional.Capacities[!is.na(INSTALLATIONS_DATA$Minimum.Additional.Capacities)]
MAX_ADDITIONAL_CAPACITIES <- INSTALLATIONS_DATA$Maximum.Additional.Capacities[!is.na(INSTALLATIONS_DATA$Maximum.Additional.Capacities)]
AC_INVESTMENTS <- INSTALLATIONS_DATA$Additional.Capacities.Investments[!is.na(INSTALLATIONS_DATA$Additional.Capacities.Investments)]
FIXED_COSTS <- INSTALLATIONS_DATA$Fixed.Costs[!is.na(INSTALLATIONS_DATA$Fixed.Costs)]
REQUIRED_RESOURCES_TO_RECOVERING <- INSTALLATIONS_DATA$Required.Resources.To.Recovering[!is.na(INSTALLATIONS_DATA$Required.Resources.To.Recovering)]
INSTALLATIONS_ZONES <- INSTALLATIONS_DATA$Province.Zone[!is.na(INSTALLATIONS_DATA$Province.Zone)]
MANUFACTURING_COSTS <- INSTALLATIONS_DATA$Manufacturing.Costs[!is.na(INSTALLATIONS_DATA$Manufacturing.Costs)] #Only Manufactures
MANUFACTURING_EMISSIONS <- INSTALLATIONS_DATA$Manufacturing.Emissions[!is.na(INSTALLATIONS_DATA$Manufacturing.Emissions)] #Only Manufactures
LJR_M <- INSTALLATIONS_DATA$LJR[1:M]
LJR_W <- INSTALLATIONS_DATA$LJR[(M+1):(M+W)]

#Customers_Data
C <- length(CUSTOMERS_DATA$Customer[!is.na(CUSTOMERS_DATA$Customer)])
DEMANDS <- CUSTOMERS_DATA$Demands[!is.na(CUSTOMERS_DATA$Demands)]
REVENUES <- CUSTOMERS_DATA$Revenues[!is.na(CUSTOMERS_DATA$Revenues)]
PENALIZATION_COSTS <- CUSTOMERS_DATA$Penalization.Costs[!is.na(CUSTOMERS_DATA$Penalization.Costs)]
CUSTOMERS_ZONES <- CUSTOMERS_DATA$Province.Zone[!is.na(CUSTOMERS_DATA$Province.Zone)]

#SM_Data
TRANSPORT_COSTS_SM <- matrix(SM_DATA$Transport.Costs[!is.na(SM_DATA$Transport.Costs)], nrow = S, ncol = M, byrow = TRUE)
TRANSPORT_EMISSIONS_SM <- matrix(SM_DATA$Transport.Emissions[!is.na(SM_DATA$Transport.Emissions)], nrow = S, ncol = M, byrow = TRUE)
HANDLING_COSTS_SM <- matrix(SM_DATA$Handling.Costs[!is.na(SM_DATA$Handling.Costs)], nrow = S, ncol = M, byrow = TRUE)

#MW_Data
TRANSPORT_COSTS_MW <- matrix(MW_DATA$Transport.Costs[!is.na(MW_DATA$Transport.Costs)], nrow = M, ncol = W, byrow = TRUE)
TRANSPORT_EMISSIONS_MW <- matrix(MW_DATA$Transport.Emissions[!is.na(MW_DATA$Transport.Emissions)], nrow = M, ncol = W, byrow = TRUE)
HANDLING_COSTS_MW <- matrix(MW_DATA$Handling.Costs[!is.na(MW_DATA$Handling.Costs)], nrow = M, ncol = W, byrow = TRUE)

#WC_Data
TRANSPORT_COSTS_WC <- matrix(WC_DATA$Transport.Costs[!is.na(WC_DATA$Transport.Costs)], nrow = W, ncol = C, byrow = TRUE)
TRANSPORT_EMISSIONS_WC <- matrix(WC_DATA$Transport.Emissions[!is.na(WC_DATA$Transport.Emissions)], nrow = W, ncol = C, byrow = TRUE)
HANDLING_COSTS_WC <- matrix(WC_DATA$Handling.Costs[!is.na(WC_DATA$Handling.Costs)], nrow = W, ncol = C, byrow = TRUE)

#Scenario_Data
SCENARIO <- length(SCENARIO_DATA$X.Scenario[!is.na(SCENARIO_DATA$X.Scenario)])
PROB_SCENARIO <- SCENARIO_DATA$Scenario.Probability[!is.na(SCENARIO_DATA$Scenario.Probability)]
IMPACT <- matrix(as.vector(as.matrix(SCENARIO_DATA[,-c(1,2)])), nrow=M+W, ncol=SCENARIO, byrow = TRUE)
N <- 3

#RR_Data
AR_INVESTMENT <- RR_DATA$Additional.Resource.Investment[!is.na(RR_DATA$Additional.Resource.Investment)]
USED_RESOURCE_COST <- RR_DATA$Used.Resource.Cost[!is.na(RR_DATA$Used.Resource.Cost)]
MAINTAINED_RESOURCE_COST <- RR_DATA$Maintained.Resource.Cost[!is.na(RR_DATA$Maintained.Resource.Cost)]
INITIAL_RESOURCE <- RR_DATA$Initial.Resource[!is.na(RR_DATA$Initial.Resource)]
MAX_ADDITIONAL_RESOURCE <- RR_DATA$Maximum.Additional.Resource[!is.na(RR_DATA$Maximum.Additional.Resource)]

#--Resilient and sustainable operations network design--END

'
#--Replacement policy for systems subjected to imperfect repairs--STAR 

#Model Parameters
H<-43800; c1_cost<-40000; c2_cost<-65000; p1_cost<-20000; p2_cost<-45000
s_cost<-8000;g_cost<-0; r_time<-40; c1_time<-152; c2_time<-224
p1_time<-131; p2_time<-203; budget<-150000; u_max<-0.1

#GRP Parameters (non-critical failure)
alpha <- 1828;  beta <- 2.026;  q <- 0.7

#Weibull Parameters (critical failure)
alpha1 <- 7760;  beta1 <- 1.62

#--Replacement policy for systems subjected to imperfect repairs--END'
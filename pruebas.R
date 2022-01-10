#pruebas
#Los datos fueron tomados de Mayo Clinic entre 1974 y 1984.
# 1) ID: unique identifier
# 2) N_Days: number of days between registration and the earlier of death, transplantation, or study analysis time in July 1986
# 3) Status: status of the patient C (censored), CL (censored due to liver tx), or D (death)
# 4) Drug: type of drug D-penicillamine or placebo
# 5) Age: age in [days]
# 6) Sex: M (male) or F (female)
# 7) Ascites: presence of ascites N (No) or Y (Yes)
# 8) Hepatomegaly: presence of hepatomegaly N (No) or Y (Yes)
# 9) Spiders: presence of spiders N (No) or Y (Yes)
# 10) Edema: presence of edema N (no edema and no diuretic therapy for edema), S (edema present without diuretics, or edema resolved by diuretics), or Y (edema despite diuretic therapy)
# 11) Bilirubin: serum bilirubin in [mg/dl]
# 12) Cholesterol: serum cholesterol in [mg/dl]
# 13) Albumin: albumin in [gm/dl]
# 14) Copper: urine copper in [ug/day]
# 15) Alk_Phos: alkaline phosphatase in [U/liter]
# 16) SGOT: SGOT in [U/ml]
# 17) Triglycerides: triglicerides in [mg/dl]
# 18) Platelets: platelets per cubic [ml/1000]
# 19) Prothrombin: prothrombin time in seconds [s]
# 20) Stage: histologic stage of disease (1, 2, 3, or 4)

setwd("~/Septimo/Estadistica 3/Survival_Analysis")
data=read.csv(file = "cirrhosis.csv")
View(data)
head(data)
tail(data) #observamos que tenemos datos qur faltan en algunas covariables
length(data$ID) #tenemos 418 pacientes 
#nos es de interes el tiempo de supervivencia de cada paciente
#tenemos datos que no nos sirven entonces vamos a limpiar la base 
colnames(data)

new_data= data.frame(data$ID,data$Sex,data$N_Days/365,ata$Status,data$Age ) 
View(new_data)

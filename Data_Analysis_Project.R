
library(naniar)
library(readxl)
library(haven)
library(plyr)
library(tidyverse)
library(cdlTools)
library(ggplot2)
library(ggthemes)
library(htmltools)
library(ggmap)
library(kableExtra)
library(knitr)
library(vtable)
library(modelsummary)
library(viridisLite)
library(viridis)
library(plotly)
library(car)
library(gt)

# Importing Federal Home Loan 2010 Files

setwd("C:/Users/achar/Desktop/R HW Problems")

FHL2010_1 <- read_excel("2010_Part1.xlsx")

dim(FHL2010_1)

FHL2010_2 <- read_excel("2010_Part2.xlsx")

dim(FHL2010_2)


# Getting rid of unnecessary variables in the second dataset for 2010
# so that duplicates aren't created when merging.

FHL2010_2 <- select(FHL2010_2, -c(Year, FHLBankID, Program, FIPSStateCode, FIPSCountyCode,
          MSA, FeatureID))

# Merging the FHL 2010 datasets 

FHL2010 <- merge(FHL2010_1, FHL2010_2, by = "Loan Number")


# Creating Analytic Dataset with relevant variables 
# for 2010 

FHL2010 <- FHL2010 %>% 
  select(Year, FHLBankID, Tract, FIPSStateCode, FIPSCountyCode,
         MinPer, TraMedY, CurAreY, LocMedY, LTV, Purpose, FedGuar, Front,
         Back, CoRace, BoRace, CoGender, BoGender, Occup, 
        `Borrower Credit Score`,`Co-Borrower Credit Score`, 
         Self, HOEPA, Amount, Rate, PropType, NumUnits, 
         Income, First, BoAge, CoAge, MortDate, AcquDate,
        NumBor)


# Importing Federal Home Loan 2015 Files

FHL2015_1 <- read_excel("2015_Part1.xlsx")

dim(FHL2015_1)

FHL2015_2 <- read_excel("2015_Part2.xlsx")

dim(FHL2015_2)


# Getting rid of unnecessary variables in the second dataset for 2015
# so that duplicates aren't created when merging.

FHL2015_2 <- select(FHL2015_2, -c(Year, FHLBank, FIPSStateCode,
                                  FIPSCountyCode, MSA, FeatureID))


# Merging the FHL 2015 datasets

FHL2015 <- merge(FHL2015_1, FHL2015_2, by = "AssignedID")


# Creating Analytic Dataset with relevant variables for 2015

FHL2015 <- FHL2015 %>% 
  select(Year, FHLBank,Tract, FIPSStateCode, FIPSCountyCode,
         MinPer, TraMedY, CurAreY, LocMedY, LTV, Purpose,
         FedGuar, Front, Back, CoRace, BoRace, CoGender,
         BoGender, Occup, BoCreditScor, CoCreditScor, Self,
         HOEPA, Amount, Rate, PropType, NumUnits,
         Income, First, BoAge, CoAge, MortDate, AcqDate,
         NumBor)


# Importing Federal Home Loan 2020 Files

FHL2020 <- read_excel("2020_Dataset.xlsx")

dim(FHL2020)

FHL2020 <- FHL2020 %>% 
  select(Year,Bank, CensusTractIdentifier,FIPSStateNumericCode, 
         FIPSCountyCode,CensusTractMinorityRatioPercent
         ,CensusTractMedFamIncomeAmount, HUDMedianIncomeAmount,
         LocalAreaMedianIncomeAmount, LTVRatioPercent,LoanPurposeType,
         MortgageType, HousingExpenseRatioPercent, TotalDebtExpenseRatioPercent,
         Borrower2Race1Type, Borrower1Race1Type, Borrower2GenderType,
         Borrower1GenderType,PropertyUsageType, Borrower1CreditScoreValue,
         Borrower2CreditScoreValue, EmploymentBorrowerSelfEmployed,
         HOEPALoanStatusType, NoteAmount, NoteRatePercent, PropertyType,
         PropertyUnitCount, TotalMonthlyIncomeAmount, BorrowerFirstTimeHomebuyer,
         Borrower1AgeAtApplicationYears, Borrower2AgeAtApplicationYears,
         NoteDate, LoanAcquistionDate, BorrowerCount)


# Renaming variables to be consistent in each dataset for the years.

FHL2020 <- FHL2020 %>% 
  rename(FHLBankDistrict = Bank,
         FIPSStateCode = FIPSStateNumericCode,
         CoBorrowerRace = Borrower2Race1Type,
         BorrowerRace = Borrower1Race1Type,
         CoBorrowerGender = Borrower2GenderType,
         BorrowerGender = Borrower1GenderType,
         BorrowerCreditScore = Borrower1CreditScoreValue,
         CoBorrowerCreditScore = Borrower2CreditScoreValue,
         BorrowerSelfEmployed = EmploymentBorrowerSelfEmployed,
         BorrowerAge = Borrower1AgeAtApplicationYears,
         CoBorrowerAge = Borrower2AgeAtApplicationYears,
         MortgageOriginated = NoteDate,
         MortgageAcquired = LoanAcquistionDate)


FHL2015 <- FHL2015 %>% 
  rename(FHLBankDistrict = FHLBank,
         CensusTractIdentifier = Tract,
         CensusTractMinorityRatioPercent = MinPer,
         CensusTractMedFamIncomeAmount = TraMedY,
         HUDMedianIncomeAmount = CurAreY,
         LocalAreaMedianIncomeAmount = LocMedY,
         LTVRatioPercent = LTV,
         LoanPurposeType = Purpose,
         MortgageType = FedGuar,
         HousingExpenseRatioPercent = Front,
         TotalDebtExpenseRatioPercent = Back,
         CoBorrowerRace = CoRace,
         BorrowerRace = BoRace,
         CoBorrowerGender = CoGender,
         BorrowerGender = BoGender,
         PropertyUsageType = Occup,
         BorrowerCreditScore = BoCreditScor,
         CoBorrowerCreditScore = CoCreditScor,
         BorrowerSelfEmployed = Self,
         HOEPALoanStatusType = HOEPA,
         NoteAmount = Amount,
         NoteRatePercent = Rate,
         PropertyType = PropType,
         PropertyUnitCount = NumUnits,
         TotalMonthlyIncomeAmount = Income,
         BorrowerFirstTimeHomebuyer = First,
         BorrowerAge = BoAge,
         CoBorrowerAge = CoAge,
         MortgageOriginated = MortDate,
         MortgageAcquired = AcqDate,
         BorrowerCount = NumBor)

FHL2010 <- FHL2010 %>% 
  rename(FHLBankDistrict = FHLBankID,
         CensusTractIdentifier = Tract,
         CensusTractMinorityRatioPercent = MinPer,
         CensusTractMedFamIncomeAmount = TraMedY,
         HUDMedianIncomeAmount = CurAreY,
         LocalAreaMedianIncomeAmount = LocMedY,
         LTVRatioPercent = LTV,
         LoanPurposeType = Purpose,
         MortgageType = FedGuar,
         HousingExpenseRatioPercent = Front,
         TotalDebtExpenseRatioPercent = Back,
         CoBorrowerRace = CoRace,
         BorrowerRace = BoRace,
         CoBorrowerGender = CoGender,
         BorrowerGender = BoGender,
         PropertyUsageType = Occup,
         BorrowerCreditScore = `Borrower Credit Score`,
         CoBorrowerCreditScore = `Co-Borrower Credit Score`,
         BorrowerSelfEmployed = Self,
         HOEPALoanStatusType = HOEPA,
         NoteAmount = Amount,
         NoteRatePercent = Rate,
         PropertyType = PropType,
         PropertyUnitCount = NumUnits,
         TotalMonthlyIncomeAmount = Income,
         BorrowerFirstTimeHomebuyer = First,
         BorrowerAge = BoAge,
         CoBorrowerAge = CoAge,
         MortgageOriginated = MortDate,
         MortgageAcquired = AcquDate,
         BorrowerCount = NumBor)
  

# Converting/Transforming 2010 and 2015 variables for consistency across
# all datasets for each year


FHL2010 <- FHL2010 %>% 
  mutate(LTVRatioPercent = LTVRatioPercent * 100,
         HousingExpenseRatioPercent = HousingExpenseRatioPercent * 100,
         TotalDebtExpenseRatioPercent = TotalDebtExpenseRatioPercent * 100,
         NoteRatePercent = NoteRatePercent * 100)


FHL2015 <- FHL2015 %>% 
  mutate(LTVRatioPercent = LTVRatioPercent * 100,
         HousingExpenseRatioPercent = HousingExpenseRatioPercent * 100,
         TotalDebtExpenseRatioPercent = TotalDebtExpenseRatioPercent * 100,
         NoteRatePercent = NoteRatePercent * 100)


FHL2010$TotalMonthlyIncomeAmount <- round((FHL2010$TotalMonthlyIncomeAmount/12), digits = 0)

FHL2015$TotalMonthlyIncomeAmount <- round((FHL2015$TotalMonthlyIncomeAmount/12), digits = 0)


# Appending the datasets for each year

FHLAppend <- rbind(FHL2010, FHL2015, FHL2020)

dim(FHLAppend)

# Creating additional dummy variables 

FHLAppend$BorrowerSelfEmployed <- ifelse(FHLAppend$BorrowerSelfEmployed == 1, 1,0)

FHLAppend$BorrowerFirstTimeHomebuyer <- ifelse(FHLAppend$BorrowerFirstTimeHomebuyer == 1, 1,0)

FHLAppend <- FHLAppend %>% 
  mutate(WhiteBorrower = ifelse(BorrowerRace == 5, 1,0)) %>% 
  mutate(WhiteCoBorrower = ifelse(CoBorrowerRace == 5, 1,0)) %>%
  mutate(FemaleBorrower = ifelse(BorrowerGender == 2, 1,0)) %>% 
  mutate(FemaleCoBorrower = ifelse(CoBorrowerGender == 2, 1,0)) %>%
  mutate(BlackBorrower = ifelse(BorrowerRace == 3, 1,0)) %>% 
  mutate(BlackCoBorrower = ifelse(CoBorrowerRace == 3, 1,0)) %>% 
  mutate(Borrower620to660 = ifelse(BorrowerCreditScore == 2, 1,0)) %>% 
  mutate(Borrower660to700 = ifelse(BorrowerCreditScore == 3, 1,0)) %>% 
  mutate(Borrower700to760 = ifelse(BorrowerCreditScore == 4, 1,0)) %>% 
  mutate(Borrower760orabove = ifelse(BorrowerCreditScore == 5, 1,0)) %>% 
  mutate(CoBorrower620to660 = ifelse(CoBorrowerCreditScore == 2, 1,0)) %>% 
  mutate(CoBorrower660to700 = ifelse(CoBorrowerCreditScore == 3, 1,0)) %>% 
  mutate(CoBorrower700to760 = ifelse(CoBorrowerCreditScore == 4, 1,0)) %>% 
  mutate(CoBorrower760orabove = ifelse(CoBorrowerCreditScore == 5, 1,0))


View(FHLAppend)

# Creating Interaction variables 

FHLAppend <- FHLAppend %>% 
  mutate(SelfEmployedWhiteBorrower = BorrowerSelfEmployed * WhiteBorrower) %>% 
  mutate(SelfEmployedFemaleBorrower = BorrowerSelfEmployed * FemaleBorrower) %>% 
  mutate(SelfEmployedBlackBorrower  = BorrowerSelfEmployed * BlackBorrower)



# Only looking at 1 or 2 total Borrowers for loan

FHLAppend <- FHLAppend %>% 
  filter(BorrowerCount <= 2)


# Only looking at female and male borrowers

FHLAppend <- FHLAppend %>% 
  filter(BorrowerGender %in% c(1,2))



# Replacing categorical numerical values with NA values to keep relevant data

FHLAppend <- FHLAppend %>% 
  replace_with_na(replace = list(BorrowerAge = c(99,999,998),
                                 CoBorrowerAge = c(999,99,998,98)))


# Getting rid of outliers for Housing Expense Ratio

quartiles_houseexp <- quantile(FHLAppend$HousingExpenseRatioPercent,
                              probs = c(.25,.75),
                              na.rm = F)

IQR_houseexp <- IQR(FHLAppend$HousingExpenseRatioPercent)


Lower_houseexp <- quartiles_houseexp[1] - 1.5*IQR_houseexp
Upper_houseexp <- quartiles_houseexp[2] + 1.5*IQR_houseexp


FHLAppend <- subset(FHLAppend,FHLAppend$HousingExpenseRatioPercent > Lower_houseexp
                    & FHLAppend$HousingExpenseRatioPercent < Upper_houseexp)

dim(FHLAppend)



# Visualization (Graphs, Figure, Charts, etc.)

# Using a Sample of the dataset to create scatter plots

FHLAppend_Sample <- sample_n(FHLAppend, 10000)


View(FHLAppend_Sample)

# Creating a Scatter Plot of LTV vs. Note Amount for 
# White and Non-White Borrowers


Scatter1 <-  FHLAppend_Sample %>% 
  mutate(WhiteBorrower = dplyr::recode(WhiteBorrower, `1` = "WhiteBorrower",
                                `0` = "NonWhiteBorrower")) %>% 
  rename(Race = WhiteBorrower) %>% 
  ggplot(aes(x = LTVRatioPercent,
             y = NoteAmount))+
  geom_point(shape = "square",
             size = 2,
             alpha = .4,
             color = "red")+
  geom_smooth(method = "lm", se = F)+
  labs(title = "Figure 2: Comparison of LTV & Note Amount by Borrower's Race",
       caption = "Correlation Coefficient (White Borrowers) = 0.095
       Correlation Coefficient (Non White Borrowers) = -0.11 \nThis Scatter Plot was created using a random sample of 
       10000 observations from the FHLAppend dataset.")+
  facet_wrap(~Race)+
  scale_y_continuous(labels = function(y) format(y,
                                                 scientific = F))+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        plot.caption = element_text(size = 10, face  = "italic"))

Scatter1

# Comparing Correlation Coefficients of White and Non White Borrowers

FHLAppendWhite <- FHLAppend_Sample %>% 
  filter(WhiteBorrower == 1 )


cor(FHLAppendWhite$LTVRatioPercent, FHLAppendWhite$NoteAmount)


FHLAppendNonWhite <- FHLAppend_Sample %>% 
  filter(WhiteBorrower == 0)

cor(FHLAppendNonWhite$LTVRatioPercent, FHLAppendNonWhite$NoteAmount)


# Creating a Scatter Plot of LTV vs. Note Amount for Female and
# Male Borrowers

Scatter2 <-  FHLAppend_Sample %>% 
  mutate(FemaleBorrower = dplyr::recode(FemaleBorrower, `1` = "Female Borrowers",
                                       `0` = "Male Borrowers")) %>% 
  rename(Gender = FemaleBorrower) %>% 
  ggplot(aes(x = LTVRatioPercent,
             y = NoteAmount))+
  geom_point(shape = "square",
             size = 2,
             alpha = .4,
             color = "royalblue3")+
  geom_smooth(method = "lm", se = F, color = "red")+
  labs(title = "Figure 3: Comparison of LTV & Note Amount by Borrower's Race",
       caption = "Correlation Coefficient (Female Borrowers) = 0.052
       Correlation Coefficient (Male Borrowers) = 0.096 \nThis Scatter Plot was created using a random sample of 
       10000 observations from the FHLAppend dataset.")+
  facet_wrap(~Gender)+
  scale_y_continuous(labels = function(y) format(y,
                                                 scientific = F))+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        plot.caption = element_text(size = 10, face = "italic"))

Scatter2


# Finding Correlation Coefficients for Male and Female Borrowers

FHLAppendFemale <- FHLAppend_Sample %>% 
  filter(FemaleBorrower == 1)

cor(FHLAppendFemale$LTVRatioPercent, FHLAppendFemale$NoteAmount)

FHLAppendMale <- FHLAppend_Sample %>% 
  filter(FemaleBorrower == 0)

cor(FHLAppendMale$LTVRatioPercent, FHLAppendMale$NoteAmount)


# Bar chart of Average LTV for the years (2010,2015,2020)

FHL_LTVavg <- data.frame(aggregate(FHLAppend$LTVRatioPercent,
                                   by = list(FHLAppend$Year),
                                   FUN = mean))


LTVavgplot <- ggplot(FHL_LTVavg, aes(factor(Group.1), x))+
  geom_bar(stat = "identity", color = "black",
           fill = "darkgreen")+
  geom_text(aes(label = round(signif(x),2))
            , vjust = -0.8, size = 3.5)+
  labs(x = "Year",
       y = "Average LTV (%)",
       title = "Figure 1: Changes in Average LTV Percentage By Year")+
  theme_clean()+
  theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

LTVavgplot


# Calculating percent change for the values of average LTV in the 
# bar chart

(74.84-72.12)/(72.12)

(72.25-74.84)/(74.84)


# Create Box Plot for White Borrowers and Non White Borrowers, comparing their
# credit score and LTV Ratio Percent


FHLAppend %>%
  filter(BorrowerCreditScore %in% c(2,3,4,5)) %>%
  mutate(WhiteBorrower = dplyr::recode(WhiteBorrower, `1` = "White Borrower",
                                `0` = "Non White Borrower")) %>%
  rename(Race = WhiteBorrower) %>% 
  mutate(BorrowerCreditScore = dplyr::recode(BorrowerCreditScore, `2` = "620 < 660",
                                      `3` = "660 < 700",
                                      `4` = "700 < 760",
                                      `5` = "760 or greater")) %>%
  ggplot(aes(x = BorrowerCreditScore,
             y = LTVRatioPercent,
             fill = Race))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~Race)+
  labs(title = "Figure 4: Comparison of LTV % by Borrower's Credit Score",
       x = "Borrower's Credit Score",
       y = "LTV Ratio Percentage")+
  theme_hc()+
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"))


# Finding the median LTV values of White and Non-White Borrowers Based on their 
# credit score

FHLAppendWhite <- FHLAppend %>% 
  filter(WhiteBorrower == 1)


aggregate(FHLAppendWhite$LTVRatioPercent,
          by = list(FHLAppendWhite$BorrowerCreditScore),
          FUN = median)

FHLAppendNonWhite <- FHLAppend %>% 
  filter(WhiteBorrower == 0)

aggregate(FHLAppendNonWhite$LTVRatioPercent,
          by = list(FHLAppendNonWhite$BorrowerCreditScore),
          FUN = median)


  
# Create Box Plot for Male and Female Borrowers, comparing their credit
# score and LTV Ratio Percent

FHLAppend %>%
  filter(BorrowerCreditScore %in% c(2,3,4,5)) %>%
  mutate(FemaleBorrower = dplyr::recode(FemaleBorrower, `1` = "Female Borrower",
                                `0` = "Male Borrower")) %>%
  rename(Gender = FemaleBorrower) %>% 
  mutate(BorrowerCreditScore = dplyr::recode(BorrowerCreditScore, `2` = "620 < 660",
                                      `3` = "660 < 700",
                                      `4` = "700 < 760",
                                      `5` = "760 or greater")) %>% 
  ggplot(aes(x = BorrowerCreditScore,
             y = LTVRatioPercent,
             fill = Gender))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~Gender)+
  labs(title = "Figure 5: Comparison of LTV % by Borrower's Credit Score",
       x = "Borrower's Credit Score",
       y = "LTV Ratio Percentage")+
  theme_hc()+
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"))


# Finding the median LTV values of Female and Male Borrowers Based on their 
# credit score

FHLAppendFemale <- FHLAppend %>% 
  filter(FemaleBorrower == 1)

aggregate(FHLAppendFemale$LTVRatioPercent,
          by = list(FHLAppendFemale$BorrowerCreditScore),
          FUN = median)


FHLAppendMale <- FHLAppend %>% 
  filter(FemaleBorrower == 0)

aggregate(FHLAppendMale$LTVRatioPercent,
          by = list(FHLAppendMale$BorrowerCreditScore),
          FUN = median)

  

# Creating new datatset called FHLStateName

FHLStateName <- FHLAppend

# Renaming Variable name for the State Variable

FHLStateName <- FHLStateName %>% 
  rename(State = FIPSStateCode)

FHLStateName2010 <- FHLStateName %>%
  filter(Year == 2010)

# Creating a dataset that counts the number of borrowers in each
# state with a credit score of 760 or above

FHL760credit_2010 <- FHLStateName2010 %>%
  filter(Borrower760orabove == 1) %>%
  group_by(State) %>% 
  summarise(Borrower760orabove = n()) 

# Changing FIPScode into Abbreviation of State

FHL760credit_2010$State<- fips(FHL760credit_2010$State, to = 'Abbreviation')

View(FHL760credit_2010)

# Creating a dataset/table for the Top 10 states who have the highest
# number of borrowers with a credit score of 760 or above in 2010

FHL760top10_2010 <- FHL760credit_2010 %>% 
  arrange(desc(Borrower760orabove)) %>% 
  slice(1:10)

View(FHL760top10_2010)


kable(x = FHL760top10_2010,
      caption = '<b>Table 3: States With the Highest Number of Low Risk
      Borrowers Based on Credit Scores</b>',
      digits = 2,
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  kableExtra::footnote(general = "This table only looks at borrowers from 2010")



# Creating a Map Chart too see where more low risk borrowers are for 2010

creditscoregraph1 <- plot_geo(FHL760credit_2010, 
                             locationmode = 'USA-states') %>% 
  add_trace(locations = ~State,
            z = ~Borrower760orabove,
            color = ~Borrower760orabove,
            colors = "YlGnBu",
            marker = list(line = list(
              width = 0.5,
              opacity = 0.5
            )
          )
        ) %>% 
  layout(geo = list(scope = 'usa'),
         font = list(family = "DM Sans"),
         title = "Figure 6: Borrowers With 760 or Above \nCredit Score in U.S. (2010)")

  

creditscoregraph1


# Creating a dataset that counts the number of borrowers in each state
# that have credit score between 620 & 660 in 2010
  

FHLcreditscore620to660_2010 <- FHLStateName2010 %>% 
  filter(Borrower620to660 == 1) %>% 
  group_by(State) %>% 
  summarise(Borrower620to660 = n())


FHLcreditscore620to660_2010$State <- fips(FHLcreditscore620to660_2010$State,
                                     to = 'Abbreviation')


# # Creating a dataset/table for the Top 10 states who have the highest
# number of borrowers with a credit score between 620 & 660 in 2015

FHL620to660top10_2010 <- FHLcreditscore620to660_2010 %>% 
  arrange(desc(Borrower620to660)) %>% 
  slice(1:10)

View(FHL620to660top10_2010)

kable(x = FHL620to660top10_2010,
      caption = '<b>Table 4: States With the Highest Number of High Risk
      Borrowers Based on Credit Scores</b>',
      digits = 2,
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  kableExtra::footnote(general = "This table only looks at borrowers from 2010.")

# Creating a Map Chart too see where more high risk borrowers are for 2010

creditscoregraph2 <- plot_geo(FHLcreditscore620to660_2010,
                              locationmode = 'USA-states') %>% 
  add_trace(locations = ~State,
            z = ~Borrower620to660,
            color = ~Borrower620to660,
            colors = "YlOrRd",
            marker = list(line = list(
              width = 0.5,
              opacity = 0.5
            )
      )
  ) %>% 
  layout(geo = list(scope = 'usa'),
         font = list(family = "DM Sans"),
         title = "Figure 8: Borrowers with Credit Score \nBetween 620 & 660 in the U.S. (2010)")


creditscoregraph2


# Filtering Values to look at Borrowers in 2020

FHLStateName2020 <- FHLStateName %>%
  filter(Year == 2020)


FHL760credit2020 <- FHLStateName2020 %>%
  filter(Borrower760orabove == 1) %>%
  group_by(State) %>% 
  summarise(Borrower760orabove = n()) 


# Changing FIPScode into Abbreviation of State

FHL760credit2020$State<- fips(FHL760credit2020$State, to = 'Abbreviation')

View(FHL760credit2020)

# Creating a dataset/table for the Top 10 states who have the highest
# number of borrowers with a credit score of 760 or above in 2020

FHL760top10_2020 <- FHL760credit2020 %>% 
  arrange(desc(Borrower760orabove)) %>% 
  slice(1:10)

View(FHL760top10_2020)


kable(x = FHL760top10_2020,
      caption = '<b>Table 5: States With the Highest Number of Low Risk
      Borrowers Based on Credit Scores</b>',
      digits = 2,
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  kableExtra::footnote(general = "This table only looks at borrowers from 2020")



# Creating a Map Chart too see where low risk borrowers are for 2020

creditscoregraph3 <- plot_geo(FHL760credit2020, 
                              locationmode = 'USA-states') %>% 
  add_trace(locations = ~State,
            z = ~Borrower760orabove,
            color = ~Borrower760orabove,
            colors = "YlGnBu",
            marker = list(line = list(
              width = 0.5,
              opacity = 0.5
            )
        )
  ) %>% 
  layout(geo = list(scope = 'usa'),
         font = list(family = "DM Sans"),
         title = "Figure 7: Borrowers With 760 or Above \nCredit Score in U.S. (2020)")


creditscoregraph3



# Creating a dataset that counts the number of borrowers in each state
# that have credit score between 620 & 660 in 2020


FHLcreditscore620to660_2020 <- FHLStateName2020 %>% 
  filter(Borrower620to660 == 1) %>% 
  group_by(State) %>% 
  summarise(Borrower620to660 = n())


FHLcreditscore620to660_2020$State <- fips(FHLcreditscore620to660_2020$State,
                                     to = 'Abbreviation')


# # Creating a dataset/table for the Top 10 states who have the highest
# number of borrowers with a credit score between 620 & 660 in 2020.

FHL620to660top10_2020 <- FHLcreditscore620to660_2020 %>% 
  arrange(desc(Borrower620to660)) %>% 
  slice(1:10)

View(FHL620to660top10_2020)

kable(x = FHL620to660top10_2020,
      caption = '<b>Table 6: States With the Highest Number of High Risk
      Borrowers Based on Credit Scores</b>',
      digits = 2,
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  kableExtra::footnote(general = "This table only looks at borrowers from 2020.")


# Creating a Map Chart too see where high risk borrowers are for 2020

creditscoregraph4 <- plot_geo(FHLcreditscore620to660_2020,
                              locationmode = 'USA-states') %>% 
  add_trace(locations = ~State,
            z = ~Borrower620to660,
            color = ~Borrower620to660,
            colors = "YlOrRd",
            marker = list(line = list(
              width = 0.5,
              opacity = 0.5
            )
        )
  ) %>% 
  layout(geo = list(scope = 'usa'),
         font = list(family = "DM Sans"),
         title = "Figure 9: Borrowers with Credit Score \nBetween 620 & 660 in the U.S. (2020)")


creditscoregraph4



# Creating Tables of Descriptive Statistics Table for White, Non-White, Female, and 
# Male Borrowers


 FHL_table1 <- FHLAppend %>%
   filter(WhiteBorrower == 1) %>% 
   select(LTVRatioPercent, HousingExpenseRatioPercent,
         TotalDebtExpenseRatioPercent, TotalMonthlyIncomeAmount,
         BorrowerAge,HUDMedianIncomeAmount,NoteRatePercent, NoteAmount) %>%
  sumtable(., summ = c('mean(x)',
                       'min(x)',
                       'max(x)', 
                       'sd(x)',
                       'median(x)'),
           out = 'return')


kable(x = FHL_table1,
      caption = "<b>Table 1: Descriptive Statistics (White Borrowers)</b>",
      digits = 2,
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2:6, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  footnote(general = "Table was created using the FHLAppend dataset which contains data from 2010, 2015, & 2020.")



FHL_table2 <- FHLAppend %>%
  filter(WhiteBorrower == 0) %>% 
  select(LTVRatioPercent, HousingExpenseRatioPercent,
         TotalDebtExpenseRatioPercent, TotalMonthlyIncomeAmount,
         BorrowerAge,HUDMedianIncomeAmount,NoteRatePercent, NoteAmount) %>%
  sumtable(., summ = c('mean(x)',
                       'min(x)',
                       'max(x)',
                       'sd(x)',
                       'median(x)'),
           out = 'return')


kable(x = FHL_table2,
      caption = "<b>Table 2: Descriptive Statistics (Non-White Borrowers)</b>",
      digits = 2,
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2:6, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  footnote(general = "Table was created using the FHLAppend dataset which contains data from 2010, 2015, & 2020.")



FHL_table3 <- FHLAppend %>%
  filter(FemaleBorrower == 1) %>% 
  select(LTVRatioPercent, HousingExpenseRatioPercent,
         TotalDebtExpenseRatioPercent, TotalMonthlyIncomeAmount,
         BorrowerAge,HUDMedianIncomeAmount,NoteRatePercent, NoteAmount) %>%
  sumtable(., summ = c('mean(x)',
                       'min(x)',
                       'max(x)',
                       'sd(x)',
                       'median(x)'),
           out = 'return')

kable(x = FHL_table3,
      caption = "<b>Table 3: Descriptive Statistics (Female Borrowers)</b>",
      digits = 2, 
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2:6, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  footnote(general = "Table was created using the FHLAppend dataset which contains data from 2010, 2015, & 2020.")


FHL_table4 <- FHLAppend %>%
  filter(FemaleBorrower == 0) %>% 
  select(LTVRatioPercent, HousingExpenseRatioPercent,
         TotalDebtExpenseRatioPercent, TotalMonthlyIncomeAmount,
         BorrowerAge,HUDMedianIncomeAmount,NoteRatePercent, NoteAmount) %>%
  sumtable(., summ = c('mean(x)',
                       'min(x)',
                       'max(x)',
                       'sd(x)',
                       'median(x)'),
           out = 'return')



kable(x = FHL_table4,
      caption = "<b>Table 4: Descriptive Statistics (Male Borrowers)</b>",
      digits = 2,
      format = 'html') %>% 
  kable_paper("striped", full_width = F) %>%
  row_spec(0, bold = T, background = "#140A0ACA", color = "white") %>% 
  column_spec(1, bold = T, background = "#F2C309D8") %>% 
  column_spec(2:6, bold = T,color = "white",
              background = "#3E4AABD8") %>% 
  footnote(general = "Table was created using the FHLAppend dataset which contains data from 2010, 2015, & 2020.")



# Transforming certain variables to be used in regression analysis to find meaningful interpretations

FHLAppend <- FHLAppend %>% 
  mutate(MonthlyIncome1000s = TotalMonthlyIncomeAmount/1000,
         NoteAmount1000s = NoteAmount/1000,
         HUDMedianIncomeAmount1000s = HUDMedianIncomeAmount/1000)

# Create 4 Different Regression Models, looking at the effects of LTV based on 
# Borrower's Race and Self-Employment Status

regression_results <- list(
  
  "Regression 1 
  White Borrowers" = lm(LTVRatioPercent ~ MonthlyIncome1000s +
                        BorrowerAge + WhiteBorrower + BorrowerFirstTimeHomebuyer 
                        + NoteAmount1000s + HUDMedianIncomeAmount1000s
                        , data = FHLAppend),
  
  "Regression 2 
  Black Borrowers" = lm(LTVRatioPercent ~ MonthlyIncome1000s +
                        BorrowerAge + BlackBorrower + BorrowerFirstTimeHomebuyer
                        + NoteAmount1000s + HUDMedianIncomeAmount1000s
                      , data = FHLAppend),
  
  "Regression 3 \nSelf Employed White Borrowers" = lm(LTVRatioPercent ~ MonthlyIncome1000s +
                                                        BorrowerAge + SelfEmployedWhiteBorrower + BorrowerFirstTimeHomebuyer 
                                                      , data = FHLAppend),
  
  "Regression 4 \nSelf Employed Black Borrowers" = lm(LTVRatioPercent ~ MonthlyIncome1000s +
                                                        BorrowerAge + SelfEmployedBlackBorrower + BorrowerFirstTimeHomebuyer
                                                      , data = FHLAppend)
  
)


regressiontable <- modelsummary(regression_results, stars = c('*' = 0.1,
                                                              '**' = 0.05,
                                                              '***' = 0.01),
                                title = "Table 7: Differences in LTV By Race & Gender",
                                gof_omit = 'IC|Log|Adj',
                                output = "gt")

regressiontable %>% 
  tab_footnote(footnote = md("Variables of interest are highlighted and their
                             coefficients are in red.")) %>% 
  
  tab_style(style = cell_text(color = 'red'),
            locations = cells_body(rows = 7)) %>%
  
  tab_style(style = cell_fill(color = 'lightblue'),
            locations = cells_body(rows = 7)) %>% 
  
  tab_style(style = cell_text(color = 'red'),
            locations = cells_body(rows = 15)) %>% 
  
  tab_style(style = cell_fill(color = 'lightblue'),
            locations = cells_body(rows = 15)) %>% 
  
  tab_style(style = cell_text(color = 'red'),
            locations = cells_body(rows = 17)) %>% 
  
  tab_style(style = cell_fill(color = 'lightblue'),
            locations = cells_body(rows = 17)) %>% 
  
  tab_style(style = cell_text(color = 'red'),
            locations = cells_body(rows = 19)) %>% 

  tab_style(style = cell_fill(color = 'lightblue'),
          locations = cells_body(rows = 19))




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    TABLE OF CONTENT
*####
*###############################################################################
*###############################################################################
********************************************************************************
*  1.PREPARATION:
*    CONFIG VARIABLES @1
*    IMPORT DATA // CLEAN DATA @2
*    REMOVE BAD OBSERVATIONS @3
*    PREPARATION FOR CII@4
*    PREPARATION FOR P- AND C-COVENANTS@5
*    OUTLIER CLEANING@10 
*    PREPARE REGRESSION ANALYSIS@11

*  2.VARIABLES GENERATION
*    GENERATE VARIABLES FOR OUR DATA SET@6
*    USE PANEL DATA STRUCTURE@7
*    Hollander's Covenant Intensity Index@8
*    P and C Covenant by Christensen and Nikolaev 2011@9 

*  3.PRESENTING:
**   GENERATE TABLE 4@12
**   GENERATE TABLE 3@13
**   DESCRIPTIVE TABLE // TABLE 2@14
**   EXPORT CURRENT DATASET FOR GRAPHS IN R@15
**   ADD GRAPHS@16
********************************************************************************



********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    CONFIG VARIABLES@1
*####
********************************************************************************


* Set directory of this replication
global WORKING_DIR C:\Users\Viktor Cheng\Downloads\Research\Research\

* Set up variables
global Lags 1 2 3 4
global LagsWithZero 0 1 2 3 4




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    IMPORT DATA // CLEAN DATA@2
*####
********************************************************************************

*** Load Data Set
import delimited "${WORKING_DIR}Data Set.csv", case(preserve) clear 

*** Transform variables
replace LeadArrangerCredit = "1" if LeadArrangerCredit == "TRUE"
replace LeadArrangerCredit = "0" if LeadArrangerCredit == "FALSE"
replace LeadArrangerCredit = "" if LeadArrangerCredit == "NA"
destring LeadArrangerCredit, replace

replace LenderCounty = "" if LenderCounty == "NA"
replace BorrowerCounty = "" if BorrowerCounty == "NA"

destring DealAmount, replace force
destring Maturity, replace force

destring BorrowerSICCode, replace force

* Lag -> "lag" month before the package start date
* -> Lag = 1 : The month before period package starts / closed
foreach lag of global Lags {
	destring L`lag'FlightTime, replace force
}

foreach lag of global LagsWithZero {
	destring L`lag'StateUnemployment, replace force
	destring L`lag'PerCapitalPersonalIncome, replace force
	destring L`lag'TotalEmployment, replace force
	destring L`lag'AverageWagesAndSalaries, replace force
	
}



*** Ensure proper Spelling / Different Spelling of NYC 
replace BorrowerCity = "new york city" if BorrowerCity == "new york" & BorrowerState == "new york"
replace LenderCity = "new york city" if LenderCity == "new york" & LenderState == "new york"


*** Label Covenants to ensure readibility
label variable CovMinQuickRatioInitialRatio "Quick ratio covenant"
label variable CovMinCurrentRatioInitialRatio "Current ratio covenant"
label variable CovMaxDToEInitialRatio "Debt-to-equity ratio covenant"
label variable CovMaxLoanToValueInitialRatio "Loan-to-value ratio covenant"
label variable CovMaxDToTNWInitialRatio "Ratio of debt to tangible net worth covenant"
label variable CovMaxLevRatioInitialRatio "Leverage ratio covenant"
label variable CovMaxSenLevInitialRatio "Senior leverage ratio covenant"
label variable CovMinNWToTotalAInitialRatio "Net worth requirement covenant"
label variable CovMinCashICInitialRatio "Cash interest coverage ratio covenant"
label variable CovMinDServiceCInitialRatio "Debt service coverage ratio covenant"
label variable CovMinEbitdaInitialRatio "Level of EBITDA covenant"
label variable CovMinFixedChargeCInitialRatio "Fixed charge coverage covenant"
label variable CovMinICInitialRatio "Interest coverage ratio covenant"
label variable CovMaxDToEbitdaInitialRatio "Ratio of debt to EBITDA covenant"
label variable CovMaxSenDToEbitdaInitialRatio "Ratio of senior debt to EBITDA covenant"




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    REMOVE BAD OBSERVATIONS@3
*####
********************************************************************************

* We exclude observations from Puerto Rico since its too far away,
* -> it for sure violate the 8 hour flight time restriction
drop if BorrowerState == "puerto rico"
drop if LenderState == "puerto rico"

* We exclude all borrower-lender-constellations closer than 150 km
* -> nobody would fly at such distances
keep if BorrowerLenderDistance > 150

* Exclude observations without industry code (SIC)
* -> without SIC code we are not able to add the according industry fixed effect
drop if BorrowerSICCode == 0

* Exclude observations without borrower county
* -> the credit demand controls has been merged by county name, so any observations
*    without county name have no credit demand controls -> get rid of them
drop if BorrowerCounty == ""

* Exclude airlines (argumentation see paper)
* -> they could enforce a change in flight time by themselves => Endogeneity issue
* -> since there are just a few observations that are airlines, better safe
*    than sorry, we won't miss them really
drop if BorrowerSICCode == 4512




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    PREPARATION FOR CII@4
*####
********************************************************************************
* Notice: Assuming all NAs in these variables are equal to 0s
* Transform The NAs and NaN are converted in covenants

replace CovMinEToARatioInitialRatio="0" if CovMinEToARatioInitialRatio=="NA"
replace CovMinEToARatioInitialRatio="0" if CovMinEToARatioInitialRatio=="NaN"
destring CovMinEToARatioInitialRatio, replace

* Targeted variables besides of financial ratios(testing purposes)
replace Secured = "0" if Secured == "No"
replace Secured = "1" if Secured == "Yes"
replace DividendRestrictions = "0" if DividendRestrictions == "No"
replace DividendRestrictions = "1" if DividendRestrictions == "Yes"
replace AssetSalesSweep = "0" if AssetSalesSweep == "NA"
replace DebtIssuanceSweep = "0" if DebtIssuanceSweep == "NA"
replace EquityIssuanceSweep = "0" if EquityIssuanceSweep == "NA"


**Change the data type to numeric values:
destring Secured, replace
destring DividendRestrictions, replace
destring AssetSalesSweep, replace
destring DebtIssuanceSweep, replace
destring EquityIssuanceSweep, replace




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    PREPARATION FOR P- AND C-COVENANTS@5
*####
********************************************************************************
*** Notice: We only care about the inital ratios, as the final ratios are simply 
*** quantitative modifications to the original covenant, but does not modified the 
*** type of covenants.


*** Convert relevant covenants for the P and C to correct data type

local Ratios CovMinCurrentRatioInitialRatio CovMinICInitialRatio CovMaxDToEbitdaInitialRatio CovMaxSenDToEbitdaInitialRatio CovMaxDToTNWInitialRatio CovMinFixedChargeCInitialRatio CovMaxLevRatioInitialRatio CovMinQuickRatioInitialRatio CovMinDServiceCInitialRatio CovMaxDToEInitialRatio CovMinCashICInitialRatio CovMaxCapexInitialRatio CovMinEbitdaInitialRatio CovMaxSenLevInitialRatio CovMaxLoanToValueInitialRatio CovOtherRatioInitialRatio CovMaxNDToAInitialRatio CovMinNWToTotalAInitialRatio

foreach var of local Ratios {
replace `var'="0" if `var'=="NA"
replace `var'="0" if `var'=="NaN"
destring `var', replace
}




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    GENERATE VARIABLES FOR OUR DATA SET@6
*####
********************************************************************************

*** First give all borrower-lender-constellations an unique id
egen RelationshipID = group(BorrowerCompanyID LenderCompanyID)


*** Convert facility-level data to package-level data
* -> We initially started with facility-level, just in case we needed facility-
*    level information. Since we know need the package-level, we just "upgrade".
* -> We can take just a random facility from the package and drop all others,
*    because the package-level information is in a facilities the same.
* => Package-level data
*
* Please note: in the following code, we use FacilityStartDate (facility-level
* value) instead of its package-level equivalent DealActiveDate . We are aware
* of this "issue", but are almost interchangable. In ~ 400 of 100,000 cases there
* is a different between two variables. Some of those differences are really
* strange, therefore, we preferred FacilityStartDate
* 
* See:
* browse if FacilityStartDate != DealActiveDate
duplicates drop RelationshipID PackageID, force


*** Add variables for fixed effects -> extract month and year
tostring FacilityStartDate, generate(StringFacilityStartDate)
* YEAR
gen Year = substr(StringFacilityStartDate, 1, 4)
* MONTH
gen Month = substr(StringFacilityStartDate, 5, 2)
* YEAR x MONTH
gen YearMonth = substr(StringFacilityStartDate, 1, 6)
destring Year, replace force
destring Month, replace force
destring YearMonth, replace force
drop StringFacilityStartDate


*** Count number of participants in the package
* -> Just count the duplicate rows and add one, this works because each lender
*    has his own row. We add one to count also the current lender.
duplicates tag PackageID, generate(Participants)
replace Participants = Participants + 1


*** Generate variable that identifies county uniquely
egen UniqueCounty = group(BorrowerCounty BorrowerState)

*** Counts the number of packages within the Constellation 
sort RelationshipID FacilityStartDate PackageID
by RelationshipID: gen RelationshipFacilityNumber = _n
by RelationshipID: gen RelationshiopTotalFacilities = _N




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    USE PANEL DATA STRUCTURE@7
*####
********************************************************************************

* In this part we get the flight time of the previous package under the same
* borrower-lender-constellation

* Usually something like:
* xtset Relationship FacilityStartDate
* should work. But for us it does not, why?
* -> If we want to take now the observation under the same borrower-lender-
*    constellation, L-1 returns the value of the observation for this 
*    constellation of the previous day. However, our borrowers don't take every
*    day a new loan. So L-1 would return NA

* Our work-around:
*   1) sort observations by borrower-lender-relationship
*   2) sort then by start date
*   3) now _n-1 returns the data of the previous package under the same
*      borrower-lender-relationship, except _n = 1 (i.e. the first package
*      under the given borrower-lender-relationship)
*      -> set the variable we want to obtain to NA
*      -> replace NA by previous value if relationship | _n = relationship | _n-1

* So sort them
sort RelationshipID FacilityStartDate PackageID

* Then generate those variables

foreach lag of global Lags {

	replace L`lag'FlightTime = round(L`lag'FlightTime, 0.1)
	
	* Get lagged value
	gen L`lag'PreviousFlightTime = L`lag'FlightTime[_n-1] if RelationshipID[_n-1] == RelationshipID
	
	* Compute treatment value / state
	gen L`lag'FasterFlight = (L`lag'PreviousFlightTime/L`lag'FlightTime) > 1.025 if L`lag'PreviousFlightTime != .
	gen L`lag'FlightTimeReduction = L`lag'PreviousFlightTime - L`lag'FlightTime
	
	* However, the work-around has one pitfall: if there is any borrower-lender-
	* relationship that closes two or more packages at the same day, we don't
	* treat it properly, example:
	
	* Relationship | PackageClosedAt | FlightTime | DifferenceFlightTimeToPrevious | FasterFlight
	*      1       |    yesterday    |     90     |                .               |      0
	*      1       |    today        |     75     |                15              |      1
	*      1       |    today        |     75     |                0               |      0
	
	* The third package refers to the second package, but both packages are closed
	* on the same day, so of course both have the same flight time and of course
	* both have a flight time reduction compared to the package closed before.
	* However, if we take the approach describes above, for n = 3, we would take
	* the 2nd row, this leads to a miss classification.
	
	* The code below adjust this issue. If two or more packages are closed on the
	* same day, all packages refer to packages closed at earlier dates.
	by RelationshipID FacilityStartDate: egen GroupPreviousFlightTime = min(L`lag'PreviousFlightTime)
	replace L`lag'PreviousFlightTime = GroupPreviousFlightTime
	
	by RelationshipID FacilityStartDate: egen GroupFasterFlight = max(L`lag'FasterFlight)
	replace L`lag'FasterFlight = GroupFasterFlight
	
	by RelationshipID FacilityStartDate: egen GroupFlightTimeReduction = max(L`lag'FlightTimeReduction)
	replace L`lag'FlightTimeReduction = GroupFlightTimeReduction
	
	drop GroupPreviousFlightTime GroupFasterFlight GroupFlightTimeReduction
	
	* Compute additional treatment variable (always positive treatment variable)
	gen L`lag'FlightTimeReductionPositive = L`lag'FlightTimeReduction
	replace L`lag'FlightTimeReductionPositive = 0 if L`lag'FlightTimeReductionPositive < 0

}

* LxFasterFlight and LxFlightTimeReductionPositive are the variables of interest,
* they indicate the treatment state.




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    Hollander's Covenant Intensity Index@8
*####
********************************************************************************
********************************************************************************
* Construct the Covenant Intensity following the hollander paper
* Hollander based their restrictiveness on counting the covenants,
* we will follow suit.
local Ratios CovMinCurrentRatioInitialRatio CovMinICInitialRatio CovMaxDToEbitdaInitialRatio CovMaxSenDToEbitdaInitialRatio CovMaxDToTNWInitialRatio CovMinFixedChargeCInitialRatio CovMaxLevRatioInitialRatio CovMinQuickRatioInitialRatio CovMinDServiceCInitialRatio CovMaxDToEInitialRatio CovMinCashICInitialRatio CovMaxCapexInitialRatio CovMinEbitdaInitialRatio CovMaxSenLevInitialRatio CovMaxLoanToValueInitialRatio CovOtherRatioInitialRatio CovMinEToARatioInitialRatio CovMaxNDToAInitialRatio CovMinNWToTotalAInitialRatio

* By using a for loop, we are able loop through all covenants to construct an indicator variable.
* We further than use the indicator variable to construct the CII with all covenants included.
* Notice some covenants are missing as they contain no values throughout the dataset.

foreach var of local Ratios {
gen FR`var'= 0
replace FR`var'= 1 if `var'!=0
}

egen CII_total = rowtotal (FRCovMinCurrentRatioInitialRatio-FRCovMinNWToTotalAInitialRatio)
* drop temporary calculations
drop (FRCovMinCurrentRatioInitialRatio-FRCovMinNWToTotalAInitialRatio)

********************************************************************************

* Construct the Covenant Intensity following the hollander paper but modified it
* (only potential P covenants are included)
local Ratios CovMinCashICInitialRatio CovMinDServiceCInitialRatio CovMinEbitdaInitialRatio CovMinFixedChargeCInitialRatio CovMinICInitialRatio CovMaxDToEbitdaInitialRatio CovMaxSenDToEbitdaInitialRatio 

* We will follow similar method from the CII_total, but as argured in the paper
* only P-Covenants are included

foreach var of local Ratios {
gen FR`var'= 0
replace FR`var'= 1 if `var'!=0
}

egen CII_HollanderP = rowtotal (FRCovMinCashICInitialRatio-FRCovMaxSenDToEbitdaInitialRatio)

* drop temporary calculations
drop (FRCovMinCashICInitialRatio-FRCovMaxSenDToEbitdaInitialRatio)


********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    P and C Covenant by Christensen and Nikolaev 2011@9 
*####
********************************************************************************
********************************************************************************
* Basic Idea: We will flag the covenants accordingly by certain ratios suggested
* by Christensen and Nikolaev.


* Flag Covenant C and P, as long as there is P the CovenantFlag will be 1

gen CovenantC =0
gen CovenantP =0


**Performance Covenants: (1) Cash interest coverage ratio , 
**(2) Debt service coverage ratio, (3) Level of EBITDA, 
** (4) Fixed charge coverage ratio, (5) Interest coverage ratio, 
**(6) Ratio of debt to EBITDA, and (7) Ratio of senior debt to EBITDA.
**In the dataset:
**CovMinCashICInitialRatio
**CovMinDServiceCInitialRatio
**CovMinEbitdaInitialRatio
**CovMinFixedChargeCInitialRatio
**CovMinICInitialRatio
**CovMaxDToEbitdaInitialRatio
**CovMaxSenDToEbitdaInitialRatio

replace CovenantP = 1 if CovMinCashICInitialRatio != 0 | CovMinDServiceCInitialRatio != 0 | CovMinEbitdaInitialRatio != 0 | CovMinFixedChargeCInitialRatio != 0 | CovMinICInitialRatio != 0 | CovMaxDToEbitdaInitialRatio != 0 | CovMaxSenDToEbitdaInitialRatio != 0 


**Capital Covenants: (1) Quick ratio, (2) Current ratio, (3) Debt-to-equity
**ratio, (4) Loan-to-value ratio, (5) Ratio of debt to tangible net worth, (6)
**Leverage ratio, (7) Senior leverage ratio, and (8) Net worth requirement
**In the dataset:
**CovMinQuickRatioInitialRatio
**CovMinCurrentRatioInitialRatio
**CovMaxDToEInitialRatio
**CovMaxLoanToValueInitialRatio
**CovMaxDToTNWInitialRatio
**CovMaxLevRatioInitialRatio
**CovMaxSenLevInitialRatio
**CovMinNWToTotalAInitialRatio

replace CovenantC = 1 if CovMinQuickRatioInitialRatio != 0 |  CovMinCurrentRatioInitialRatio != 0 | CovMaxDToEInitialRatio != 0 | CovMaxLoanToValueInitialRatio != 0 | CovMaxDToTNWInitialRatio != 0 | CovMaxLevRatioInitialRatio != 0 |  CovMaxSenLevInitialRatio != 0 |  CovMinNWToTotalAInitialRatio != 0



********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    OUTLIER CLEANING@10 
*####
********************************************************************************
********************************************************************************

* Exclude all extreme treatment values
* -> We focus on L3, since this is our lag of interest. If we would do it for
*    every four lags, we would exclude more than the top and bottom 1 %. More-
*    over, these outliers seem to be correlated: i.e. if L3FlightTimeReduction
*    is a outlier, L4FlightTimeReduction will it be most likely as well
quietly: summarize L3FlightTimeReduction, detail
drop if L3FlightTimeReduction < r(p1) | L3FlightTimeReduction > r(p99)




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    PREPARE REGRESSION ANALYSIS@11
*####
********************************************************************************
********************************************************************************

* Define cluster level, controls, and fixed effects
global Clustering i.BorrowerCompanyID#i.LenderCompanyID
global Effects c.Year#i.BorrowerSICCode i.LenderCompanyID#i.BorrowerCompanyID i.Month i.Year
global Controls L0StateUnemployment L0PerCapitalPersonalIncome L0TotalEmployment L0AverageWagesAndSalaries


* Make sure we follow our model assumption, i.e. set exclusion restriction for the sample
* More detailed reasoning and explanation, see paper.

* Sample restriction criteria -> This marks all variables that fulfil the specified criteria. 
foreach lag of global Lags {
	
	* This is a variable indicating whether a variable fulfills the requirements
	gen L`lag'SampleRestriction = 0
	gen L`lag'SampleRestrictionSimple = 0
	
	* Basic inclusion criteria
	* (i)   At least one covenant
	* (ii)  Flight time of more than 60 minutes
	* (iii) Flight time below 8 hours
	replace L`lag'SampleRestrictionSimple = 1 if CII_total > 0 & L`lag'FlightTime > 1*60 & L`lag'FlightTime < 8*60
	
	* Add then restriction due to parallel trend assumption
	* (iv)  Year > 1995 
	* (v)   Year < 2019
	replace L`lag'SampleRestriction = 1 if L`lag'SampleRestrictionSimple & Year > 1995 & Year < 2019
	
	* Split sample into lead and non-lead arrangers
	gen L`lag'SampleRestrictionLeaders = (L`lag'SampleRestriction == 1 & LeadArrangerCredit == 1)
	gen L`lag'SampleRestrictionNoLeaders = (L`lag'SampleRestriction == 1 & LeadArrangerCredit == 0)
}




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    GENERATE TABLE 4@12
*####
********************************************************************************
* This code generates the Table 2 of our paper

* Across all tables we ensure that the sample size is the same. Since Table 2
* extends Table 1, it is sufficient to ensure that in this table the number
* of observations stays constant, how do we ensure it?

* -> We run the regression two times. In the first round we mark all used
*    observations
* -> The second one uses only those observations used in all regressions
*    and returns the "nice" sample size.


* We create a temporary variable for the treatment variables such that
* estout thinks it is always the same variable, even though we consider
* different flight periods
gen FasterFlight = .
gen FlightTimeReductionPositive = .


* This variable marks observation used in all regressions
quietly: gen UsedObservation = 1


* Since in the regression of Table 2 vary only in the period of considered
* flights, we just need to adjust this within the loop. everything else
* stays constant
foreach lag of global Lags {

	* Take only observations that we may use
	quietly: replace UsedObservation = UsedObservation & L`lag'SampleRestrictionLeaders
	
	* Copy treatment variables
	quietly: replace FlightTimeReductionPositive = L`lag'FlightTimeReductionPositive
	quietly: replace FasterFlight = L`lag'FasterFlight
		
	* Run binary treatment regression // Columns (1) - (4), L1 = Column (1), L4 = Column (4)
	quietly: reghdfe CII_HollanderP FasterFlight $Controls if UsedObservation, absorb($Effects) vce(cluster $Clustering) noconstant
	
	* Mark used observations
	quietly: gen UsedObservationBinary = e(sample)
	
	* Run continuous treatment regression  // Columns (5) - (8), L1 = Column (4), L4 = Column (8)
	quietly: reghdfe CII_HollanderP FlightTimeReductionPositive $Controls if UsedObservation, absorb($Effects) vce(cluster $Clustering) noconstant
	
	* Mark used observations
	quietly: gen UsedObservationContinuous = e(sample)
	
	* Update the indicator whether a variable is used or not
	quietly: replace UsedObservation = UsedObservation & UsedObservationBinary & UsedObservationContinuous
		
	quietly: drop UsedObservationBinary
	quietly: drop UsedObservationContinuous
}



* This is now the same regression above, but we use the exact same sample for
* all regression within this Table 2. And we store the result for estout
foreach lag of global Lags {
	* Copy treatment values again
	quietly: replace FlightTimeReductionPositive = L`lag'FlightTimeReductionPositive
	quietly: replace FasterFlight = L`lag'FasterFlight
	
	* Run binary regression
	quietly: reghdfe CII_HollanderP FasterFlight $Controls if UsedObservation, absorb($Effects) vce(cluster $Clustering) noconstant
	quietly: eststo ModelBinaryL`lag'
	
	* Run continuous regression
	quietly: reghdfe CII_HollanderP FlightTimeReductionPositive $Controls if UsedObservation, absorb($Effects) vce(cluster $Clustering) noconstant
	quietly: eststo ModelContinuousL`lag'
}


* Label variables for table such that they look nicely formatted.
label var FasterFlight "\$FasterFlight\$"
label var FlightTimeReductionPositive "\$TimeReduction\$"


* Create the actual table
esttab ModelBinaryL1 ModelBinaryL2 ModelBinaryL3 ModelBinaryL4 ModelContinuousL1 ModelContinuousL2 ModelContinuousL3 ModelContinuousL4 using "${WORKING_DIR}Tables\AdditionalTable.tex", b(%5.4f) se(%5.4f) booktabs noomit keep(FasterFlight FlightTimeReductionPositive) replace label nodepvars nomtitles star(* 0.1 ** 0.05 *** 0.01) nonotes nolz stats(r2_a F N, fmt(%4.3f %4.3f %9.0fc) labels("Adj. \$R^2\$" "\$F\$-Statistic" "N")) nonumbers nomtitles nodepvars prehead(\renewcommand{\arraystretch}{1.2}\begin{tabular}{lcccccccc} ///
 \midrule \midrule ///
 & \multicolumn{4}{c}{\shortstack{Binary \\ Treatment}} & \multicolumn{4}{c}{\shortstack{Continuous \\ Treatment}} \\ ///
 \cmidrule(lr){2-5} \cmidrule(lr){6-9} ///
 & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\ ///
 ) posthead( ///
\midrule) postfoot(\midrule ///
Time (Year + Month) FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\ ///
Borrower Industry $\times$ Time FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\ ///
Borrower $\times$ Lender FE & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes  \\ ///
Credit Demand Controls & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\ ///
Considered Flights & \small{1 Months} & \small{2 Months}  & \small{3 Months}  & \small{4 Months} & \small{1 Months} & \small{2 Months} & \small{3 Months} & \small{4 Months} \\ ///
\midrule ///
Lead Arrangers & Yes & Yes & Yes & Yes & Yes & Yes & Yes & Yes \\ ///
Non-Lead Arrangers & No & No & No & No & No & No & No & No \\ ///
\midrule \midrule \end{tabular} \renewcommand{\arraystretch}{1} )




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    GENERATE TABLE 3@13
*####
********************************************************************************
* This code generates the Table 1 of our paper

* We already ensured by creating Table 2 that we have the same sample size, so 
* we will use this variable this time again

* For the non-leader we don't have such a variable, but we also don't need it
* the sample size is always the same


**** Regression for Binary Treatment on ... // COLUMN 1 TO 2
*** ... lead-arrangers only // COLUMN 1
quietly: reghdfe CII_HollanderP L3FasterFlight $Controls if UsedObservation, absorb($Effects) vce(cluster $Clustering) noconstant
quietly: eststo ModelBinaryLeader
* Basically we could also use the stored table from Table 1, but just for sake
* of completeness, we will do it again (for this column)

*** ... non-lead-arrangers only // COLUMN 2
quietly: reghdfe CII_HollanderP L3FasterFlight $Controls if L3SampleRestrictionNoLeaders, absorb($Effects) vce(cluster $Clustering) noconstant
quietly: eststo ModelBinaryNoLeader


**** Regression for Continuous Treatment on ... // COLUMN 3 TO 4
*** ... lead-arrangers only // COLUMN 3
quietly: reghdfe CII_HollanderP L3FlightTimeReductionPositive $Controls if UsedObservation, absorb($Effects) vce(cluster $Clustering) noconstant
quietly: eststo ModelContinuousLeader
* Basically we could also use the stored table from Table 1, but just for sake
* of completeness, we will do it again (for this column)

*** ... non-lead-arrangers only // COLUMN 4
quietly: reghdfe CII_HollanderP L3FlightTimeReductionPositive $Controls if L3SampleRestrictionNoLeaders, absorb($Effects) vce(cluster $Clustering) noconstant
quietly: eststo ModelContinuousNoLeader


* Label variables for table such that they look nicely formatted.
label var L3FasterFlight "\$FasterFlight\$"
label var L3FlightTimeReductionPositive "\$TimeReduction\$"


* Create the actual table
esttab ModelBinaryLeader ModelBinaryNoLeader ModelContinuousLeader ModelContinuousNoLeader using "${WORKING_DIR}Tables\MainTable.tex", b(%5.4f) se(%5.4f) booktabs noomit keep(L3FasterFlight L3FlightTimeReductionPositive) replace label star(* 0.1 ** 0.05 *** 0.01) nonotes nolz stats(r2_a F N, fmt(%4.3f %4.3f %9.0fc) labels("Adj. \$R^2\$" "\$F\$-Statistic" "N")) nonumbers nomtitles nodepvars prehead( \renewcommand{\arraystretch}{1.2} \begin{tabular}{lcccc} ///
 \midrule \midrule ///
 & \multicolumn{2}{c}{\shortstack{Binary \\ Treatment}} & \multicolumn{2}{c}{\shortstack{Continuous \\ Treatment}} \\ ///
 \cmidrule(lr){2-3} \cmidrule(lr){4-5} ///
 & (1) & (2) & (3) & (4) \\ ///
 ) posthead( ///
\midrule) postfoot(\midrule ///
Time (Year + Month) FE & Yes & Yes & Yes & Yes \\ ///
Borrower Industry $\times$ Time FE & Yes & Yes & Yes & Yes \\ ///
Borrower $\times$ Lender FE & Yes & Yes & Yes & Yes  \\ ///
Credit Demand Controls & Yes & Yes & Yes & Yes \\ ///
Considered Flights & \small{3 Months} & \small{3 Months} & \small{3 Months} & \small{3 Months} \\ ///
\midrule ///
Lead Arrangers & Yes & No & Yes & No \\ ///
Non-Lead Arrangers & No & Yes & No & Yes \\ ///
\midrule \midrule \end{tabular} \renewcommand{\arraystretch}{1} )




********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    DESCRIPTIVE TABLE // TABLE 2@14
*####
********************************************************************************


est clear

**********************************
*** SECTION: PACKAGE
* Transform deal amount (to million)
replace DealAmount = DealAmount / 1000000

* Label variables
*label variable CII_total "CII"
label variable CII_HollanderP " \hspace{3mm} Covenant Intensity Index"
label variable CovenantP " \hspace{3mm} P-Covenants (Share)"
label variable CovenantC " \hspace{3mm} C-Covenants (Share)"
label variable DealAmount " \hspace{3mm} Deal Amount (Million USD)"
label variable Participants " \hspace{3mm} Syndicate Participants"
label variable BorrowerLenderDistance " \hspace{3mm} Distance Borrower Lender (km)"
label variable RelationshiopTotalFacilities " \hspace{3mm} Packages per Constellation"
label variable Year " \hspace{3mm} Closing Year"
label variable Month " \hspace{3mm} Closing Month"

* CovenantP
eststo: quietly estpost summarize CovenantP if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", replace fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

* CovenantC
eststo: quietly estpost summarize CovenantC if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

* CII_total (CII for all covenants)
* -> We excluded it from the final table as it might be to confusing and adds no value
*eststo: quietly estpost summarize CII_total if UsedObservation, detail
*esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.12c)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
*est clear

* CII_HollanderP (CII - P-Covenants only)
eststo: quietly estpost summarize CII_HollanderP if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(label(N) fmt(%9.0fc)) mean(label(Mean) fmt(%9.2fc)) p50(label(Median) fmt(%9.2fc)) sd(label(SD)  fmt(%9.2fc)) min(label(Min)  fmt(%9.0fc)) max(label(Max)  fmt(%9.0fc))") prehead(" ") posthead(" ") label noobs nonumbers nomtitles nodepvars nonotes collabels(none)
est clear

* DealAmount
eststo: quietly estpost summarize DealAmount if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

* Participants
eststo: quietly estpost summarize Participants if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

* Borrower Lender Distance
eststo: quietly estpost summarize BorrowerLenderDistance if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

* Number of Packages Per Relationship
eststo: quietly estpost summarize RelationshiopTotalFacilities if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

* Closed Year 
eststo: quietly estpost summarize Year if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) p50(fmt(%9.0f)) sd(fmt(%9.0f)) min(fmt(%9.0f)) max(fmt(%9.0f))") label noobs nonumbers nolines nomtitles collabels(none)
est clear
* Closed Month  
eststo: quietly estpost summarize Month if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCovenants.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.0fc)) sd(fmt(%9.0fc)) min(fmt(%9.0f)) max(fmt(%9.0fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear




**********************************
*** SECTION: FLIGHT TIME
* Label variables
label variable L1FlightTime " \hspace{3mm} $\dots$ 1 Month before Closing (min)"
forvalues i = 2(1)4 {
	label variable L`i'FlightTime " \hspace{3mm} $\dots$ ${i} Months before Closing (min)"
}
eststo: quietly estpost summarize L1FlightTime if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableFlights.tex", replace fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

* Flight Time (L1 - L4)
forvalues i= 2/4 {
	eststo: quietly estpost summarize L`i'FlightTime if UsedObservation, detail
	esttab using "${WORKING_DIR}Tables\DescriptiveTableFlights.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
	est clear
}



**********************************
*** SECTION: TREATMENT
* Label variables
label variable L1FasterFlight " \hspace{3mm} $\dots$ 1 Month before Closing (min)"
label variable L1FlightTimeReductionPositive " \hspace{3mm} $\dots$ 1 Month before Closing (min)"
forvalues i = 2(1)4 {
	label variable L`i'FasterFlight " \hspace{3mm} $\dots$ ${i} Months before Closing (min)"
	label variable L`i'FlightTimeReductionPositive " \hspace{3mm} $\dots$ ${i} Months before Closing (min)"
}

* Treatment Variables: Faster Flight (L1 - L4)
eststo: quietly estpost summarize L1FlightTime if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableTreatmentFasterFlight.tex", replace fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

forvalues i= 2/4 {
	eststo: quietly estpost summarize L`i'FlightTime if UsedObservation, detail
	esttab using "${WORKING_DIR}Tables\DescriptiveTableTreatmentFasterFlight.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
	est clear
}
* Treatment Variables: Time Reduction (L1 - L4)
eststo: quietly estpost summarize L1FlightTimeReductionPositive if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableTreatmentTimeReduction.tex", replace fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear

forvalues i= 2/4 {
	eststo: quietly estpost summarize L`i'FlightTimeReductionPositive if UsedObservation, detail
	esttab using "${WORKING_DIR}Tables\DescriptiveTableTreatmentTimeReduction.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
	est clear
}



**********************************
*** SECTION: CREDIT DEMAND
* Label variables
label variable L0StateUnemployment " \hspace{3mm} Current unemployment rate (state)"
label variable L0PerCapitalPersonalIncome "  \hspace{3mm} Current personal income (county, per capital USD)"
label variable L0TotalEmployment " \hspace{3mm} Current employment (county, number)"
label variable L0AverageWagesAndSalaries " \hspace{3mm} Current wages and salaries (county, USD)"

* L0StateUnemployment     
eststo: quietly estpost summarize L0StateUnemployment if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCreditControls.tex", replace fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear
* L0PerCapitalPersonalIncome  
eststo: quietly estpost summarize L0PerCapitalPersonalIncome if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCreditControls.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear
* L0TotalEmployment  
eststo: quietly estpost summarize L0TotalEmployment if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCreditControls.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear
* L0AverageWagesAndSalaries 
eststo: quietly estpost summarize L0AverageWagesAndSalaries if UsedObservation, detail
esttab using "${WORKING_DIR}Tables\DescriptiveTableCreditControls.tex", append fragment cells("count(fmt(%9.0fc)) mean(fmt(%9.2fc)) p50(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(%9.2fc)) max(fmt(%9.2fc))") label noobs nonumbers nolines nomtitles collabels(none)
est clear





********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    EXPORT CURRENT DATASET FOR GRAPHS IN R@15
*####
********************************************************************************

* We need to export this data set, since R does not known we observation it
* may use for its plots. We just want tot plot those used observations
* -> We basically just need the UsedObservation-indicator
save "${WORKING_DIR}\Output\For Non-Essential Graphs.dta", replace

* The data set is used for the following graphs
* - Visualise Flight Routing.R
* - Map Distribution Lenders.R



********************************************************************************
*###############################################################################
*###############################################################################
*####
*####    ADD GRAPHS@16
*####
********************************************************************************
* Below are all graphs we use in our paper and presentations (Besides of Lender
* location distribtion and other flight route visualisation)


** Figure name: Parallel Trend Assumption
********************************************************************************
quietly: sort RelationshipID FacilityStartDate
quietly: by RelationshipID: gen BeforeFirstTreatment = 0
quietly: by RelationshipID: replace BeforeFirstTreatment = 1 if _n == 1
quietly: by RelationshipID: replace BeforeFirstTreatment = 1 if L3FasterFlight == 0 & BeforeFirstTreatment[_n - 1] == 1
quietly: by RelationshipID: egen ControlGroup = min(BeforeFirstTreatment)



sort ControlGroup Year
quietly: by ControlGroup Year: egen MeanCII = mean(CII_HollanderP) if L3SampleRestrictionSimple == 1 & BeforeFirstTreatment == 1


twoway (line MeanCII Year if L3SampleRestrictionSimple == 1 & ControlGroup == 1 & Year>=1993 , legend(label(1 "Control Group")) lcol(navy)) (line MeanCII Year if ControlGroup == 0 & L3SampleRestrictionSimple == 1& Year>=1993 , legend(label(2 "Treatment Group")) lcol(red)) , xlabel(1994(5)2018) xline(1996, lcol(emidblue) lpattern(dash)) xline(2018, lcol(emidblue) lpattern(dash)) graphregion(color(white)) ytitle("Mean(CII)", margin(medium)) xtitle("Year", margin(medium))




** Figure name: Research Question Illustration
********************************************************************************

drop MeanCII
gen Treated = BeforeFirstTreatment == 0

sort Treated Year
by Treated Year: egen MeanCII = mean(CII_HollanderP) if UsedObservation == 1

twoway (line MeanCII Year if UsedObservation == 1 & Treated == 1, legend(label(1 "Treated")) lcol(navy)) (line MeanCII Year if UsedObservation == 1 & Treated == 0, legend(label(2 "Untreated")) lcol(red)), xlabel(1996(5)2018) ytitle("Mean(CII)", margin(medium)) xtitle("Year", margin(medium)) graphregion(color(white))




** Figure name: Distribution of the CII
********************************************************************************
hist CII_HollanderP if UsedObservation == 1, width(0.5) color(navy) graphregion(color(white)) ytitle("Density", margin(medium)) xtitle("CII", margin(medium)) 


** Figure name: Flight Time Distribution
********************************************************************************
hist L3FlightTime if UsedObservation == 1, color(navy) lcolor(white) barwidth(12) graphregion(color(white)) ytitle("Density", margin(medium)) xtitle("Flight Time", margin(medium)) 



** Figure name: Flight Time Reduction
********************************************************************************

hist L3FlightTimeReductionPositive if UsedObservation == 1, bin(40) color(navy) lcolor(white) graphregion(color(white)) ytitle("Density", margin(medium)) xtitle("Flight Time Reduction", margin(medium)) xline(10, lcol(red) lpattern(dash) lwidth(medthick))


** Figure name: Flight Time Reduction (10 minutes)
********************************************************************************
hist L3FlightTimeReductionPositive if UsedObservation == 1 & L3FlightTimeReductionPositive > 10,  bin(35) graphregion(color(white)) color(navy) lcolor(white) ytitle("Density", margin(medium)) xtitle("Flight Time Reduction", margin(medium)) xline(10, lcol(red) lpattern(dash) lwidth(thick)) 
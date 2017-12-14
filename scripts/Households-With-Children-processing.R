library(dplyr)
library(datapkg)
library(acs)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Households With Children
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

# ACS B25115
# Get geography object for CT and subcounty divisions

options(scipen=999)
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2016,
    table = "B25115"
)

dataset <- data.table()
for (data in acsdata) {
    year <- data@endyear
    message("Processing: ", year)
    year <- paste(year-4, year, sep="-")

    total <- data[,1]
    acs.colnames(total) <- "Total Households"

    owner.married.children <- data[,5]
    acs.colnames(owner.married.children) <- "number.owner.married.children"
    percent.owner.married.children <- divide.acs(owner.married.children, total, method="proportion")
    acs.colnames(percent.owner.married.children) <- "percent.owner.married.children"

    owner.married.no <- data[,6 ]
    acs.colnames(owner.married.no) <- "number.owner.married.no"
    percent.owner.married.no <- divide.acs(owner.married.no, total, method="proportion")
    acs.colnames(percent.owner.married.no) <- "percent.owner.married.no"

    owner.male.children <- data[,9]
    acs.colnames(owner.male.children) <- "number.owner.male.children"
    percent.owner.male.children <- divide.acs(owner.male.children, total, method="proportion")
    acs.colnames(percent.owner.male.children) <- "percent.owner.male.children"

    owner.male.no <- data[,10]
    acs.colnames(owner.male.no) <- "number.owner.male.no"
    percent.owner.male.no <- divide.acs(owner.male.no, total, method="proportion")
    acs.colnames(percent.owner.male.no) <- "percent.owner.male.no"

    owner.female.children <- data[,12]
    acs.colnames(owner.female.children) <- "number.owner.female.children"
    percent.owner.female.children <- divide.acs(owner.female.children, total, method="proportion")
    acs.colnames(percent.owner.female.children) <- "percent.owner.female.children"

    owner.female.no <- data[,13]
    acs.colnames(owner.female.no) <- "number.owner.female.no"
    percent.owner.female.no <- divide.acs(owner.female.no, total, method="proportion")
    acs.colnames(percent.owner.female.no) <- "percent.owner.female.no"

    owner.nonfamily <- data[,14]
    acs.colnames(owner.nonfamily) <- "number.owner.nonfamily"
    percent.owner.nonfamily <- divide.acs(owner.nonfamily, total, method="proportion")
    acs.colnames(percent.owner.nonfamily) <- "percent.owner.nonfamily"

    owner.total.children <- acsSum(data, c(5,9,12), "number.owner.total.children")
    acs.colnames(owner.total.children) <- "number.owner.total.children"
    percent.owner.total.children <- divide.acs(owner.total.children, total, method="proportion")
    acs.colnames(percent.owner.total.children) <- "percent.owner.total.children"

    owner.total.no <- acsSum(data, c(6,10,13), "number.owner.total.no")
    acs.colnames(owner.total.no) <- "number.owner.total.no"
    percent.owner.total.no <- divide.acs(owner.total.no, total, method="proportion")
    acs.colnames(percent.owner.total.no) <- "percent.owner.total.no"

    # Renter population
    renter.married.children <- data[,18]
    acs.colnames(renter.married.children) <- "number.renter.married.children"
    percent.renter.married.children <- divide.acs(renter.married.children, total, method="proportion")
    acs.colnames(percent.renter.married.children) <- "percent.renter.married.children"

    renter.married.no <- data[,19]
    acs.colnames(renter.married.no) <- "number.renter.married.no"
    percent.renter.married.no <- divide.acs(renter.married.no, total, method="proportion")
    acs.colnames(percent.renter.married.no) <- "percent.renter.married.no"

    renter.male.children <- data[,22]
    acs.colnames(renter.male.children) <- "number.renter.male.children"
    percent.renter.male.children <- divide.acs(renter.male.children, total, method="proportion")
    acs.colnames(percent.renter.male.children) <- "percent.renter.male.children"

    renter.male.no <- data[,23]
    acs.colnames(renter.male.no) <- "number.renter.male.no"
    percent.renter.male.no <- divide.acs(renter.male.no, total, method="proportion")
    acs.colnames(percent.renter.male.no) <- "percent.renter.male.no"

    renter.female.children <- data[,25]
    acs.colnames(renter.female.children) <- "number.renter.female.children"
    percent.renter.female.children <- divide.acs(renter.female.children, total, method="proportion")
    acs.colnames(percent.renter.female.children) <- "percent.renter.female.children"

    renter.female.no <- data[,26]
    acs.colnames(renter.female.no) <- "number.renter.female.no"
    percent.renter.female.no <- divide.acs(renter.female.no, total, method="proportion")
    acs.colnames(percent.renter.female.no) <- "percent.renter.female.no"

    renter.nonfamily <- data[,27]
    acs.colnames(renter.nonfamily) <- "number.renter.nonfamily"
    percent.renter.nonfamily <- divide.acs(renter.nonfamily, total, method="proportion")
    acs.colnames(percent.renter.nonfamily) <- "percent.renter.nonfamily"

    renter.total.children <- acsSum(data, c(18,22,25), "number.renter.total.children")
    acs.colnames(renter.total.children) <- "number.renter.total.children"
    percent.renter.total.children <- divide.acs(renter.total.children, total, method="proportion")
    acs.colnames(percent.renter.total.children) <- "percent.renter.total.children"

    renter.total.no <- acsSum(data, c(19,23,26), "number.renter.total.no")
    acs.colnames(renter.total.no) <- "number.renter.total.no"
    percent.renter.total.no <- divide.acs(renter.total.no, total, method="proportion")
    acs.colnames(percent.renter.total.no) <- "percent.renter.total.no"

    # start building data tables
    datafips <- data.table(fips = getACSFips(data))

    # Cast to separate data frames
    numberEstimates <- data.table(
        datafips$fips,
        estimate(owner.married.children),
        estimate(owner.married.no),
        estimate(owner.male.children),
        estimate(owner.male.no),
        estimate(owner.female.children),
        estimate(owner.female.no),
        estimate(owner.nonfamily),
        estimate(owner.total.children),
        estimate(owner.total.no),
        estimate(renter.married.children),
        estimate(renter.married.no),
        estimate(renter.male.children),
        estimate(renter.male.no),
        estimate(renter.female.children),
        estimate(renter.female.no),
        estimate(renter.nonfamily),
        estimate(renter.total.children),
        estimate(renter.total.no),
        year,
        "Number",
        "Households"
    )
    numberMOES <- data.table(
        datafips$fips,
        standard.error(owner.married.children) * 1.645,
        standard.error(owner.married.no) * 1.645,
        standard.error(owner.male.children) * 1.645,
        standard.error(owner.male.no) * 1.645,
        standard.error(owner.female.children) * 1.645,
        standard.error(owner.female.no) * 1.645,
        standard.error(owner.nonfamily) * 1.645,
        standard.error(owner.total.children) * 1.645,
        standard.error(owner.total.no) * 1.645,
        standard.error(renter.married.children) * 1.645,
        standard.error(renter.married.no) * 1.645,
        standard.error(renter.male.children) * 1.645,
        standard.error(renter.male.no) * 1.645,
        standard.error(renter.female.children) * 1.645,
        standard.error(renter.female.no) * 1.645,
        standard.error(renter.nonfamily) * 1.645,
        standard.error(renter.total.children) * 1.645,
        standard.error(renter.total.no) * 1.645,
        year,
        "Number",
        "Margins of Error"
    )
    numberNames <- c(
        "FIPS",
        "owner:married:children",
        "owner:married:no",
        "owner:male:children",
        "owner:male:no",
        "owner:female:children",
        "owner:female:no",
        "owner:nonfamily",
        "owner:total:children",
        "owner:total:no",
        "renter:married:children",
        "renter:married:no",
        "renter:male:children",
        "renter:male:no",
        "renter:female:children",
        "renter:female:no",
        "renter:nonfamily",
        "renter:total:children",
        "renter:total:no",
        "Year",
        "Measure Type",
        "Variable"
    )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)

    numbersData.melt <- melt(
        rbind(numberEstimates, numberMOES),
        id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name="Category",
        variable.factor = F,
        value.name="Value",
        value.factor = F
    )

    percentEstimates <- data.table(
        datafips$fips,
        estimate(percent.owner.married.children),
        estimate(percent.owner.married.no),
        estimate(percent.owner.male.children),
        estimate(percent.owner.male.no),
        estimate(percent.owner.female.children),
        estimate(percent.owner.female.no),
        estimate(percent.owner.nonfamily),
        estimate(percent.owner.total.children),
        estimate(percent.owner.total.no),
        estimate(percent.renter.married.children),
        estimate(percent.renter.married.no),
        estimate(percent.renter.male.children),
        estimate(percent.renter.male.no),
        estimate(percent.renter.female.children),
        estimate(percent.renter.female.no),
        estimate(percent.renter.nonfamily),
        estimate(percent.renter.total.children),
        estimate(percent.renter.total.no),
        year,
        "Percent",
        "Households"
    )
    percentMOES <- data.table(
        datafips$fips,
        standard.error(percent.owner.married.children) * 1.645,
        standard.error(percent.owner.married.no) * 1.645,
        standard.error(percent.owner.male.children) * 1.645,
        standard.error(percent.owner.male.no) * 1.645,
        standard.error(percent.owner.female.children) * 1.645,
        standard.error(percent.owner.female.no) * 1.645,
        standard.error(percent.owner.nonfamily) * 1.645,
        standard.error(percent.owner.total.children) * 1.645,
        standard.error(percent.owner.total.no) * 1.645,
        standard.error(percent.renter.married.children) * 1.645,
        standard.error(percent.renter.married.no) * 1.645,
        standard.error(percent.renter.male.children) * 1.645,
        standard.error(percent.renter.male.no) * 1.645,
        standard.error(percent.renter.female.children) * 1.645,
        standard.error(percent.renter.female.no) * 1.645,
        standard.error(percent.renter.nonfamily) * 1.645,
        standard.error(percent.renter.total.children) * 1.645,
        standard.error(percent.renter.total.no) * 1.645,
        year,
        "Percent",
        "Margins of Error"
    )
    percentNames <- c(
        "FIPS",
        "owner:married:children",
        "owner:married:no",
        "owner:male:children",
        "owner:male:no",
        "owner:female:children",
        "owner:female:no",
        "owner:nonfamily",
        "owner:total:children",
        "owner:total:no",
        "renter:married:children",
        "renter:married:no",
        "renter:male:children",
        "renter:male:no",
        "renter:female:children",
        "renter:female:no",
        "renter:nonfamily",
        "renter:total:children",
        "renter:total:no",
        "Year",
        "Measure Type",
        "Variable"
    )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)

    percentData.melt <- melt(
        rbind(percentEstimates, percentMOES),
        id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
        variable.name="Category",
        variable.factor = F,
        value.name="Value",
        value.factor = F
    )
    dataset <- rbind(dataset, numbersData.melt, percentData.melt)
}

#Final Additions, processing
dataset[,c("Housing Tenure Type", "family", "children"):=do.call(Map, c(f = c, strsplit(Category, ":", fixed=T)))]

# fix children labels
dataset[,`:=`(
        children = ifelse(children=="children", "with own children", "without own children"),
        Category = NULL
    )]

# rename household types
dataset[,`:=`(
        `Household Type` = switch(
            family,
            married = "Married-couple family",
            male = "Male householder, no wife present,",
            female = "Female householder, no husband present,",
            nonfamily = "Nonfamily household",
            total = "Total households"
        )
    ), by = family, mult="first"]

dataset[,`:=`(
        `Housing Tenure Type` = paste(str_to_title(`Housing Tenure Type`), "Occupied"),
        `Household Type` = paste(`Household Type`, children),
        Value = ifelse(`Measure Type` == "Number", round(Value, 0), round(Value*100, 2)),
        family = NULL,
        children = NULL
    )]

# Join town names by FIPS code
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

#set final column order
dataset <- dataset %>% 
  select(Town, FIPS, Year, `Household Type`, `Housing Tenure Type`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Household Type`, `Housing Tenure Type`, `Measure Type`, Variable)

write.table(
  dataset,
  file.path("data", "households_with_children-2016.csv"),
  sep = ",",
  row.names=F,
  na = "-9999"
)

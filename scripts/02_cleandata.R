# File name: 02_cleandata.R
# Path: './scripts/02_cleandata.R'

# Author: NK Chan
# Purpose: Cleans data by searching for outliers and missing values

source(paste0(getwd(), "/scripts/01_loaddata.R"))

# Select columns with quantitative data (i.e., contains numeric values or data
# that can be coerced to numerics). Note that categorical data are permissible
# as categories can be read in as factors (and are thus coercible to numeric).
# Thus, any qualitative data (e.g. short responses/strings) are excluded.
coviddata_num <- coviddata[, which(sapply(coviddata, is.numeric))]

# Make a data frame listing the question and values for each column of
# coviddata_num
datadict <- sapply(coviddata, get_var_info, simplify = F)

# Make it easier to work with the dataset
attach(coviddata_num)

# Timepoint variables are actually boolean; replace NA with 0 in each Timepoint
coviddata_num[-which(Timepoint_1 == 1), "Timepoint_1"] <- 0
coviddata_num[-which(Timepoint_2 == 1), "Timepoint_2"] <- 0
coviddata_num[-which(Timepoint_3 == 1), "Timepoint_3"] <- 0

# Not all questions are asked at each Timepoint. Moreover, outlier analysis will
# require complete cases. We will need to exclude questions containing only NA
# values by Timepoint. In order to save as many variables as possible, we should
# split the data by Timepoint, then determine the variables with NAs in at each
# Timepoint.
vars_na1 <-
  coviddata_num[which(Timepoint_1 == 1),] %>%
  .[, which(sapply(., function(x) all(is.na(x))))] %>%
  names()
vars_na2 <-
  coviddata_num[which(Timepoint_2 == 1),] %>%
  .[, which(sapply(., function(x) all(is.na(x))))] %>%
  names()
vars_na3 <-
  coviddata_num[which(Timepoint_3 == 1),] %>%
  .[, which(sapply(., function(x) all(is.na(x))))] %>%
  names()

# We can deal with the variables containing only NA variables for some Timepoint
# later. For now, let's combine them then exclude them from the sifting step
# in the next code block.
vars_naall <- unique(c(vars_na1, vars_na2, vars_na3))
coviddata_exclNA <- coviddata_num[, -which(names(coviddata_num) %in% vars_naall)]
detach(coviddata_num)
attach(coviddata_exclNA)

# Sift through datadict_exclNA and categorize each variable by type (i.e., 
# categorical, ordinal, continuous, has NA or missing values)
datadict_exclNA <- sapply(coviddata_exclNA, get_var_info, simplify = F)

vars_cat <- bind_cols(
  "Timepoint_1"     = Timepoint_1,      # Identifier for time when survey collected
  "Timepoint_2"     = Timepoint_2,      # Identifier for time when survey collected
  "Timepoint_3"     = Timepoint_3,      # Identifier for time when survey collected
  "S6A"             = S6A,              # Relationship status
  "S6D"             = S6D,              # Health care worker (y/n)
  "RF1r1"           = RF1r1,            # Have heart disease (y/n)
  "RF1r2"           = RF1r2,            # Have hypertension (y/n)
  "RF1r3"           = RF1r3,            # Have lung disease (y/n)
  "RF1r4"           = RF1r4,            # Have diabetes (y/n)
  "RF1r5"           = RF1r5,            # Have cancer (y/n)
  "RF1r6"           = RF1r6,            # Have chronic kidney disease (y/n)
  "RF1r7"           = RF1r7,            # Have obesity (y/n)
  "RF2"             = RF2,              # Have weakened immune system (y/n)
  "ASr1"            = ASr1,             # Have fever/chills/shakes (y/n)
  "ASr2"            = ASr2,             # Have cough (y/n)
  "ASr3"            = ASr3,             # Have shortness of breath (y/n)
  "ASr4"            = ASr4,             # Have tired or fatigued(y/n)
  "ASr5"            = ASr5,             # Have lost appetite (y/n)
  "ASr6"            = ASr6,             # Have muscle aches and pains (y/n)
  "ASr7"            = ASr7,             # Have nasal congestion (y/n)
  "ASr8"            = ASr8,             # Have sore throat (y/n)
  "ASr9"            = ASr9,             # Have excessive sputum (y/n)
  "ASr10"           = ASr10,            # Have lost smell (y/n)
  "ASr11"           = ASr11,            # Have lost taste (y/n)
  "ASr12"           = ASr12,            # Have diarrhea (y/n)
  "ASr13"           = ASr13,            # Have other symptoms (y/n)
  "C1"              = C1,               # Ever tested for COVID-19 (y/n)
  "CVSQ1"           = CVSQ1,            # Source of health information (incl. other)
  "CVSQ2"           = CVSQ2,            # Belief of origin of COVID-19 (incl. other)
  "CVSQ3"           = CVSQ3,            # Had COVID/what was outcome
  "CVSQCVSQ4"       = CVSQCVSQ4,        # Close family member or friend that is health care worker (y/n)
  "CVSQCVSQ5"       = CVSQCVSQ5,        # Close family member or friend that is high risk (y/n)
  "CVSQCVSQ6"       = CVSQCVSQ6,        # Close family member or friend in senior's residence (y/n)
  "CVSQCVSQ7"       = CVSQCVSQ7,        # Close family member or friend in long-term care (y/n)
  "noanswerS15_r0"  = noanswerS15_r0,   # cat; Number of drinks in last 7 days (non-drinkers)
  "noanswerS15_r1"  = noanswerS15_r1,   # cat; Number of drinks in last 7 days (less than 1 per week)
  "noanswerS15_r2"  = noanswerS15_r2,   # cat; Number of drinks in last 7 days (it depends)
  "S16A"            = S16A,             # Use of cannabis products (combine with below)
  "noanswerS17A_r0" = noanswerS17A_r0,  # cat; Number of cigarettes (non-smokers)
  "noanswerS17A_r1" = noanswerS17A_r1,  # cat; Number of cigarettes (less than 1 per day)
  "noanswerS17A_r2" = noanswerS17A_r2,  # con; Number of cigarettes (it depends)
  "noanswerS17B_r0" = noanswerS17B_r0,  # cat; Number of e-cigarettes (non-users)
  "noanswerS17B_r1" = noanswerS17B_r1,  # cat; Number of e-cigarettes (less than 1 per day)
  "noanswerS17B_r2" = noanswerS17B_r2   # cat; Number of e-cigarettes (it depends)
)

vars_ord <- bind_cols(
  "S10A"          = S10A,                 # Highest level of education
  "S8x1S8A"       = S8x1S8A,              # Income reduced by COVID-19
  "S8x1S8B"       = S8x1S8B,              # In financial distress
  "S8x1S8C"       = S8x1S8C,              # Trouble making ends meet
  "SPFQr1"        = SPFQr1,               # Comply with social distancing (avoid groups)
  "SPFQr2"        = SPFQr2,               # Comply with social distancing (maintain 6ft distance in community)
  "SPFQr3"        = SPFQr3,               # Comply with social distancing (keep distance from people at risk)
  "SPQr1"         = SPQr1,                # Stay at home unless for work
  "SPQr2"         = SPQr2,                # Avoid non-essential trips
  "SPQr3"         = SPQr3,                # Keep distance if leaving home
  "SPQr4"         = SPQr4,                # Limit close contacts with people at high risk
  "SPQr5"         = SPQr5,                # Avoid crowded places
  "SPQr6"         = SPQr6,                # Avoid common greetings (hugging and kissing)
  "SPQr7"         = SPQr7,                # Disinfect hands often
  "SPQr8"         = SPQr8,                # Wash or disinfect hands for at least 20 seconds
  "SPQr9"         = SPQr9,                # Cough or sneeze into bend of arm
  "SPQr10"        = SPQr10,               # Avoid touching eyes, nose, mouth
  "SPQ11A"        = SPQ11A,               # Exercise frequency
  "SSSQr1"        = SSSQr1,               # Easily influenced by people's opinions
  "SSSQr2"        = SSSQr2,               # Easily influenced by commercials
  "SSSQr3"        = SSSQr3,               # Likely to cough or sneeze when seeing others do so
  "SSSQr4"        = SSSQr4,               # Imagining drink can make me thirsty
  "SSSQr5"        = SSSQr5,               # Salesperson can make me want their product
  "SSSQr6"        = SSSQr6,               # Practical advice from magazines or TV
  "SSSQr7"        = SSSQr7,               # Nicely displayed product makes me want to buy
  "SSSQr8"        = SSSQr8,               # Seeing shiver makes me feel a chill
  "SSSQr9"        = SSSQr9,               # Get style from celebrities
  "SSSQr10"       = SSSQr10,              # Feel same feelings when others share feelings
  "SSSQr11"       = SSSQr11,              # Follow other people's advice when making decisions
  "SSSQr12"       = SSSQr12,              # Reading tasty descriptions of dishes makes mouth water
  "SSSQr13"       = SSSQr13,              # Get good ideas from others
  "SSSQr14"       = SSSQr14,              # Frequently change opinions after talking with others
  "SSSQr15"       = SSSQr15,              # Lotion commercials make skin feel dry
  "SSSQr16"       = SSSQr16,              # Discover favorite things from friends
  "SSSQr17"       = SSSQr17,              # Follow current fashion trends
  "SSSQr18"       = SSSQr18,              # Thinking about scary makes heart pound
  "SSSQr19"       = SSSQr19,              # Pick up habits from friends
  "SSSQr20"       = SSSQr20,              # Feel ill if told don't look well
  "SSSQr21"       = SSSQr21,              # Important to fit in
  "RQr1"          = RQr1,                 # Saftey first
  "RQr2"          = RQr2,                 # Do not take risks with my health
  "RQr3"          = RQr3,                 # Avoid risks
  "RQr4"          = RQr4,                 # Regularly take risks
  "RQr5"          = RQr5,                 # Dislike not knowing what is going to happen
  "RQr6"          = RQr6,                 # View risks as a challenge
  "RQ7"           = RQ7,                  # Risk avoider vs risk seeker
  "DRIQInfor1"    = DRIQInfor1,           # Experience presence of divine
  "DRIQInfor2"    = DRIQInfor2,           # Religious/spiritual beliefs lie behind whole approach to life
  "DRIQInfor3"    = DRIQInfor3,           # Try hard to carry religious/spiritual beliefs into all dealings in life
  "TIPIQr1"       = TIPIQr1,              # Extroverted or enthusiastic
  "TIPIQr2"       = TIPIQr2,              # Critical or quarrelsome
  "TIPIQr3"       = TIPIQr3,              # Dependable or self-disciplined
  "TIPIQr4"       = TIPIQr4,              # Anxious or easily upset
  "TIPIQr5"       = TIPIQr5,              # Open to new experiences or complex
  "TIPIQr6"       = TIPIQr6,              # Reserved or quiet
  "TIPIQr7"       = TIPIQr7,              # Sympathetic or warm
  "TIPIQr8"       = TIPIQr8,              # Disorganized or careless
  "TIPIQr9"       = TIPIQr9,              # Calm or emotionally stable
  "TIPIQr10"      = TIPIQr10,             # Conventional or un-creative
  "CTQr1"         = CTQr1,                # All levels of government are capable concerning COVID-19
  "CTQr2"         = CTQr2,                # All levels of government are experts concerning COVID-19
  "CTQr3"         = CTQr3,                # All levels of government carry duties out well concerning COVID-19
  "CTQr4"         = CTQr4,                # All levels of government will do their best to help citizens concerning COVID-19
  "CTQr5"         = CTQr5,                # All levels of government act in the interest of citizens concerning COVID-19
  "CTQr6"         = CTQr6,                # All levels of government are interested in the well-being of citizens concerning COVID-19
  "CTQr7"         = CTQr7,                # All levels of government approach citizens in sincere ways concerning COVID-19
  "CTQr8"         = CTQr8,                # All levels of government are honest concerning COVID-19
  "HCQr1"         = HCQr1,                # Positive thinking can help fight off minor illness
  "HCQr2"         = HCQr2,                # Alternative/holistic medicine should be subject to more scientific testing
  "HCQr3"         = HCQr3,                # Important to be careful about lifestyle when stressed
  "HCQr4"         = HCQr4,                # Alternative/holistic medicine can be dangerous by preventing people getting proper treatment
  "HCQr5"         = HCQr5,                # Symptoms of illness made worse by depression
  "HCQr6"         = HCQr6,                # Alternative/holistic medicine used only as last resort
  "HCQr7"         = HCQr7,                # Stressful life events increase likelihood of illness
  "HCQr8"         = HCQr8,                # Worthwhile to try complementary medicine before seeing doctor
  "HCQr9"         = HCQr9,                # Conflict with others has no effect on health
  "HCQr10"        = HCQr10,               # Alternative/holistic medicine only used for minor ailments and not serious illness
  "HCQr11"        = HCQr11,               # Important to find balance between work and relaxation to stay healthy
  "HCQr12"        = HCQr12,               # Alternative/holistic medicine builds up body's defenses leading to permanent cure
  "LCQr1"         = LCQr1,                # Life is determined by own actions
  "LCQr2"         = LCQr2,                # Able to protect personal interests
  "LCQr3"         = LCQr3,                # Can determine what will happen in my life
  "LCQr4"         = LCQr4,                # Life controlled by accidental findings
  "LCQr5"         = LCQr5,                # No chance of protecting personal interest from bad luck
  "LCQr6"         = LCQr6,                # I get what I want because I'm lucky
  "LCQr7"         = LCQr7,                # Little chance of protecting personal interests when conflict with strong pressure groups
  "LCQr8"         = LCQr8,                # Life chiefly controlled by powerful others
  "LCQr9"         = LCQr9,                # Feel like what happens in life determined by powerful people
  "VAXQr1"        = VAXQr1,               # Feel safe after vaccination
  "VAXQr2"        = VAXQr2,               # Rely on vaccines to stop serious infectious diseases
  "VAXQr3"        = VAXQr3,               # Feel protected after getting vaccinated
  "VAXQr4"        = VAXQr4,               # Vaccines appear safe but have problems not yet discovered
  "VAXQr5"        = VAXQr5,               # Vaccines can cause unforeseen problems in children
  "VAXQr6"        = VAXQr6,               # Worry about unknown effects of vaccines in future
  "VAXQr7"        = VAXQr7,               # Vaccines make a lot of money for pharmaceutical companies
  "VAXQr8"        = VAXQr8,               # Authorities promote vaccination for financial gain not health
  "VAXQr9"        = VAXQr9,               # Vaccination programs are a big con
  "VAXQr10"       = VAXQr10,              # Natural immunity lasts longer than vaccination
  "VAXQr11"       = VAXQr11,              # Natural exposure to viruses and germs gives safest protection
  "VAXQr12"       = VAXQr12,              # Exposed to diseases naturally safer for immune system vs vaccination
  "GTSQr1"        = GTSQr1,               # Most people are basically honest
  "GTSQr2"        = GTSQr2,               # Most people are trustworthy
  "GTSQr3"        = GTSQr3,               # Most people are basically good and kind
  "GTSQr4"        = GTSQr4,               # Most people are trustful of others
  "GTSQr5"        = GTSQr5,               # I am trustful
  "GTSQr6"        = GTSQr6,               # Most people respond in kind when they are trusted by others
  "PAQ1"          = PAQ1,                 # Political spectrum (communism to fascism)
  "ABQr1"         = ABQr1,                # Listen to older people about how to behave
  "ABQr2"         = ABQr2,                # Question judgment of umpires or referees
  "ABQr3"         = ABQr3,                # Do what authoritative figures tell you to do
  "ABQr4"         = ABQr4,                # Criticize people who are rude to superiors
  "ABQr5"         = ABQr5,                # Encourage young people to do what they want even if against parents wishes
  "ABQr6"         = ABQr6,                # Dress to be acceptable to people that run the place
  "ABQr7"         = ABQr7,                # Treat experts with respect even when you don't think much of them personally
  "ABQr8"         = ABQr8,                # Support left-wing radical policies
  "ABQr9"         = ABQr9,                # Take part in demonstrations to show opposition to policies you do not like
  "ABQr10"        = ABQr10,               # Express approval for work of school teachers
  "ABQr11"        = ABQr11,               # Go to church
  "ABQr12"        = ABQr12,               # Make fun of police
  "ABQr13"        = ABQr13,               # Look for guidance from someone wiser
  "ABQr14"        = ABQr14,               # Sympathize with rebels
  "ABQr15"        = ABQr15,               # Break speed limit or encourage driver to do so if safe when in hurry
  "ABQr16"        = ABQr16,               # Follow doctor's orders
  "ABQr17"        = ABQr17,               # Question what you hear on news
  "ABQr18"        = ABQr18,               # Cross road against pedestrian traffic lights
  "ABQr19"        = ABQr19,               # Ask for a second opinion when uncertain about doctor's advice
  "ABQr20"        = ABQr20,               # Stand when playing national anthem in public
  "ABQr21"        = ABQr21,               # Express contempt for politicians
  "ABQr22"        = ABQr22,               # Annoyed when people sneer at those in authority
  "ABQr23"        = ABQr23,               # Show special respect for people in high positions
  "ABQr24"        = ABQr24,               # Speak up against boss when acting unfairly
  "CVSQ8_Q9CVSQ8" = CVSQ8_Q9CVSQ8,        # Willing to get vaccinated against COVID-19
  "PANAS_1"       = PANAS_1,              # Interested
  "PANAS_2"       = PANAS_2,              # Distressed
  "PANAS_3"       = PANAS_3,              # Excited
  "PANAS_4"       = PANAS_4,              # Upset
  "PANAS_5"       = PANAS_5,              # Strong
  "PANAS_6"       = PANAS_6,              # Guilty
  "PANAS_7"       = PANAS_7,              # Scared
  "PANAS_8"       = PANAS_8,              # Hostile
  "PANAS_9"       = PANAS_9,              # Enthusiastic
  "PANAS_10"      = PANAS_10,             # Proud
  "PANAS_11"      = PANAS_11,             # Irritable
  "PANAS_12"      = PANAS_12,             # Alert
  "PANAS_13"      = PANAS_13,             # Ashamed
  "PANAS_14"      = PANAS_14,             # Inspired
  "PANAS_15"      = PANAS_15,             # Nervous
  "PANAS_16"      = PANAS_16,             # Determined
  "PANAS_17"      = PANAS_17,             # Attentive
  "PANAS_18"      = PANAS_18,             # Jittery
  "PANAS_19"      = PANAS_19,             # Active
  "PANAS_20"      = PANAS_20,             # Afraid
  "ECR_1"         = ECR_1,                # Uncomfortable when other people want to be close
  "ECR_2"         = ECR_2,                # Worry about feeling abandoned
  "ECR_3"         = ECR_3,                # Tell people whom I feel close just about everything
  "ECR_4"         = ECR_4,                # Need reassurance that I am loved
  "ECR_5"         = ECR_5,                # Don't feel comfortable opening up to other people
  "ECR_6"         = ECR_6,                # Worry a lot about relationships
  "ECR_7"         = ECR_7,                # Discuss problems and concerns with people close to me
  "ECR_8"         = ECR_8,                # Find that other people don't want to get as close as I would like
  "ECR_9"         = ECR_9,                # Avoid getting too close to other people
  "ECR_10"        = ECR_10,               # Worry that other people won't care about me as much as I care about them
  "ECR_11"        = ECR_11,               # Don't mind asking other people about comfort, advice, or help
  "ECR_12"        = ECR_12,               # Get frustrated when other people are not around as much as I would like
  "ECR_13"        = ECR_13,               # Prefer not to be too close to other people
  "ECR_14"        = ECR_14,               # Worry a fair about about losing people I feel close to
  "ECR_15"        = ECR_15,               # Helps to turn to other people in times of need
  "ECR_16"        = ECR_16                # Resent when people I am close to spend time away from me
)

vars_con <- bind_cols(
  "S2" = S2 # Year of birth
)

vars_dealwithlater <- bind_cols(
  "Provinces_Canada" = Provinces_Canada  # cat; Province (for Canadian responses, incl. NA for US)
)

vars_excl <- bind_cols(
  "Primary_Case" = Primary_Case,      # Unknown (incl. NA)
  "C1AA"         = C1AA              # ord; When tested for COVID-19 (incl. 99 for missing; too many NA)
)

# These variables need NAs converted to 0 then combined with vars_cat
vars_catnom_convertto0 <- bind_cols(
  "S6E"            = S6E,               # Hospital or long-term care worker (y/n/NA)
  "S16B"           = S16B,              # Type of cannabis product (incl. NA)
  "noanswerS16C_r1" = noanswerS16C_r1,  # cat; Number of cannabis products (less than 1 per day; incl. NA)
  "noanswerS16C_r2" = noanswerS16C_r2,  # cat; Number of cannabis products (it depends; incl. NA)
  "C1B"            = C1B,               # Type of testing (incl. NA)
  "C1C"            = C1C                # COVID-19 Detected (incl. NA)
)

S6E <- S6E %>% labelled(
  ., 
  labels = c(attr(., "labels", exact = TRUE), "Not a health care worker" = 2),
  label = attr(., "label", exact = TRUE))
S6E[is.na(S6E)] <- 2
S16B <- S16B %>% labelled(
  ., 
  labels = c(attr(., "labels", exact = TRUE), "Not a cannabis/marijuana user" = 0),
  label = attr(., "label", exact = TRUE))
S16B[is.na(S16B)] <- 0
noanswerS16C_r1 <- noanswerS16C_r1 %>% labelled(
  ., 
  labels = c(attr(., "labels", exact = TRUE), "Not a cannabis/marijuana user" = 2),
  label = attr(., "label", exact = TRUE))
noanswerS16C_r1[is.na(noanswerS16C_r1)] <- 2
noanswerS16C_r2 <- noanswerS16C_r2 %>% labelled(
  ., 
  labels = c(attr(., "labels", exact = TRUE), "Not a cannabis/marijuana user" = 2),
  label = attr(., "label", exact = TRUE))
noanswerS16C_r2[is.na(noanswerS16C_r2)] <- 2
C1B <- C1B %>% labelled(
  ., 
  labels = c(attr(., "labels", exact = TRUE), "Not tested" = 0),
  label = attr(., "label", exact = TRUE))
C1B[is.na(C1B)] <- 0
C1C <- C1C %>% labelled(
  ., 
  labels = c(attr(., "labels", exact = TRUE), "Not tested" = 0),
  label = attr(., "label", exact = TRUE))
C1C[is.na(C1C)] <- 0
vars_cat <- bind_cols(
  vars_cat, 
  "S6E" = S6E,
  "S16B" = S16B,
  "noanswerS16C_r1" = noanswerS16C_r1,
  "noanswerS16C_r2" = noanswerS16C_r2,
  "C1B" = C1B,
  "C1C" = C1C)

# These variables need to be passed through mice to impute NA values
vars_catbin_NA <- bind_cols(
  # Categorical variables; impute with logreg for binary 
  "SDSAQr1"        = SDSAQr1,           # Willing to marry East Asian descent (y/n/NA)
  "SDSAQr2"        = SDSAQr2,           # Willing to accept East Asian as a close personal friend (y/n/NA)
  "SDSAQr3"        = SDSAQr3,           # Willing to have East Asian as neighbor (y/n/NA)
  "SDSAQr4"        = SDSAQr4,           # Willing to have East Asian as coworker (y/n/NA)
  "SDSAQr5"        = SDSAQr5,           # Willing to have East Asian as citizen (y/n/NA)
  "SDSAQr6"        = SDSAQr6,           # Willing to have East Asian as non-citizen visitor (y/n/NA)
  "SDSAQr7"        = SDSAQr7,           # Willing to exclude East Asian from visiting country (y/n/NA)
  "SDSEQr1"        = SDSEQr1,           # Willing to marry Northern European descent (y/n/NA)
  "SDSEQr2"        = SDSEQr2,           # Willing to accept Northern European as a close personal friend (y/n/NA)
  "SDSEQr3"        = SDSEQr3,           # Willing to have cartoon as neighbor (y/n/NA)
  "SDSEQr4"        = SDSEQr4,           # Willing to have Northern European as coworker (y/n/NA)
  "SDSEQr5"        = SDSEQr5,           # Willing to have Northern European as citizen (y/n/NA)
  "SDSEQr6"        = SDSEQr6,           # Willing to have Northern European as non-citizen visitor (y/n/NA)
  "SDSEQr7"        = SDSEQr7            # Willing to exclude Northern European from visiting country (y/n/NA)
)

# These variables need to be passed through mice to impute NA values
# There are no categorical unordered variables with NA values that do not need conversion...
# vars_catnom_NA <- bind_cols(
#   
# )

# These variables need to be passed through mice to impute NA values
vars_ord_NA <- bind_cols(
  # Ordinal variables; impute with polr for ordered
  "SPQ11B"         = SPQ11B,            # Leave home to exercise (incl. NA)
  "SPQ11C"         = SPQ11C,            # Stay close to home (incl. NA)
  "CVSQ2b"         = CVSQ2b,            # Government messaging suggest end to social distancing (incl. NA)
  "PVDr1"          = PVDr1,             # Bothered by people sneezing without covering mouths (incl. NA)
  "PVDr2"          = PVDr2,             # If illness going around, I will get it (incl. NA)
  "PVDr3"          = PVDr3,             # Comfortable sharing water bottle with friend (incl. NA)
  "PVDr4"          = PVDr4,             # Don't like chewed on pencils (incl. NA)
  "PVDr5"          = PVDr5,             # Past experience make me believe unlikely to get sick even when friends are (incl. NA)
  "PVDr6"          = PVDr6,             # Susceptible to infectious diseases (incl. NA)
  "PVDr7"          = PVDr7,             # Wash hands soon after shaking hands (incl. NA)
  "PVDr8"          = PVDr8,             # Susceptible to colds, flus, and other infectious diseases (incl. NA)
  "PVDr9"          = PVDr9,             # Dislike used clothing because you don't know how past person was like (incl. NA)
  "PVDr10"         = PVDr10,            # More likely to catch infectious diseases than others (incl. NA)
  "PVDr11"         = PVDr11,            # Hands do not feel dirty after touching money (incl. NA)
  "PVDr12"         = PVDr12,            # Unlikely to catch cold, flus, other illness even if going around (incl. NA)
  "PVDr13"         = PVDr13,            # Does not make me anxious to be around sick people (incl. NA)
  "PVDr14"         = PVDr14,            # Immune system protects me from most illnesses that others get (incl. NA)
  "PVDr15"         = PVDr15,            # Avoid public telephones because of risk of catching something from previous user (incl. NA)
  "SSQr1"          = SSQr1,             # Satisfaction with personal relationships (incl. NA)
  "SSQr2"          = SSQr2,             # Satisfaction with support from friends (incl. NA)
  "CVSQ8_Q9CVSQ9a" = CVSQ8_Q9CVSQ9a,    # COVID-19 affected mental health (incl. NA)
  "CVSQ8_Q9CVSQ9b" = CVSQ8_Q9CVSQ9b     # social distancing affected mental health (incl. NA)
)

# These variables need to be passed through mice to impute NA values
vars_con_NA <- bind_cols(
  # Continuous variables; impute with pmm for numeric
  "S6Fr1"          = S6Fr1,             # Number living in household (incl. NA)
  "S6Gr1"          = S6Gr1,             # Number of children under 6 (incl. NA)
  "S6Gr2"          = S6Gr2,             # Number of children 6 to 12 (incl. NA)
  "S6Gr3"          = S6Gr3              # Number of children 13 to 17 (incl. NA)
)

# These variables need SOME NAs converted to 0 then combined with vars_con_NA
vars_catnom_convertto0andNA <- bind_cols(
  # Convert NA to 0 then convert 99 to NA
  "S7C"            = S7C,               # Temporarily or permanently laid off (y/n/NA)
)

S7C[is.na(S7C)] <- 0
S7C[which(S7C == 99)] <- NA
vars_catnom_NA <- bind_cols(
  "S7C" = S7C
)

# These variables need SOME NAs converted to 0 then combined with vars_con_NA
vars_con_convertto0andNA <- bind_cols(
  # Combine noanswerS15_r0 and noanswerS15_r1 with S15r99; impute noanswerS15_r2 with pmm for numeric
  "S15r99" = S15r99,   # con; Number of drinks in last 7 days (incl. NA; convert NA from r0 and r1 to 0)
  
  # Combine noanswerS16C_r1 with S16Cr99; impute noanswerS16C_r2 with pmm for numeric
  "S16Cr99" = S16Cr99, # con; Number of cannabis products in last 7 days (incl. NA; convert NA from r0 and r1 to 0)
  
  # Combine noanswerS17A_r0 and noanswerS17A_r1 with S17Ar99; impute noanswerS17A_r2 with pmm for numeric
  "S17Ar99" = S17Ar99, # con; Number of cigarettes in last 7 days (incl. NA; convert NA from r0 and r1 to 0)
  
  # Combine noanswerS17B_r0 and noanswerS17B_r1 with S17Ar99; impute noanswerS17B_r2 with pmm for numeric
  "S17Br99" = S17Br99  # con; Number of times e-cigarettes used in last 7 days (incl. NA; convert NA from r0 and r1 to 0)
)


S15r99[which(noanswerS15_r0 == 1)] <- 0
S15r99[which(noanswerS15_r1 == 1)] <- 0
S16Cr99[which(S16A == 0)] <- 0
S16Cr99[which(noanswerS16C_r1 == 1)] <- 0
S17Ar99[which(noanswerS17A_r0 == 1)] <- 0
S17Ar99[which(noanswerS17A_r1 == 1)] <- 0
S17Br99[which(noanswerS17B_r0 == 1)] <- 0
S17Br99[which(noanswerS17B_r1 == 1)] <- 0
vars_con_NA <- bind_cols(
  vars_con_NA,
  "S15r99" = S15r99,
  "S16Cr99" = S16Cr99,
  "S17Ar99" = S17Ar99,
  "S17Br99" = S17Br99
)


# Variables below contain non-valid answers that need to be converted for imputation

# These variables need non-valid answers converted to NA then combined with vars_cat_NA
vars_catbin_converttoNA <- bind_cols(
  # Categorical variables; binary
  "S8D"         = S8D,              # Received government stimulus (y/n/"rather not say")
  "S8F"         = S8F,              # Received employment insurance (y/n/"rather not say")
  "C2"          = C2,               # Close contact of COVID-19 (y/n/"unsure")
  "S7B"         = S7B               # Laid off because of COVID-19 (y/n/"rather not say")
)

S8D[which(S8D == 99)] <- NA
S8F[which(S8F == 99)] <- NA
C2[which(C2 == 3)] <- NA
S7B[which(S7B == 99)] <- NA
vars_catbin_NA <- bind_cols(
  vars_catbin_NA,
  "S8D"         = S8D,
  "S8F"         = S8F,
  "C2"          = C2, 
  "S7B"         = S7B 
)

# These variables need non-valid answers converted to NA then combined with vars_catnom_NA
vars_catnom_converttoNA <- bind_cols(
  # Categorical variables; unordered
  "S3"          = S3,               # Gender (incl. other/prefer not to answer/rather not say; NB non-Male and non-Female answers have very low samples so combine with others)
  "S4"          = S4,               # Racial or ethnic group (incl. "rather not say")
  "S5"          = S5,               # Religion (incl. "rather not say")
  "S6B"         = S6B,              # Type of dwelling (incl. "rather not say")
  "S7A"         = S7A,              # Employment status (incl. "rather not say")
  "Q8AA"        = Q8AA              # Ever had depression, anxiety, or mental health issues (incl. NA with unknown values; convert 4 to NA)
)

S3[-which(S3 %in% c(1,2))] <- NA
S4[which(S4 == 99)] <- NA
S5[which(S5 == 99)] <- NA
S6B[which(S6B == 99)] <- NA
S7A[which(S7A == 99)] <- NA
Q8AA[which(Q8AA == 4)] <- NA
vars_catnom_NA <- bind_cols(
  vars_catnom_NA,
  "S3"          = S3, 
  "S4"          = S4, 
  "S5"          = S5, 
  "S6B"         = S6B,
  "S7A"         = S7A,
  "Q8AA"        = Q8AA
)

# These variables need non-valid answers converted to NA then combined with vars_ord_NA
vars_ord_converttoNA <- bind_cols(
  # Ordinal variables; ordered
  "S11A"        = S11A,             # Father's level of education (incl. "don't know")
  "S11C"        = S11C,             # Mother's level of education (incl. "don't know")
  "S1B"         = S1B,              # Population of area (incl. NA and "don't know")
  "S6H"         = S6H               # Household income (incl. no-responses)
)

S11A[which(S11A == 99)] <- NA
S11C[which(S11C == 99)] <- NA
S1B[which(S1B == 5)] <- NA
S6H[which(S6H == 99)] <- NA
vars_ord_NA <- bind_cols(
  vars_ord_NA,
  "S11A"        = S11A,
  "S11C"        = S11C,
  "S1B"         = S1B, 
  "S6H"         = S6H  
)

# For each of the vars_X_NA, we need to impute the NA values. We can use the
# mice package to do this!

haven_binarize_yesno <- function(x) {
  a <- as_factor(x) %>% as.character()
  a[which(a == "No")] <- 0
  a[which(a == "Yes")] <- 1
  a <- as.numeric(a)
  return(a)
}

haven_ordered <- function(x) {
  a <- as_factor(x, ordered = TRUE, levels = "values")
  return(a)
}


# Prep datasets for each imputation

data_preimpute <- bind_cols(
  vars_cat %>% mutate(across(everything(), as_factor)),
  vars_ord %>% mutate(across(everything(), haven_ordered)),
  vars_con
)

data_preimpute_catbin_NA <- bind_cols(
  data_preimpute,
  vars_catbin_NA %>% mutate(across(everything(), as_factor))
) %>% zap_labels()

data_preimpute_catnom_NA <- bind_cols(
  data_preimpute,
  vars_catnom_NA %>% mutate(across(everything(), as_factor))
) %>% zap_labels()

data_preimpute_ord_NA <- bind_cols(
  data_preimpute,
  vars_ord_NA %>% mutate(across(everything(), haven_ordered))
  # vars_con_NA
) %>% zap_labels()

data_preimpute_con_NA <- bind_cols(
  data_preimpute,
  vars_con_NA
) %>% zap_labels()


# Check if data set has already been imputed and is unchanged; otherwise, redo

if (file.exists(paste0(getwd(), "/output/data_impute_catbin_NA.RDS"))) {
  data_check <- readRDS(file = paste0(getwd(), "/output/data_impute_catbin_NA.RDS"))
} else {
  data_check <- NULL
}

if (setequal(data_check$data, data_preimpute_catbin_NA)) {
  data_impute_catbin_NA <- data_check
  message("data_impute_catbin_NA read in from ./output/data_impute_catbin_NA.RDS")
} else {
  data_impute_catbin_NA <- mice(
    data = data_preimpute_catbin_NA,
    nnet.MaxNWts = 50000,
    seed = 1
  )
  saveRDS(object = data_impute_catbin_NA, 
          file = paste0(getwd(), "/output/data_impute_catbin_NA.RDS"))
  message("data_impute_catbin_NA saved to ./output/data_impute_catbin_NA.RDS")
}

if (file.exists(paste0(getwd(), "/output/data_impute_catnom_NA.RDS"))) {
  data_check <- readRDS(file = paste0(getwd(), "/output/data_impute_catnom_NA.RDS"))
} else {
  data_check <- NULL
}

if (setequal(data_check$data, data_preimpute_catnom_NA)) {
  data_impute_catnom_NA <- data_check
  message("data_impute_catnom_NA read in from ./output/data_impute_catnom_NA.RDS")
} else {
  data_impute_catnom_NA <- mice(
    data = data_preimpute_catnom_NA,
    nnet.MaxNWts = 50000,
    seed = 1
  )
  
  saveRDS(object = data_impute_catnom_NA, 
          file = paste0(getwd(), "/output/data_impute_catnom_NA.RDS"))
  message("data_impute_catnom_NA saved to ./output/data_impute_catnom_NA.RDS")
}

if (file.exists(paste0(getwd(), "/output/data_impute_ord_NA.RDS"))) {
  data_check <- readRDS(file = paste0(getwd(), "/output/data_impute_ord_NA.RDS"))
} else {
  data_check <- NULL
}

if (setequal(data_check$data, data_preimpute_ord_NA)) {
  data_impute_ord_NA <- data_check
  message("data_impute_ord_NA read in from ./output/data_impute_ord_NA.RDS")
} else {
  data_impute_ord_NA <- mice(
    data = data_preimpute_ord_NA,
    nnet.MaxNWts = 50000,
    seed = 1
  )
  
  saveRDS(object = data_impute_ord_NA, 
          file = paste0(getwd(), "/output/data_impute_ord_NA.RDS"))
  message("data_impute_ord_NA saved to ./output/data_impute_ord_NA.RDS")
}

if (file.exists(paste0(getwd(), "/output/data_impute_con_NA.RDS"))) {
  data_check <- readRDS(file = paste0(getwd(), "/output/data_impute_con_NA.RDS"))
} else {
  data_check <- NULL
}

if (setequal(data_check$data, data_preimpute_con_NA)) {
  data_impute_con_NA <- data_check
  message("data_impute_con_NA read in from ./output/data_impute_con_NA.RDS")
} else {
  data_impute_con_NA <- mice(
    data = data_preimpute_con_NA,
    nnet.MaxNWts = 50000,
    seed = 1
  )
  
  saveRDS(object = data_impute_con_NA, 
          file = paste0(getwd(), "/output/data_impute_con_NA.RDS"))
  message("data_impute_con_NA saved to ./output/data_impute_con_NA.RDS")
}

data_complete <- left_join(
  x = complete(data_impute_catbin_NA),
  y = complete(data_impute_catnom_NA),
  by = names(data_preimpute)
)

data_complete <- left_join(
  x = data_complete,
  y = complete(data_impute_ord_NA),
  by = names(data_preimpute)
)

data_complete <- left_join(
  x = data_complete,
  y = complete(data_impute_con_NA),
  by = names(data_preimpute)
)

detach(coviddata_exclNA)

# Save out objects required for analysis, then clean up the environment

coviddata_imp <- data_complete
datadict_imp <- datadict[names(data_complete)]

for (i in 1:length(datadict_imp)) {
  varname <- names(datadict_imp[i])
  datadict_imp[[i]]$Table <- 
    table(coviddata_imp[, names(datadict_imp[i])], useNA = "ifany")
}

vars_imp <- list(
  cat = c(names(vars_cat),
          names(vars_catbin_NA),
          names(vars_catnom_NA)),
  con = c(names(vars_con),
          names(vars_con_NA)),
  ord = c(names(vars_ord),
          names(vars_ord_NA))
)

write_csv(data_complete, file = paste0(getwd(), "/output/data_imputed.csv"))
message("data_complete saved to ./output/data_imputed.csv")

rm(
  C1B,
  C1C,
  C2,
  # coviddata,
  coviddata_exclNA,
  # coviddata_imp,
  coviddata_num,
  data_check,
  data_complete,
  data_impute_catbin_NA,
  data_impute_catnom_NA,
  data_impute_con_NA,
  data_impute_ord_NA,
  data_preimpute,
  data_preimpute_catbin_NA,
  data_preimpute_catnom_NA,
  data_preimpute_con_NA,
  data_preimpute_ord_NA,
  # datadict,
  datadict_exclNA,
  # datadict_imp,
  haven_binarize_yesno,
  haven_ordered,
  noanswerS16C_r1,
  noanswerS16C_r2,
  Q8AA,
  # required_packages,
  S11A,
  S11C,
  S15r99,
  S16B,
  S16Cr99,
  S17Ar99,
  S17Br99,
  S1B,
  S3,
  S4,
  S5,
  S6B,
  S6E,
  S6H,
  S7A,
  S7B,
  S7C,
  S8D,
  S8F,
  # using,
  vars_cat,
  vars_catbin_converttoNA,
  vars_catbin_NA,
  vars_catnom_convertto0,
  vars_catnom_convertto0andNA,
  vars_catnom_converttoNA,
  vars_catnom_NA,
  vars_con,
  vars_con_convertto0andNA,
  vars_con_NA,
  vars_dealwithlater,
  vars_excl,
  # vars_imp,
  vars_na1,
  vars_na2,
  vars_na3,
  vars_naall,
  vars_ord,
  vars_ord_converttoNA,
  vars_ord_NA
)

message("./scripts/02_cleandata.R was executed")

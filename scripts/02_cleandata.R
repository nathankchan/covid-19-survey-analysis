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
datadict <- sapply(
  coviddata_num,
  function(x) {
    a <- attr(x, "label", exact = TRUE)
    b <- attr(x, "labels", exact = TRUE)
    return(list("Question" = a, "Values" = b))
  }, simplify = F
)

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
navars1 <-
  coviddata_num[which(Timepoint_1 == 1),] %>%
  .[, which(sapply(., function(x) all(is.na(x))))] %>%
  names()
navars2 <-
  coviddata_num[which(Timepoint_2 == 1),] %>%
  .[, which(sapply(., function(x) all(is.na(x))))] %>%
  names()
navars3 <-
  coviddata_num[which(Timepoint_3 == 1),] %>%
  .[, which(sapply(., function(x) all(is.na(x))))] %>%
  names()

# We can deal with the variables containing only NA variables for some Timepoint
# later. For now, let's combine them then exclude them from the sifting step
# in the next code block.
navars_all <- unique(c(navars1, navars2, navars3))
coviddata_exclna <- coviddata_num[, -which(names(coviddata_num) %in% navars_all)]
detach(coviddata_num)
attach(coviddata_exclna)

# Sift through datadict_exclna and categorize each variable by type (i.e., 
# categorical, ordinal, continuous, or has NA or missing values)
datadict_exclna <- sapply(
  coviddata_exclna,
  function(x) {
    a <- attr(x, "label", exact = TRUE)
    b <- attr(x, "labels", exact = TRUE)
    d <- table(x, useNA = "ifany")
    return(list("Question" = a, "Values" = b, "Table" = d))
  }, simplify = F
)

catvars <- cbind.data.frame(
  Timepoint_1, # Identifier for time when survey collected
  Timepoint_2, # Identifier for time when survey collected
  Timepoint_3, # Identifier for time when survey collected
  S3, # Gender (incl. other/prefer not to answer/rather not say)
  S4, # Racial or ethnic group (incl. "rather not say")
  S5, # Religion (incl. "rather not say")
  S6A, # Relationship status
  S6B, # Type of dwelling (incl. "rather not say")
  S7A, # Employment status (incl. "rather not say")
  S8D, # Received government stimulus (y/n/"rather not say")
  S8F, # Received employment insurance (y/n/"rather not say")
  CVSQ1, # Source of health information (incl. other)
  C1, # Ever tested for COVID-19 (y/n)
  C2, # Close contact of COVID-19 (y/n/unsure)
  RF1r1, # Have heart disease (y/n)
  RF1r2, # Have hypertension (y/n)
  RF1r3, # Have lung disease (y/n)
  RF1r4, # Have diabetes (y/n)
  RF1r5, # Have cancer (y/n)
  RF1r6, # Have chronic kidney disease (y/n)
  RF1r7, # Have obesity (y/n)
  RF2, # Have weakened immune system (y/n)
  ASr1, # Have fever/chills/shakes (y/n)
  ASr2, # Have cough (y/n)
  ASr3, # Have shortness of breath (y/n)
  ASr4, # Have tired or fatigued(y/n)
  ASr5, # Have lost appetite (y/n)
  ASr6, # Have muscle aches and pains (y/n)
  ASr7, # Have nasal congestion (y/n)
  ASr8, # Have sore throat (y/n)
  ASr9, # Have excessive sputum (y/n)
  ASr10, # Have lost smell (y/n)
  ASr11, # Have lost taste (y/n)
  ASr12, # Have diarrhea (y/n)
  ASr13, # Have other symptoms (y/n)
  CVSQ2, # Belief of origin of COVID-19 (incl. other)
  CVSQ3, # Had COVID/what was outcome
  CVSQCVSQ4, # Close family member or friend that is health care worker (y/n)
  CVSQCVSQ5, # Close family member or friend that is high risk (y/n)
  CVSQCVSQ6, # Close family member or friend in senior's residence (y/n)
  CVSQCVSQ7 # Close family member or friend in long-term care (y/n)
)

ordvars <- cbind.data.frame(
  S10A, # Highest level of education
  S8x1S8A, # Income reduced by COVID-19
  S8x1S8B, # In financial distress
  S8x1S8C, # Trouble making ends meet
  SPFQr1, # Comply with social distancing (avoid groups)
  SPFQr2, # Comply with social distancing (maintain 6ft distance in community)
  SPFQr3, # Comply with social distancing (keep distance from people at risk)
  SPQr1, # Stay at home unless for work
  SPQr2, # Avoid non-essential trips
  SPQr3, # Keep distance if leaving home
  SPQr4, # Limit close contacts with people at high risk
  SPQr5, # Avoid crowded places
  SPQr6, # Avoid common greetings (hugging and kissing)
  SPQr7, # Disinfect hands often
  SPQr8, # Wash or disinfect hands for at least 20 seconds
  SPQr9, # Cough or sneeze into bend of arm
  SPQr10, # Avoid touching eyes, nose, mouth
  SSSQr1, # Easily influenced by people's opinions
  SSSQr2, # Easily influenced by commercials
  SSSQr3, # Likely to cough or sneeze when seeing others do so
  SSSQr4, # Imagining drink can make me thirsty
  SSSQr5, # Salesperson can make me want their product
  SSSQr6, # Practical advice from magazines or TV
  SSSQr7, # Nicely displayed product makes me want to buy
  SSSQr8, # Seeing shiver makes me feel a chill
  SSSQr9, # Get style from celebrities
  SSSQr10, # Feel same feelings when others share feelings
  SSSQr11, # Follow other people's advice when making decisions
  SSSQr12, # Reading tasty descriptions of dishes makes mouth water
  SSSQr13, # Get good ideas from others
  SSSQr14, # Frequently change opinions after talking with others
  SSSQr15, # Lotion commercials make skin feel dry
  SSSQr16, # Discover favorite things from friends
  SSSQr17, # Follow current fashion trends
  SSSQr18, # Thinking about scary makes heart pound
  SSSQr19, # Pick up habits from friends
  SSSQr20, # Feel ill if told don't look well
  SSSQr21, # Important to fit in
  RQr1, # Saftey first
  RQr2, # Do not take risks with my health
  RQr3, # Avoid risks
  RQr4, # Regularly take risks
  RQr5, # Dislike not knowing what is going to happen
  RQr6, # View risks as a challenge
  RQ7, # Risk avoider vs risk seeker
  DRIQInfor1, # Experience presence of divine
  DRIQInfor2, # Religious/spiritual beliefs lie behind whole approach to life
  DRIQInfor3, # Try hard to carry religious/spiritual beliefs into all dealings in life
  TIPIQr1, # Extroverted or enthusiastic
  TIPIQr2, # Critical or quarrelsome
  TIPIQr3, # Dependable or self-disciplined
  TIPIQr4, # Anxious or easily upset
  TIPIQr5, # Open to new experiences or complex
  TIPIQr6, # Reserved or quiet
  TIPIQr7, # Sympathetic or warm
  TIPIQr8, # Disorganized or careless
  TIPIQr9, # Calm or emotionally stable
  TIPIQr10, # Conventional or un-creative
  CTQr1, # All levels of government are capable concerning COVID-19
  CTQr2, # All levels of government are experts concerning COVID-19
  CTQr3, # All levels of government carry duties out well concerning COVID-19
  CTQr4, # All levels of government will do their best to help citizens concerning COVID-19
  CTQr5, # All levels of government act in the interest of citizens concerning COVID-19
  CTQr6, # All levels of government are interested in the well-being of citizens concerning COVID-19
  CTQr7, # All levels of government approach citizens in sincere ways concerning COVID-19
  CTQr8, # All levels of government are honest concerning COVID-19
  HCQr1, # Positive thinking can help fight off minor illness
  HCQr2, # Alternative/holistic medicine should be subject to more scientific testing
  HCQr3, # Important to be careful about lifestyle when stressed
  HCQr4, # Alternative/holistic medicine can be dangerous by preventing people getting proper treatment
  HCQr5, # Symptoms of illness made worse by depression
  HCQr6, # Alternative/holistic medicine used only as last resort
  HCQr7, # Stressful life events increase likelihood of illness
  HCQr8, # Worthwhile to try complementary medicine before seeing doctor
  HCQr9, # Conflict with others has no effect on health
  HCQr10, # Alternative/holistic medicine only used for minor ailments and not serious illness
  HCQr11, # Important to find balance between work and relaxation to stay healthy
  HCQr12, # Alternative/holistic medicine builds up body's defenses leading to permanent cure
  LCQr1, # Life is determined by own actions
  LCQr2, # Able to protect personal interests
  LCQr3, # Can determine what will happen in my life
  LCQr4, # Life controlled by accidental findings
  LCQr5, # No chance of protecting personal interest from bad luck
  LCQr6, # I get what I want because I'm lucky
  LCQr7, # Little chance of protecting personal interests when conflict with strong pressure groups
  LCQr8, # Life chiefly controlled by powerful others
  LCQr9, # Feel like what happens in life determined by powerful people
  VAXQr1, # Feel safe after vaccination
  VAXQr2, # Rely on vaccines to stop serious infectious diseases
  VAXQr3, # Feel protected after getting vaccinated
  VAXQr4, # Vaccines appear safe but have problems not yet discovered
  VAXQr5, # Vaccines can cause unforeseen problems in children
  VAXQr6, # Worry about unknown effects of vaccines in future
  VAXQr7, # Vaccines make a lot of money for pharmaceutical companies
  VAXQr8, # Authorities promote vaccination for financial gain not health
  VAXQr9, # Vaccination programs are a big con
  VAXQr10, # Natural immunity lasts longer than vaccination
  VAXQr11, # Natural exposure to viruses and germs gives safest protection
  VAXQr12, # Exposed to diseases naturally safer for immune system vs vaccination
  GTSQr1, # Most people are basically honest
  GTSQr2, # Most people are trustworthy
  GTSQr3, # Most people are basically good and kind
  GTSQr4, # Most people are trustful of others
  GTSQr5, # I am trustful
  GTSQr6, # Most people respond in kind when they are trusted by others
  PAQ1, # Political spectrum (communism to fascism)
  ABQr1, # Listen to older people about how to behave
  ABQr2, # Question judgment of umpires or referees
  ABQr3, # Do what authoritative figures tell you to do
  ABQr4, # Criticize people who are rude to superiors
  ABQr5, # Encourage young people to do what they want even if against parents wishes
  ABQr6, # Dress to be acceptable to people that run the place
  ABQr7, # Treat experts with respect even when you don't think much of them personally
  ABQr8, # Support left-wing radical policies
  ABQr9, # Take part in demonstrations to show opposition to policies you do not like
  ABQr10, # Express approval for work of school teachers
  ABQr11, # Go to church
  ABQr12, # Make fun of police
  ABQr13, # Look for guidance from someone wiser
  ABQr14, # Sympathize with rebels
  ABQr15, # Break speed limit or encourage driver to do so if safe when in hurry
  ABQr16, # Follow doctor's orders
  ABQr17, # Question what you hear on news
  ABQr18, # Cross road against pedestrian traffic lights
  ABQr19, # Ask for a second opinion when uncertain about doctor's advice
  ABQr20, # Stand when playing national anthem in public
  ABQr21, # Express contempt for politicians
  ABQr22, # Annoyed when people sneer at those in authority
  ABQr23, # Show special respect for people in high positions
  ABQr24, # Speak up against boss when acting unfairly
  CVSQ8_Q9CVSQ8, # Willing to get vaccinated against COVID-19
  PANAS_1, # Interested
  PANAS_2, # Distressed
  PANAS_3, # Excited
  PANAS_4, # Upset
  PANAS_5, # Strong
  PANAS_6, # Guilty
  PANAS_7, # Scared
  PANAS_8, # Hostile
  PANAS_9, # Enthusiastic
  PANAS_10, # Proud
  PANAS_11, # Irritable
  PANAS_12, # Alert
  PANAS_13, # Ashamed
  PANAS_14, # Inspired
  PANAS_15, # Nervous
  PANAS_16, # Determined
  PANAS_17, # Attentive
  PANAS_18, # Jittery
  PANAS_19, # Active
  PANAS_20, # Afraid
  ECR_1, # Uncomfortable when other people want to be close
  ECR_2, # Worry about feeling abandoned
  ECR_3, # Tell people whom I feel close just about everything
  ECR_4, # Need reassurance that I am loved
  ECR_5, # Don't feel comfortable opening up to other people
  ECR_6, # Worry a lot about relationships
  ECR_7, # Discuss problems and concerns with people close to me
  ECR_8, # Find that other people don't want to get as close as I would like
  ECR_9, # Avoid getting too close to other people
  ECR_10, # Worry that other people won't care about me as much as I care about them
  ECR_11, # Don't mind asking other people about comfort, advice, or help
  ECR_12, # Get frustrated when other people are not around as much as I would like
  ECR_13, # Prefer not to be too close to other people
  ECR_14, # Worry a fair about about losing people I feel close to
  ECR_15, # Helps to turn to other people in times of need
  ECR_16 # Resent when people I am close to spend time away from me
)

convars <- cbind.data.frame(
  S2 # Year of birth
)

navars <- cbind.data.frame(
  
  # Add label for S6E non health care worker
  S6D, # Health care worker (y/n)
  S6E, # cat; Hospital or long-term care worker (y/n/NA)
  
  # Add label for S7C not laid off
  S7B, # Laid off because of COVID-19 (y/n/"rather not say")
  S7C, # cat; Temporarily or permanently laid off (y/n/NA)
  
  Provinces_Canada, # cat; Province (for Canadian responses, incl. NA for US)
  S16A, # cat; Use of cannabis products (combine with below)
  S16B, # cat, Type of cannabis product (no 0; incl. NA)
  
  S11A, # ord; Father's level of education (incl. "don't know")
  S11C, # ord; Mother's level of education (incl. "don't know")
  S1B, # Population of area (incl. NA and "don't know")
  S6Fr1, # ord; Number living in household (incl. NA)
  S6Gr1, # ord; Number of children under 6 (incl. NA)
  S6Gr2, # ord; Number of children 6 to 12 (incl. NA)
  S6Gr3, # ord; Number of children 13 to 17 (incl. NA)
  S6H, # ord; Household income (incl. no-responses)
  
  S15r99, # con; Number of drinks in last 7 days (no 0; incl. NA; combine with below)
  noanswerS15_r0, # con; Number of drinks in last 7 days (non-drinkers)
  noanswerS15_r1, # con; Number of drinks in last 7 days (less than 1 per week)
  noanswerS15_r2, # con; Number of drinks in last 7 days (it depends)

  S16Cr99, # con; Number of cannabis products in last 7 days (no 0; incl. NA; combine with below)
  noanswerS16C_r1, # con; Number of cannabis products (non-users)
  noanswerS16C_r2, # con; Number of cannabis products (less than 1 per week)
  
  S17Ar99, # con; Number of cigarettes in last 7 days (no 0; incl. NA; combine with below)
  noanswerS17A_r0, # con; Number of cigarettes (non-smokers)
  noanswerS17A_r1, # con; Number of cigarettes (less than 1 per day)
  noanswerS17A_r2, # con; Number of cigarettes (it depends)
  
  S17Ar99, # con; Number of times e-cigarettes used in last 7 days (no 0; incl. NA; combine with below)
  noanswerS17B_r0, # con; Number of e-cigarettes (non-users)
  noanswerS17B_r1, # con; Number of e-cigarettes (less than 1 per day)
  noanswerS17B_r2, # con; Number of e-cigarettes (it depends)
  
  C1AA, # ord; When tested for COVID-19 (incl. NA; convert to 0)
  C1B, # cat; Type of testing (incl. NA; convert to 0)
  C1C, # cat; COVID-19 Detected (incl. NA, convert to 0)
  
  SPQ11A, # ord; Exercise frequency
  SPQ11B, # ord; Leave home to exercise (incl. NA from "never" in SPQ11A)
  SPQ11C, # ord; Stay close to home (incl. NA from "never" in SPQ11B)
  
  # CVSQ2b may have NAs from CVSQ2 (do not believe COVID exists, but NAs mismatch (102 in 2 vs 104 in 2b))
  CVSQ2b, # ord; Government messaging suggest end to social distancing (incl. NA; convert to 0)
  
  # All PVDrX are ord and have 1021 NA cases
  PVDr1, # Bothered by people sneezing without covering mouths (incl. NA)
  PVDr2, # If illness going around, I will get it (incl. NA)
  PVDr3, # Comfortable sharing water bottle with friend (incl. NA)
  PVDr4, # Don't like chewed on pencils (incl. NA)
  PVDr5, # Past experience make me believe unlikely to get sick even when friends are (incl. NA)
  PVDr6, # Susceptible to infectious diseases (incl. NA)
  PVDr7, # Wash hands soon after shaking hands (incl. NA)
  PVDr8, # Susceptible to colds, flus, and other infectious diseases (incl. NA)
  PVDr9, # Dislike used clothing because you don't know how past person was like (incl. NA)
  PVDr10, # More likely to catch infectious diseases than others (incl. NA)
  PVDr11, # Hands do not feel dirty after touching money (incl. NA)
  PVDr12, # Unlikely to catch cold, flus, other illness even if going around (incl. NA)
  PVDr13, # Does not make me anxious to be around sick people (incl. NA)
  PVDr14, # Immune system protects me from most illnesses that others get (incl. NA)
  PVDr15, # Avoid public telephones because of risk of catching something from previous user (incl. NA)
  
  # SSQrX has 104 NA cases
  SSQr1, # ord; Satisfaction with personal relationships (incl. NA; impute data)
  SSQr2, # ord; Satisfaction with support from friends (incl. NA; impute data)
  
  # SDSAQrX are cat vars
  SDSAQr1, # Willing to marry East Asian descent (incl. NA)
  SDSAQr2, # Willing to accept East Asian as a close personal friend (incl. NA)
  SDSAQr3, # Willing to have East Asian as neighbor (incl. NA)
  SDSAQr4, # Willing to have East Asian as coworker (incl. NA)
  SDSAQr5, # Willing to have East Asian as citizen (incl. NA)
  SDSAQr6, # Willing to have East Asian as non-citizen visitor (incl. NA)
  SDSAQr7, # Willing to exclude East Asian from visiting country (incl. NA)
  
  # SDSEQrX are cat vars
  SDSEQr1, # Willing to marry Northern European descent (incl. NA)
  SDSEQr2, # Willing to accept Northern European as a close personal friend (incl. NA)
  SDSEQr3, # Willing to have cartoon as neighbor (incl. NA)
  SDSEQr4, # Willing to have Northern European as coworker (incl. NA)
  SDSEQr5, # Willing to have Northern European as citizen (incl. NA)
  SDSEQr6, # Willing to have Northern European as non-citizen visitor (incl. NA)
  SDSEQr7, # Willing to exclude Northern European from visiting country (incl. NA)
  
  CVSQ8_Q9CVSQ9a, # ord; COVID-19 affected mental health (incl. NA; impute NA)
  CVSQ8_Q9CVSQ9b, # ord; social distancing affected mental health (incl. NA; impute NA)
  
  Q8AA # cat; Ever had depression, anxiety, or mental health issues (incl. NA with unknown values)
  
)

excludevars <- c(
  Primary_Case # Unknown... (incl. NA)
)

# Let's recover as many of the variables containing NAs and incorporate them
# back into sensible variables where possible

# S6D and S6E concern health care workers. Since all NAs in S6E are those than
# answered "no" in S6D, we can add a new label to S6E for those that are not
# health care workers
coviddata_exclna$S6E_NAlabelled <- S6E %>% labelled(
  ., 
  labels = c(attr(., "labels", exact = TRUE), "Not a health care worker" = 2),
  label = attr(., "label", exact = TRUE))
coviddata_exclna$S6E_NAlabelled[is.na(coviddata_exclna$S6E_NAlabelled)] <- 2 
catvars <- cbind.data.frame(catvars, S6D, S6E_NAlabelled = coviddata_exclna$S6E_NAlabelled)

# S7B and S7C concern laid off workers. Since all NAs in S7C are those that
# answered "no" or "no response" in S7B, we can add them back to their
# respective label in S7C
coviddata_exclna$S7C_NAlabelled <- S7C %>% labelled(
  ., 
  labels = attr(., "labels", exact = TRUE),
  label = attr(., "label", exact = TRUE))
coviddata_exclna[which(S7B == 0), "S7C_NAlabelled"] <- 0
coviddata_exclna[which(S7B == 99), "S7C_NAlabelled"] <- 99
catvars <- cbind.data.frame(catvars, S7B, S7C_NAlabelled = coviddata_exclna$S7C_NAlabelled)


message("./scripts/01_loaddata.R was executed.")

library(censusapi)
library(dplyr)
library(stringr)
library(janitor)
library(jsonlite)
library(RSocrata)
library(tidyr)

### REQUEST ------------
# Map of gun violence and where cure violence operates (specifically crisis management systems (CMS) but we’ll focus on cure)

## OUTLINE TEXT for Reference ##

# Health-Based Approaches to Prevent + Address Violence #

# > Professionalizing + Expanding CMS Violence Prevention 
# Council budget/policy initiative to create a working group of national violence prevention experts and local violence prevention leaders to develop a roadmap/implementation plan for professionalizing and scaling-up violence prevention as part of NYC safety infrastructure 

# > Community-Based Safety Programs 
# Council budget initiative to support community-based safety programs that work alongside CMS organizations and local precincts (i.e. We Build the Block, etc.) 

# > Increased Victim Services in Communities Experiencing Most Violence 
# Trauma Recovery Centers in communities 
# Trauma recovery services in schools within communities experiencing most violence as part of mental health services expansion in schools to address childhood exposure to violence 
# Food Access

Supply gap map

* The map reveals multiple areas with supply gaps that currently have none or few EFAP providers (e.g., most of Staten Island, the Bayside area of Queens, Middle Village, Steinway, Van Cortlandt Village).
* Even some areas with EFAP providers still have a relatively high supply gap (e.g., Elmhurst, LES, Bedford, Harlem).
* There are many EFAP providers in the Crown Heights and East Flatbush area which is reflected in a lower supply gap in many but not all the neighborhoods there.

***   


### Data Sources 
- [Active EFAP programs from NYC HRA](https://www1.nyc.gov/assets/hra/downloads/pdf/services/efap/EFAP_ACTIVE.pdf)
- [Mayor's Office of Food Policy Supply Gap](https://www1.nyc.gov/site/foodpolicy/reports-and-data/supply-gap.page)

### Code

Code is numbered in the order that they are run. 

- 01_efap_pdf.R: Pull and clean active EFAP sites from pdf.  Create data/EFAP_pdf_[DATE]. 
- 02_efap_geocode.ipynb: Geocode active EFAP sites using the program name and address provided from pdf. Create data/efap_geo.csv. 
- 03_data_assembly.R: Group EFAP locations by NTA to use with supply gap metric. 
- 04_efap_supplygap_map.R: Map supply gap metric overlayed with EFAP locations. Create visual/efap_score.png and visual/efap_score.html. 


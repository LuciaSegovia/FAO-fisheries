# An open framework and tools to create reproducible food composition data for use in nutrition


This repository contains the scripts necessary to adopt and adapt the open framework and tools to compile transparent and reproducible Food Composition Tables and Databases (FCTs) and Nutrient Conversion Tables (NCTs). 


In summary, the framework consists in six steps (Figure 1). 


![](presentation/Workflow_v6.0.0.jpg)


  1) For **identification of the FCTs**, we followed the “FAO/INFOODS Evaluation framework to assess the quality of published food composition tables and databases” [(Charrondiere et al., 2023)]( https://doi.org/10.4060/cc5371en), and the list of FCTs included is available below. 

  2) The second and third steps, **importing** and **standardising** are FCT independent are performed by independent scripts, which are all contained in an individual folder. By using the script `merging_all.R` script all the FCTs available will be standardised and merged. For the addition of new FCTs a template folder (`00_template`) and accompanying script (`template_FCT_FAO_Tags.R`) is included for reference. 
  
  3) The step four is the **harmonisation** where the construction of the NCT starts, some steps, like the "Food name/ description standardisation and food matching" (see section 3.4 of the manual) are specific to this example use. Other steps, for instance, "re-calculating variables", are more generalisables, and can be applied to any standardised dataset by using the script `variable_re-calculation.R`. And, hence functions were developed and are available in the package: [NutritionTools](https://github.com/TomCodd/NutritionTools).
  
  4) The next step is an iterative process which is applied at different points and it use different **visualisation** for quality checks. Some examples are provided in the `visualisation.R` script.
  
  5) Finally, this repository contains the script (`Summary_Table_Production.R`) to replicate the compilation of the fish and other aquatic products subset of the [Global NCT for Food and Agriculture Organization of the United Nations (FAO) Supply and Utilization Accounts (SUAs)](https://www.fao.org/faostat/en/#data/SUA) developed by FAO’s Food and Nutrition Division [(Grande et al., 2024)](https://doi.org/10.4060/cc9678en). As well, as the expansion of nine nutrients for the same subset.
  
**Data**

|FCT id.| FCT Name| Reference| 
|--|---|---|
| AU19 | Australian Food Composition Database | Food Standards Australia New Zealand (FSANZ). 2019. The Australian Food Composition Database, release 1. Canberra.|
| BA13 | Food Composition Table for Bangladesh |	Shaheen N., Rahim A.T.M.A., Mohiduzzaman Md., Banu C.P., Bari Md. L., Tukun A.B., Mannan M.A., Bhattacharjee L., Stadlmayr B. 2013. Food Composition Table for Bangladesh. Institute of Nutrition and Food Science, Centre for Advanced Research in Sciences, University of Dhaka|
| BR11 | Brazilian FCT (TACO) |	Nucleo de Estudos e pesquisas em Alimentacao (NEPA). Brazilian Food Composition Table (TACO), 4th ed., 2011 |
| DK19| Frida: Food Database | Frida, DTU Foods public food database, version 4, 2019, National Food Institute, Technical University of Denmark |
| IN17 | Indian Food Composition Tables | 	Longvah, T., Ananthan, R., Bhaskarachary, K. & Venkaiah, K. 2017. Indian Food Composition Tables. Hyderabad, India, National Institute of Nutrition |
| JA15| Standard Tables of Food Composition in Japan | MEXT. (2015).The Standard Tables of Food Composition in Japan 2015 (Seventh Revised Edition). Official Gazette Co-operation of Japan. Japan |
| KE18| Kenya Food Composition Tables | FAO & Government of Kenya. 2018. Kenya Food Composition Tables [online]. Nairobi |
| NO21| The Norwegian Food Composition Table | Norwegian Food Composition Database 2021. Norwegian Food Safety Authority |
| NZ18| New Zealand Food Composition Database | New Zealand Food Composition Database. 2019. New Zealand FOODfilesTM 2018 Manual. The New Zealand Institute for Plant and Food Research Limited and Ministry of Health |
| UF16 | uFiSh: FAO/INFOODS User Database for Fish and Shellfish | FAO, 2016. FAO/INFOODS Global Food Composition Database for Fish and Shellfish. Version 1.0 -- uFiSh1.0. Rome |
| US19 | USDA National Nutrient Database for Standard Reference, Legacy Release |(1)	US Department of Agriculture (USDA), Agricultural Research Service, Nutrient Data Laboratory. USDA National Nutrient Database for Standard Reference, Legacy. Version Current: April 2019. |
| WA19 | FAO/INFOODS Food Composition Table for Western Africa | Vincent, A., Grande, F., Compaoré, E., Amponsah Annor, G., Addy, P.A., Aburime, L.C., Ahmed, D., Bih Loh, A.M., Dahdouh Cabia, S., Deflache, N., Dembélé, F.M., Dieudonné, B., Edwige, O.B., Ene-Obong, H.N., Fanou Fogny, N., Ferreira, M., Omaghomi Jemide, J., Kouebou, P.C., Muller, C., Nájera Espinosa, S., Ouattara, F., Rittenschober, D., Schönfeldt, H., Stadlmayr, B., van Deventer, M., Razikou Yiagnigni, A. & Charrondière, U.R. 2020. FAO/INFOODS Food Composition Table for Western Africa (2019) User Guide & Condensed Food Composition Table / Table de composition des aliments FAO/INFOODS pour l'Afrique de l'Ouest (2019) Guide d'utilisation & table de composition des aliments condensée. Rome, FAO |


Note: Those FCTs that are publicly available and accessible will be automatically standardised and merged, for the others, the user would need to get raw data into the folder. See the table below for more information on data provenance. 


**Supporting data**

Some data sets are needed to replicate the fish and other aquatic products subset of the [Global NCT for FAO Supply and Utilization Accounts (SUAs)](https://www.fao.org/faostat/en/#data/SUA). Some of which provide formatting structure and other data provided by expert inputs and consultation.

|File name | Description | Reference |
|---|---|---|
|List_SUA_ICS_fish.xlsx| This file provides information about the SUA codes, and their corresponding ISSCAAP codes. | REF|
|FISHERIES-GlobalNCT_ForSharing_Feb2022.csv| This file provides the information for the food matching of all FCTs to their correspondict SUA with the exception of the NO21| REF|

 

This repository has a data dependencies which are not available here. 


**References**

Charrondiere, U.R., Stadlmayr, B., Grande, F., Vincent, A., Oseredczuk, M., Sivakumaran, S., Puwastien, P., Judprasong, K., Haytowitz, D., Gnagnarella, P., 2023. FAO/INFOODS Evaluation framework to assess the quality of published food composition tables and databases: User guide. FAO, Rome, Italy. https://doi.org/10.4060/cc5371en

Grande, F., Ueda, Y., Masangwi, S., Holmes, B., 2024. Global nutrient conversion table for FAO supply utilization accounts. FAO, Rome, Italy. https://doi.org/10.4060/cc9678en




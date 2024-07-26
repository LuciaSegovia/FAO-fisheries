# An open framework and tools to create reproducible food composition data for use in nutrition


This repository contains the scripts necessary to adopt and adapt the open framework and tools to compile transparent and reproducible Food Composition Tables and Databases (FCTs) and Nutrient Conversion Tables (NCTs). 


In summary, the framework consists in six steps (Figure 1). 


![](presentation/Workflow_v6.0.0.jpg)


  1) For **identification of the FCTs**, we followed the “FAO/INFOODS Evaluation framework to assess the quality of published food composition tables and databases” (Charrondiere et al., 2023), and the list of FCTs included is available below. 

  2) The second and third steps, **importing** and **standardising** are FCT independent are performed by independent scripts, which are all contained in an individual folder. By using the script `merging_all.R` script all the FCTs available will be standardised and merged. For the addition of new FCTs a template folder (`00_template`) and accompanying script (`template_FCT_FAO_Tags.R`) is included for reference. 
  
  3) The step four is the **harmonisation** where the construction of the NCT starts, some steps, like the "Food name/ description standardisation and food matching" (see section 3.4 of the manual) are specific to this example use. Other steps, for instance, "re-calculating variables", are more generalisables, and can be applied to any standardised dataset by using the script `variable_re-calculation.R`. And, hence functions were developed and are available in the package: [NutritionTools](https://github.com/TomCodd/NutritionTools).
  
  4) The next step is an iterative process which is applied at different points and it use different **visualisation** for quality checks. Some examples are provided in the `visualisation.R` script.
  
  5) Finally, this repository contains the script (`Summary_Table_Production.R`) to replicate the compilation of the fish and other aquatic products subset of the Global NCT for Food and Agriculture Organization of the United Nations (FAO) Supply and Utilization Accounts (SUAs) developed by FAO’s Food and Nutrition Division. As well, as the expansion of nine nutrients for the same subset.
  


Note: Those FCTs that are publicly available and accessible will be automatically standardised and merged, for the others, the user would need to get raw data into the folder. See the table below for more information on data provenance. 


This repository has a data dependencies which are not available here. 




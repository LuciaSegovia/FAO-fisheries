
variable_names <- c("fdc_id",
"food_desc",
"food_group",
"scientific_name",
"source_fct",
"nutrient_data_source",
"Edible_factor_in_FCT",
"Edible_desc",
"specific_gravity",
"SOPg",
"SOPg_std",
"ASHg",
"ASHg_std",
"ENERCkJ",
"ENERCkJ_standardised",
"ENERCkcal",
"ENERCkcal_standardised",
"WATERg",
"PROCNTg",
"NTg",
"XN",
"FATg",
"FAT_g",
"FATCEg",
"FATg_standardised",
"CHOAVLDFg",
"CHOAVLg",
"CHOCSMg",
"CHOAVLMg",
"CHOAVLDFg_std",
"FIBTGg",
"NSPg",
"FIBCg",
"ALCg",
"ALCg_100mL",
"SUGARg",
"FASATg",
"FAMSg",
"FAPUg",
"FATRNg",
"F22D6N3g",
"F20D5N3g",
"CHOLEmg",
"CHOL_mg",
"RETOLmcg",
"VITAmcg_std",
"VITAmcg",
"VITA_RAEmcg",
"VITA_RAEmcg_std",
"CARTBEQmcg",
"CARTBEQmcg_std",
"CARTAmcg",
"CARTBmcg",
"CRYPXBmcg",
"VITEmg",
"TOCPHAmg",
"TOCPHBmg",
"TOCPHGmg",
"TOCPHDmg",
"TOCTRAmg",
"TOCTRBmg",
"TOCTRGmg",
"THIAmg",
"THIAHCLmg",
"RIBFmg",
"VITB6Amg",
"VITB6Cmg",
"VITB6_mg",
"VITB6_mg_standardised",
"FOLDFEmcg",
"FOLmcg",
"FOLACmcg",
"FOLFDmcg",
"FOLSUMmcg",
"FOL_mcg",
"NIAEQmg",
"NIAmg",
"NIATRPmg",
"TRPmg",
"VITB12mcg",
"VITCmg",
"ASCLmg",
"VITDEQmcg",
"VITDmcg",
"CHOCALmcg",
"ERGCALmcg",
"CHOCALOHmcg",
"ERGCALOHmcg",
"CAmg",
"MGmg",
"MNmg",
"Pmg",
"FEmg",
"NAmg",
"Kmg",
"CUmg",
"ZNmg",
"SEmcg",
"IDmcg")

"Food identifier as per original source (FCT)",     
"Food name and description as per original source (FCT)"
"Food group as per original source (FCT)"           
"Scientific name of the food"                       
"Id. of the FCT (BiblioID), see Table 1"            
"References or data source for the nutrient values" 
"Edible portion reported as fraction edible, imputed from the original FCT, or calculated from refuse in the FCT",
"Information provided on the edible portion"
"Density reported in the original units as in the original FCT"
  Sum of Proximate in g per 100g EP as reported in t
  Sum of Proximate in g per 100g EP (Eq. 3.3)   |   
    Ashes in g per 100g of EP  |                      
    Ashes in g per 100g of EP (Eq. 3.4)   |           
    Energy in kJ/ 100g of EP as reported in the origin
  Energy in kJ/ 100g of EP (Eq. 3.5)   |            
    Energy in kcal/ 100g EP as reported in the origina
  Energy in kcal/ 100g EP (Eq. 3.5)   |             
    Water/ moisture content in g per 100g of EP      |
    Protein in g per 100g of EP, as reported in the or
  Nitrogen, total |                                 
    Nitrogen conversion factor       |                
    Total fat content in g per 100g of EP. Sum of trig
  fat content unknown method of calculation in g per
  Fat content measured using continuous extraction (
    Total fat content in g per 100g of EP regardless o
    Available carbohydrates calculated by difference (
      Available carbohydrates calculated by weight in g 
      Carbohydrates, total; calculated by summation. Thi
      Carbohydrates, available in monosaccharide equival
      Available carbohydrates calculated by difference (
        Total dietary fibre by AOAC Prosky method expresse
        Non-starch polysaccharide, (Englyst fibre) express
        Fibre, crude in g per 100g of EP   |              
          Alcohol in g per 100g   |                         
          Alcohol expressed in g per 100mL                  
        Total sugar (the sum of free mono- and disaccharid
                     Fatty acids, total saturated in g per 100g        
                     Fatty acids, total monounsaturated in g per 100g  
                     Fatty acids, total polyunsaturated in g per 100g o
                     Fatty acids, total trans in g per 100g EP         
                     Docosahexaenoic acid (DHA) (22:6 n-3) in g/100g of
                     Eicosapentaenoic acid (EPA) (20:5 n-3) in g/100g o
                     Cholesterol determined by enzymatic or chromatogra
                     Cholesterol by unknown method of determination in 
                     Retinol in mcg per 100g of EP                     
                     Vitamin A (Retinol Eq. (RE) in mcg per 100g of EP 
                                Vitamin A (Retinol Eq. (RE) = mcg retinol + 1/6 mc
                                           Vitamin A (Retinol Activity Eq. (RAE) = mcg retino
                                                      Vitamin A (Retinol Activity Eq. (RAE)) in mcg per 
                                                      Beta-carotene equivalents, is the sum of the beta-
                                                        Beta-carotene equivalents, expressed in mcg per 10
                                                      Alpha-carotene in mcg per 100g of EP              
                                                      Beta-carotene in mcg per 100g of EP               
                                                      Beta-cryptoxanthin in mcg per 100g of EP          
                                                      Vitamin E calculated by summation of the vitamin E
                                                      Alpha-tocopherol in mg per 100g of EP             
                                                      Beta-tocopherol in mg per 100g of EP              
                                                      Gamma-tocopherol in mg per 100g of EP             
                                                      Delta-tocopherol in mg per 100g of EP             
                                                      Alpha-tocotrienol in mg per 100g of EP            
                                                      Beta-tocotrienol in mg per 100g of EP             
                                                      Gamma-tocotrienol in mg per 100g of EP            
                                                      Thiamin, vitamin B1 analysed and expressed as thia
                                                      Thiamin hydrochloride, vitamin B1 analysed and exp
                                                      Riboflavin (Vitamin B2) in mg per 100g of EP      
                                                      Vitamin B6 determined by analysis in mg/100g of EP
                                                      Vitamin B6 determined by calculation (sum of pyrid
                                                                                            Vitamin B6 by unknown method in mg/100g of EP     
                                                                                            Vitamin B6 in mg/100g of EP regardless of the Tagn
                                                                                            Dietary folate equivalents calculated as food fola
                                                                                            Folate, total (food folate + synthetic folic) in m
                                                                                            Folic acid. Synthetic folic acid used in fortifica
                                                                                            Folate food, naturally occurring food folate (dete
                                                                                                                                          Folate, sum vitamers in mcg per 100g EP. It includ
                                                                                                                                          Folate total, unknown method                      
                                                                                                                                          Niacin equivalents, total. Preformed niacin plus n
                                                                                                                                          Niacin, prefrormed in mg per 100g EP              
                                                                                                                                          Niacin equivalents, from tryptophan. 1/60 x trypto
                                                                                                                                          Tryptophan in mg per 100g of EP (includes only L-t
                                                                                                                                                                           Vitamin B12 (cobalamin, including all the active f
                                                                                                                                                                                        Vitamin C (L-ascorbic acid plus Ldehydro-ascorbic 
                                                                                                                                                                                                   L-ascorbic acid in mg per 100g of EP. Titrimetry c
                                                                                                                                                                                                   Vitamin D calculated as the sum of Vitamin D3 + D2
                                                                                                                                                                                                   Vitamin D calculated by summation of ergocalcifero
                                                                                                                                                                                                   Cholecalciferol (D3) in mcg per 100g of EP        
                                                                                                                                                                                                   Ergocalciferol (D2) in mcg per 100g of EP         
                                                                                                                                                                                                   25-hydroxycholecalciferol in mcg per 100g of EP   
                                                                                                                                                                                                   25-hydroxyergocalciferol                          
                                                                                                                                                                                                   Calcium in mg per 100g of EP                      
                                                                                                                                                                                                   Magnesium in mg per 100g of EP                    
                                                                                                                                                                                                   Manganese in mg per 100g of EP                    
                                                                                                                                                                                                   Phosphorus in mg per 100g of EP                   
                                                                                                                                                                                                   Iron in mg per 100g of EP                         
                                                                                                                                                                                                   Sodium in mg per 100g of EP                       
                                                                                                                                                                                                   Potassium in mg per 100g of EP                    
                                                                                                                                                                                                   Copper in mg per 100g of EP                       
                                                                                                                                                                                                   Zinc in mg per 100g of EP                         
                                                                                                                                                                                                   Selenium in mcg per 100g of EP                    
                                                                                                                                                                                                   Iodine in mcg per 100g of EP                      
                                                                                                                                                                                                   
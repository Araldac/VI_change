configfile: 'config/InternshipSMKConfig.yaml'

def get_climatic_vars(wildcards):
    """Get appropriate climatic variables based on var_type and species"""
    if wildcards.var_type == "CSI":
        return config["Climatic_variables"]["CSI"]
    elif wildcards.var_type == "Raw":
  #      return config["Variables_CSI"][wildcards.sp]
        return config["Climatic_variables"]["Raw"]
    else:
        raise ValueError(f"Unknown var_type: {wildcards.var_type}")


rule add_Bioclim_selected:
    '''
   Add biolcimatic variables selected 
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_allv.csv'
        
    output:
        out= 'results/species/CSI/{sp}_allV_Raw.csv'
        
    log:
        err = "logs/CSI/{sp}_add_Bioclim_selected.err",
        out = "logs/CSI/{sp}_add_Bioclim_selected.out"

    conda:
	    '../envs/CSI.yaml'

    params:
        path_bioclim = config["Path_Bioclim"],
        v_name = ",".join(config["Climatic_variables"]["Raw"])

    shell:
        r"""
        Rscript workflow/scripts/CSI/add_Bioclim_selected.R \
        {input.occurrences} \
        "{params.path_bioclim}" \
        "{params.v_name}"\
        {output.out} \
        >{log.out} 2> {log.err}
        """


rule CSI_compute:
    '''
   Applies 1 CSI for each PA set, it takes aprox 8 hours to run

    '''
    input:
        all_v = "results/species/CSI/{sp}_allV_VBio.csv"
#        bioV = "results/species/CSI/{sp}_BioV.csv"
        
    output:
        output_s1 = 'results/species/CSI/CSI_{sp}_raster_S1.tif',
        output_s2 = 'results/species/CSI/CSI_{sp}_raster_S2.tif',
        df_eval_out = 'results/species/CSI/{sp}_eval_CSI.csv'

    log:
        err = "logs/CSI/{sp}_CSI_compute.err",
        out = "logs/CSI/{sp}_CSI_compute.out"
    
    conda:
	    '../envs/CSI.yaml'
    
    params:
        #v_name = lambda wildcards: ",".join(config["Variables_CSI"][wildcards.sp]),
        v_name = ",".join(config["Climatic_variables"]["CSI"]),
        path_bioclim = config["Path_Bioclim"]

    shell:
        r"""      
        Rscript workflow/scripts/CSI/CSI_compute2.R \
        {input.all_v} \
        "{params.path_bioclim}" \
        "{params.v_name}" \
        {output.output_s1} \
        {output.output_s2} \
        {output.df_eval_out} \
        >{log.out} 2> {log.err}
        """
               

rule add_CSI:
    '''
   Add CSI to the dataframe  
    '''
    input:
        occurrences = "results/species/CSI/{sp}_allV_VBio.csv",
        raster_s1 = 'results/species/CSI/CSI_{sp}_raster_S1.tif',
        raster_s2 = 'results/species/CSI/CSI_{sp}_raster_S2.tif'
        
    output:
        dataset_out = 'results/species/CSI/{sp}_allV_CSI.csv'
        
    log:
        err = "logs/CSI/{sp}_add_CSI.err",
        out = "logs/CSI/{sp}_add_CSI.out"

    conda:
	    '../envs/CSI.yaml'

    shell:
        r"""
        Rscript workflow/scripts/CSI/add_CSI.R \
        {input.occurrences} \
        {input.raster_s1} \
        {input.raster_s2} \
        {output.dataset_out} \
        >{log.out} 2> {log.err}
        """

rule CSI_RF_optimalParam:
    '''
   Find the optimla paramters for the BRT using the CSI with S2 data w
    '''
    input:
        occurrences = 'results/species/CSI/{sp}_allV_{var_type}.csv'

    output:
        out = 'results/species/CSI/{sp}_{var_type}_RF_optimalParam.csv'
    
    log:
        err = "logs/CSI/{sp}_{var_type}_RF_optimalParam.err",
        out = "logs/CSI/{sp}_{var_type}_RF_optimalParam.out"
    conda:
	    '../envs/CSI.yaml'

    params:
        tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp],
        variables = get_climatic_vars

    shell:
        r"""      
        Rscript workflow/scripts/CSI/CSI_RF_optimalParam.R \
        {input.occurrences} \
        "{params.tribe_list}" \
        "{params.variables}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule CSI_RF_extractParams:
    '''
   extract the best parameters from the RF
    '''
    input:
        paramOpt = 'results/species/CSI/{sp}_{var_type}_RF_optimalParam.csv'
        
    output:
        table = 'results/species/CSI/{sp}_{var_type}_RF_top5_params.csv',
        out = 'results/species/CSI/{sp}_{var_type}_RF_params.csv'

    log:
        err = "logs/CSI/{sp}_{var_type}_RF_extractParams.err",
        out = "logs/CSI/{sp}_{var_type}_RF_extractParams.out"

    conda:
	    '../envs/CSI.yaml'

    shell:
        r"""      
        Rscript workflow/scripts/RF/RF_extractParams.R \
        {input.paramOpt} \
        {output.table} \
        {output.out} \
        >{log.out} 2> {log.err}
        """


rule CSI_RF_apply:
    '''
   apply RF with the bets parameters
    '''
    input:
       occurrences = 'results/species/CSI/{sp}_allV_{var_type}.csv',
       RFparamOpt = 'results/species/CSI/{sp}_{var_type}_RF_params.csv'

    output:
        out = 'results/species/CSI/{var_type}_RF_{sp}_relimpAUC1.csv'

    log:
        err = "logs/CSI/{sp}_{var_type}_RF_apply.err",
        out = "logs/CSI/{sp}_{var_type}_RF_apply.out"

    conda:
	    '../envs/CSI.yaml'
    
    params:
        tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp],
        variables = get_climatic_vars

    shell:
        r"""      
        Rscript workflow/scripts/CSI/CSI_RF_apply.R \
        {input.occurrences} \
        "{params.tribe_list}"\
        "{params.variables}"\
        {input.RFparamOpt}\
        {output.out} \
        >{log.out} 2> {log.err}
        """





####i shouldn't need thi part from now on
# 
# 
# rule Raw_RF_optimalParam:
#   '''
#    Find the optimla paramters for the RF using the CSI with S2 data w
#     '''
# input:
#   all_v = "results/species/AssignVariables/{sp}_allv.csv",
# bioV = "results/species/CSI/{sp}_BioV.csv"
# 
# output:
#   out = 'results/species/CSI/{sp}_Raw_RF_optimalParam.csv',
# out_allV = 'results/species/CSI/{sp}_Raw_allV.csv'
# 
# log:
#   err = "logs/CSI/{sp}_Raw_RF_optimalParam.err",
# out = "logs/CSI/{sp}_Raw_RF_optimalParam.out"
# conda:
#   '../envs/CSI.yaml'
# 
# params:
#   tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp],
# v_name = lambda wildcards: ",".join(config["Variables_CSI"][wildcards.sp]),
# path_bioclim = config["Path_Bioclim"]
# 
# shell:
#   r"""      
#         Rscript workflow/scripts/CSI/Raw_RF_optimalParam.R \
#         {input.all_v} \
#         {input.bioV} \
#         "{params.path_bioclim}" \
#         "{params.v_name}" \
#         "{params.tribe_list}" \
#         {output.out} \
#         {output.out_allV} \
#         >{log.out} 2> {log.err}
#         """
# 
# rule Raw_RF_extractParams:
#   '''
#    extract the best parameters from the RF
#     '''
# input:
#   paramOpt = 'results/species/CSI/{sp}_Raw_RF_optimalParam.csv'
# 
# output:
#   table = 'results/species/CSI/{sp}_Raw_RF_top5_params.csv',
# out = 'results/species/CSI/{sp}_Raw_RF_params.csv'
# 
# log:
#   err = "logs/CSI/{sp}_Raw_RF_extractParams.err",
# out = "logs/CSI/{sp}_Raw_RF_extractParams.out"
# 
# conda:
#   '../envs/CSI.yaml'
# 
# shell:
#   r"""      
#         Rscript workflow/scripts/RF/RF_extractParams.R \
#         {input.paramOpt} \
#         {output.table} \
#         {output.out} \
#         >{log.out} 2> {log.err}
#         """
# 
# 
# rule Raw_RF_apply:
#   '''
#    apply RF with the bets parameters
#     '''
# input:
#   occurrences = 'results/species/CSI/{sp}_Raw_allV.csv',
# RawparamOpt = 'results/species/CSI/{sp}_Raw_RF_params.csv'
# 
# output:
#   out = 'results/species/CSI/Raw_RF_{sp}_relimpAUC1.csv'
# 
# log:
#   err = "logs/CSI/{sp}_Raw_RF_apply.err",
# out = "logs/CSI/{sp}_Raw_RF_apply.out"
# 
# conda:
#   '../envs/CSI.yaml'
# 
# params:
#   v_name = lambda wildcards: ",".join(config["Variables_CSI"][wildcards.sp]),
# tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp]
# 
# shell:
#   r"""      
#         Rscript workflow/scripts/CSI/Raw_RF_apply.R \
#         {input.occurrences} \
#         "{params.tribe_list}"\
#         "{params.v_name}" \
#         {input.RawparamOpt}\
#         {output.out} \
#         >{log.out} 2> {log.err}
#         """
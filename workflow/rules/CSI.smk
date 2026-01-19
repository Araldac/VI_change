configfile: 'config/InternshipSMKConfig.yaml'

rule add_Bioclim_V:
    '''
   Add biolcimatic variables selected 
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_PA.csv'
        
    output:
        out= 'results/species/CSI/{sp}_BioV.csv'
        
    log:
        err = "logs/CSI/{sp}_add_Bioclim_V.err",
        out = "logs/CSI/{sp}_add_Bioclim_V.out"

    conda:
	    '../envs/CSI.yaml'

    params:
    #    v_name = lambda wildcards: config["Variables_CSI"][wildcards.sp],
    #    
        #v_name = lambda wildcards: ",".join(config["VariablesCSI"][wildcards.sp]),  # Join list into comma-separated string
        #path_bioclim = lambda wildcards: config["Path_Bioclim"]  # Make this a lambda too for consistency
        v_name = lambda wildcards: ",".join(config["Variables_CSI"][wildcards.sp]),
        #path_bioclim = "data/raster/Worldclim/"
        path_bioclim = config["Path_Bioclim"]
    shell:
        r"""
        Rscript workflow/scripts/CSI/add_Bioclim_V.R \
        {input.occurrences} \
        "{params.path_bioclim}" \
        "{params.v_name}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """
    
rule CSI_extractParams:
    '''
   Extract the best parameters from the BRT parameter optimization
    '''
    input:
        paramOpt = "results/species/CSI/{sp}_CSI_paramS2_10PA.csv"
        
    output:
        table = 'results/species/CSI/CSI_{sp}_top5_params.csv',
        out = 'results/species/CSI/CSI_{sp}_params.csv'

    log:
        err = "logs/CSI/{sp}_CSI_extractParams.err",
        out = "logs/CSI/{sp}_CSI_extractParams.out"
    conda:
	    '../envs/CSI.yaml'

    shell:
        r"""      
        Rscript workflow/scripts/CSI/CSI_extractParams.R \
        {input.paramOpt} \
        {output.table} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule CSI_compute:
    '''
   Applies 1 CSI for each PA set, it takes aprox 8 hours to run
    '''
    input:
        all_v = "results/species/AssignVariables/{sp}_allV.csv",
        bioV = "results/species/CSI/{sp}_BioV.csv"
        
    output:
        output_s1 = 'results/species/CSI/CSI_{sp}_df_combined_S1.RData',
        output_s2 = 'results/species/CSI/CSI_{sp}_df_combined_S2.RData'

    log:
        err = "logs/CSI/{sp}_CSI_compute.err",
        out = "logs/CSI/{sp}_CSI_compute.out"
    
    conda:
	    '../envs/CSI.yaml'
    
    params:
        v_name = lambda wildcards: ",".join(config["Variables_CSI"][wildcards.sp]),
        path_bioclim = config["Path_Bioclim"]

    shell:
        r"""      
        Rscript workflow/scripts/CSI/CSI_compute.R \
        {input.all_v} \
        {input.bioV} \
        "{params.path_bioclim}" \
        "{params.v_name}" \
        {output.output_s1} \
        {output.output_s2} \
        >{log.out} 2> {log.err}
        """


rule add_CSI:
    '''
   Add CSI to the dataframe  
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_allv.csv',
        raster_s1 = 'results/species/CSI/CSI_{sp}_df_combined_S1.RData',
        raster_s2 = 'results/species/CSI/CSI_{sp}_df_combined_S2.RData'
        
    output:
        df_eval_out = 'results/species/CSI/{sp}_eval_CSI.csv',
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
        {output.df_eval_out} \
        {output.dataset_out} \
        >{log.out} 2> {log.err}
        """

rule CSI_RF_optimalParam:
    '''
   Find the optimla paramters for the BRT using the CSI with S2 data w
    '''
    input:
        occurrences = 'results/species/CSI/{sp}_allV_CSI.csv'

    output:
        out = 'results/species/CSI/{sp}_CSI_RF_optimalParam.csv'
        
    log:
        err = "logs/CSI/{sp}_CSI_RF_optimalParam.err",
        out = "logs/CSI/{sp}_CSI_RF_optimalParam.out"
    conda:
	    '../envs/CSI.yaml'

    params:
        tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp]

    shell:
        r"""      
        Rscript workflow/scripts/CSI/CSI_RF_optimalParam.R \
        {input.occurrences} \
        "{params.tribe_list}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule CSI_RF_extractParams:
    '''
   extract the best parameters from the RF
    '''
    input:
        paramOpt = 'results/species/CSI/{sp}_CSI_RF_optimalParam.csv'
        
    output:
        table = 'results/species/CSI/{sp}_CSI_RF_top5_params.csv',
        out = 'results/species/CSI/{sp}_CSI_RF_params.csv'

    log:
        err = "logs/CSI/{sp}_CSI_RF_extractParams.err",
        out = "logs/CSI/{sp}CSI_RF_extractParams.out"
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
       occurrences = 'results/species/CSI/{sp}_allV_CSI.csv',
       RFparamOpt = 'results/species/CSI/{sp}_CSI_RF_params.csv'

    output:
        out = 'results/species/CSI/CSI_RF_{sp}_relimpAUC1.csv'

    log:
        err = "logs/CSI/{sp}_CSI_RF_apply.err",
        out = "logs/CSI/{sp}_CSI_RF_apply.out"
    conda:
	    '../envs/CSI.yaml'
    
    params:
        tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp]

    shell:
        r"""      
        Rscript workflow/scripts/CSI/CSI_RF_apply.R \
        {input.occurrences} \
        "{params.tribe_list}"\
        {input.RFparamOpt}\
        {output.out} \
        >{log.out} 2> {log.err}
        """
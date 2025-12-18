configfile: 'config/InternshipSMKConfig.yaml'

rule RF_optimalParam:
    '''
   Find the optimla pàrameers for the BRT using the S2 data 
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_allv.csv'

    output:
        out='results/species/RF/{sp}_rep_optimalRF.csv'
        
    log:
        err = "logs/RF/{sp}_RF_optimalParam.err",
        out = "logs/RF/{sp}_RF_optimalParam.out"
    conda:
	    '../envs/RF.yaml'

    params:
        tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp]

    shell:
        r"""      
        Rscript workflow/scripts/RF/RF_optimalParam.R \
        {input.occurrences} \
        "{params.tribe_list}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule RF_extractParams:
    input:
        paramOpt = "logs/RF/{sp}_RF_optimalParam.out"
        
    output:
        table = 'results/species/RF/RF_{sp}_top5_params.csv',
        out = 'results/species/RF/RF_{sp}_params.csv'

    log:
        err = "logs/RF/{sp}_RF_extractParams.err",
        out = "logs/RF/{sp}_RF_extractParams.out"
    conda:
	    '../envs/RF.yaml'

    shell:
        r"""      
        Rscript workflow/scripts/RF/RF_extractParams.R \
        {input.paramOpt} \
        {output.table} \
        {output.out} \
        >{log.out} 2> {log.err}
        """


rule RF_apply:
    input:
       occurrences = 'results/species/AssignVariables/{sp}_allv.csv',
       RFparamOpt = 'results/species/RF/RF_{sp}_params.csv'

    output:
        out = 'results/species/RF/RF_{sp}_relimpAUC1.csv'

    log:
        err = "logs/RF/{sp}_RF_apply.err",
        out = "logs/RF/{sp}_RF_apply.out"
    conda:
	    '../envs/RF.yaml'
    
    params:
        tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp]

    shell:
        r"""      
        Rscript workflow/scripts/RF/RF_apply.R \
        {input.occurrences} \
        "{params.tribe_list}"\
        {input.RFparamOpt}\
        {output.out} \
        >{log.out} 2> {log.err}
        """
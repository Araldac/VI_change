configfile: 'config/InternshipSMKConfig.yaml'

rule BRT_optimalParam:
    '''
   Find the optimla pàrameers for the BRT using the S2 data 
   WARNING !!! This rule takes a lot of time instead I used the cl_param_BRT2_v2.R code and used it in the cluster 
    '''
    input:
        occurrences = rules.formatDataset.output.out 

    output:
        out='results/species/BRT/rep_optimalBRT.csv'
        
    log:
        err = "logs/BRT/BRT_optimalParam.err",
        out = "logs/BRT/BRT_optimalParam.out"
    conda:
	    '../envs/BRT.yaml'

    params:
        #n_reps = config['Repetitions'][0]["Opt_BRT"] # Access first species name from list

    shell:
        r"""      
        Rscript workflow/scripts/BRT/BRT_optimalParam.R \
        {input.occurrences} \
        "{params.n_reps}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """


rule BRT_extractParams:
    input:
        paramOpt = "results/species/BRT/output_LH_parametrization_S2_v3.csv"
        
    output:
        table = 'results/species/BRT/BRT_LH_top5_params.csv',
        out = 'results/species/BRT/BRT_LH_params.csv'

    log:
        err = "logs/BRT/BRT_extractParams.err",
        out = "logs/BRT/BRT_extractParams.out"
    conda:
	    '../envs/BRT.yaml'

    shell:
        r"""      
        Rscript workflow/scripts/BRT/BRT_extractParams.R \
        {input.paramOpt} \
        {output.table} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule BRT_apply:
    input:
       occurrences = rules.formatDataset.output.out,
       paramOpt = rules.BRT_extractParams.output.out

    output:
        out = 'results/species/BRT/BRT_LH_relimpAUC.csv'

    log:
        err = "logs/BRT/BRT_apply.err",
        out = "logs/BRT/BRT_apply.out"
    conda:
	    '../envs/BRT.yaml'
    
    params:
        tribe_list = config['Tribe_list'][0]["LH"]

    shell:
        r"""      
        Rscript workflow/scripts/BRT/BRT_apply.R \
        {input.occurrences}\
        "{params.tribe_list}"\        
        {input.paramOpt}\
        {output.out} \
        >{log.out} 2> {log.err}
        """
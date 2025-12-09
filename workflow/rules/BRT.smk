configfile: 'config/InternshipSMKConfig.yaml'

rule BRT_optimalParam:
    '''
   Find the optimla pàrameers for the BRT using the S2 data 
   WARNING !!! This rule takes a lot of time instead I used the cl_param_BRT2_v2.R code and used it in the cluster 
    '''
    input:
        occurrences = rules.formatDataset.output.out ,

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
        paramOpt = "results/BRT/..."
        
    output:
        table = 'results/species/BRT/LH_top5_params.csv',
        out = 'results/species/BRT/LH_params.csv'

    log:
        err = "logs/BRT/BRT_extractParams.err",
        out = "logs/BRT/BRT_extractParams.out"
    conda:
	    '../envs/BRT.yaml'

    shell:
        r"""      
        Rscript workflow/scripts/BRT/BRT_extractParams.R \
        {output.table} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule BRT_apply:
    input:
       # paramOpt = rule.BRT_extractParams.output.out,
        
    output:
        out = 'results/species/BRT/LH_params.csv'

    log:
        err = "logs/BRT/BRT_apply.err",
        out = "logs/BRT/BRT_apply.out"
    conda:
	    '../envs/BRT.yaml'

    shell:
        r"""      
        Rscript workflow/scripts/BRT/BRT_apply.R \
        {output.table} \
        {output.out} \
        >{log.out} 2> {log.err}
        """
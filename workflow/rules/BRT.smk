onfigfile: 'config/InternshipSMKConfig.yaml'

rule BRT_optimalParam:
    '''
   Find the optimla pàrameers for the BRT using the S2 data 
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
        n_reps = config['Repetitions'][0]["Opt_BRT"] # Access first species name from list

    shell:
        r"""      
        Rscript workflow/scripts/BRT/BRT_optimalParam.R \
        {input.occurrences} \
        "{params.n_reps}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """
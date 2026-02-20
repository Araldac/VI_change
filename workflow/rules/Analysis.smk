configfile: 'config/InternshipSMKConfig.yaml'

rule forecast:
    '''
  forecast training on S1 and testing on S2
    '''
    input:
        data = 'results/species/AssignVariables/{sp}_allv.csv',
        params = 'results/species/RF/RF_{sp}_params.csv'

    output:
        out='results/species/Anlaysis/{sp}_forecast.csv'
        
    log:
        err = "logs/Analysis/{sp}_forecast.err",
        out = "logs/Analysis/{sp}_forecast.out"
    conda:
	    '../envs/Analysis.yaml'

    shell:
        r"""
                  
        Rscript workflow/scripts/Analysis/forecast.R \
        {input.data} \
        {input.params} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

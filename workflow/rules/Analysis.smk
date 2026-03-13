configfile: 'config/InternshipSMKConfig.yaml'

type=["0" , "1", "All"]

rule forecast:
    '''
  forecast training on S1 and testing on S2
    '''
    input:
        data = 'results/species/AssignVariables/{sp}_allv.csv',
        params = 'results/species/RF/RF_{sp}_params.csv'

    output:
        out='results/species/Anlaysis/{sp}_forecast_{type}.csv'
        
    log:
        err = "logs/Analysis/{sp}_forecast_{type}.err",
        out = "logs/Analysis/{sp}_forecast_{type}.out"
    conda:
	    '../envs/Analysis.yaml'
    
    params:
        data_type = "{type}"

    shell:
        r"""
        
        Rscript workflow/scripts/Analysis/forecast.R \
        {input.data} \
        {input.params} \
        "{params.data_type}"\
        {output.out} \
        >{log.out} 2> {log.err}
        """

onfigfile: 'config/InternshipSMKConfig.yaml'

rule RF_optimalParam:
    '''
   Find the optimla pàrameers for the BRT using the S2 data 
   WARNING !!! This rule takes a lot of time instead I used the cl_param_BRT2_v2.R code and used it in the cluster 
    '''
    input:
        occurrences = rules.formatDataset.output.out 

    output:
        out='results/species/RF/rep_optimalRF.csv'
        
    log:
        err = "logs/RF/RF_optimalParam.err",
        out = "logs/RF/RF_optimalParam.out"
    conda:
	    '../envs/RF.yaml'

    params:
        tribe_list = config['Tribe_list'][0]["LH"]

    shell:
        r"""      
        Rscript workflow/scripts/RF/RF_optimalParam.R \
        {input.occurrences} \
        "{params.tribe_list}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """



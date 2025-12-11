configfile: 'config/InternshipSMKConfig.yaml'

rule RF_optimalParam:
    '''
   Find the optimla pàrameers for the BRT using the S2 data 
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

rule RF_extractParams:
    input:
        paramOpt = rules.RF_optimalParam.output.out
        
    output:
        table = 'results/species/RF/RF_LH_top5_params.csv',
        out = 'results/species/RF/RF_LH_params.csv'

    log:
        err = "logs/RF/RF_extractParams.err",
        out = "logs/RF/RF_extractParams.out"
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
       occurrences = rules.formatDataset.output.out,
       RFparamOpt = rules.RF_extractParams.output.out

    output:
        out = 'results/species/RF/RF_LH_relimpAUC.csv'

    log:
        err = "logs/RF/RF_apply.err",
        out = "logs/RF/RF_apply.out"
    conda:
	    '../envs/RF.yaml'
    
    params:
        tribe_list = config['Tribe_list'][0]["LH"]

    shell:
        r"""      
        Rscript workflow/scripts/RF/RF_apply.R \
        {input.occurrences} \
        "{params.tribe_list}"\
        {input.RFparamOpt}\
        {output.out} \
        >{log.out} 2> {log.err}
        """
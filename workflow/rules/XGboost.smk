configfile: 'config/InternshipSMKConfig.yaml'

model=[ "Equal", "Weighted"]

rule applyXGboost:
    '''
    XGboost to weighted or eql 
    '''
    input:
        data = 'results/species/AssignVariables/{sp}_allv.csv'

    output:
        out_metrics = 'results/species/XGboost/{sp}_applyXGboost_metrics_{model}.csv',
        out_shap = 'results/species/XGboost/{sp}_applyXGboost_shap_{model}.csv'
        
    log:
        err = "logs/XGboost/{sp}_applyXGboost_{model}.err",
        out = "logs/XGboost/{sp}_applyXGboost_{model}.out"
    conda:
	    '../envs/XGboost.yaml'
    
    params:
        model_type = "{model}"

    shell:
        r"""
        python workflow/scripts/XGboost/applyXGboost.py \
        {input.data} \
        "{params.model_type}"\
        {output.out_metrics} \
        {output.out_shap} \
        >{log.out} 2> {log.err}
        """

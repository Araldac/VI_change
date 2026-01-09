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
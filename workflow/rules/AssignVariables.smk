configfile: 'config/InternshipSMKConfig.yaml'

rule add_Climate_PA:
    '''
   Add climate and pseudo-absences 
    '''
    input:
        occurrences = 'results/species/DatasetPrep/{sp}_selE.csv',
        climate = rules.Climate_DownloadPCA.output.out,
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData',
        distributionData = 'data/distribution/LH_dis.RData'

    output:
        out='results/species/AssignVariables/{sp}_PA.csv'
        
    log:
        err = "logs/AssignVariables/{sp}_add_Climate_PA.err",
        out = "logs/AssignVariables/{sp}_add_Climate_PA.out"
    conda:
	    '../envs/AssignVariables.yaml'

    params:
        sp_name = lambda wildcards: config["species_name"][wildcards.sp]

    shell:
        r"""
                  
        Rscript workflow/scripts/AssignVariables/add_Climate_PA.R \
        {input.occurrences} \
        {input.climate} \
        {input.antmaps_pol} \
        "{params.sp_name}" \
        {input.distributionData} \
        {output.out} \
        >{log.out} 2> {log.err}
        """


rule add_BioticRelatedness:
    '''
    General rule to Clean, Thin and Select Exotic occurrences, the following rules decompose these three steps
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_PA.csv',
        distributionData = 'data/distribution/LH_dis.RData' ,
        relatedSpecies = 'data/variables/Biotic/{sp}_tribe_genus.csv',
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/AssignVariables/{sp}_PA_B.csv'
        
    log:
        err = "logs/AssignVariables/{sp}_add_BioticRelatedness.err",
        out = "logs/AssignVariables/{sp}_add_BioticRelatedness.out"

    conda:
	    '../envs/AssignVariables.yaml'

    params:
        sp_name = lambda wildcards: config["species_name"][wildcards.sp],
        tribe_list = lambda wildcards: config["Tribe_list"][wildcards.sp]

    shell:
        r"""
        Rscript workflow/scripts/AssignVariables/add_BioticRelatedness.R \
        {input.occurrences} \
        {input.relatedSpecies} \
        {input.distributionData} \
        {input.antmaps_pol} \
        "{params.sp_name}" \
        "{params.tribe_list}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule add_Trade:
    '''
    Adding cumulative trade values form Sum Import function
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_PA_B.csv' ,
        sumimport = 'results/variables/Trade_{sp}_SumImports.csv'

    output:
        out='results/species/AssignVariables/{sp}_PA_B_T.csv'
        
    log:
        err = "logs/AssignVariables/{sp}_add_Trade.err",
        out = "logs/AssignVariables/{sp}_add_Trade.out"

    conda:
	    '../envs/AssignVariables.yaml'

    shell:
        r"""
        Rscript workflow/scripts/AssignVariables/add_Trade.R \
        {input.occurrences} \
        {input.sumimport} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule add_HM:
    '''
    Add human modification values 
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_PA_B_T.csv' ,
        hm1970 = 'results/variables/hm_1970.RData',
        hm2025 = 'results/variables/hm_2025.RData'

    output:
        out='results/species/AssignVariables/{sp}_PA_B_T_HM.csv'
        
    log:
        err = "logs/AssignVariables/{sp}_add_HM.err",
        out = "logs/AssignVariables/{sp}_add_HM.out"

    conda:
	    '../envs/AssignVariables.yaml'

    shell:
        r"""
        Rscript workflow/scripts/AssignVariables/add_HM.R \
        {input.occurrences} \
        {input.hm1970} \
        {input.hm2025} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule formatDataset:
    '''
    Format dataset to have : longitude, latitude, occurences, S1 PA_N
    '''
    input:
        occurrences = 'results/species/AssignVariables/{sp}_PA_B_T_HM.csv'
        
    output:
        out='results/species/AssignVariables/{sp}_allv.csv'
        
    log:
        err = "logs/AssignVariables/{sp}_formatDataset.err",
        out = "logs/AssignVariables/{sp}_formatDataset.out"

    conda:
	    '../envs/AssignVariables.yaml'

    shell:
        r"""
        Rscript workflow/scripts/AssignVariables/formatDataset.R \
        {input.occurrences} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

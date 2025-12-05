configfile: 'config/InternshipSMKConfig.yaml'

rule add_Climate_PA:
    '''
   Add climate and pseudo-absences 
    '''
    input:
        occurrences = rules.selectE.output.out ,
        climate = rules.Climate_DownloadPCA.output.out,
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData',
        distributionData = 'data/distribution/LH_dis.RData'

    output:
        out='results/species/AssignVariables/LH_PA.csv'
        
    log:
        err = "logs/AssignVariables/add_Climate_PA.err",
        out = "logs/AssignVariables/add_Climate_PA.out"
    conda:
	    '../envs/AssignVariables.yaml'

    params:
        sp_name = config['species_name'][0] # Access first species name from list

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
        occurrences = rules.add_Climate_PA.output.out ,
        distributionData = 'data/distribution/LH_dis.RData' ,
        relatedSpecies = 'data/variables/Biotic/LH_tribe_genus.csv',
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/AssignVariables/LH_PA_B.csv'
        
    log:
        err = "logs/AssignVariables/add_BioticRelatedness.err",
        out = "logs/AssignVariables/add_BioticRelatedness.out"

    conda:
	    '../envs/AssignVariables.yaml'

    params:
        sp_name = config['species_name'][0] ,# Access first species name from list
        tribe_list = config['Tribe_list'][0]["LH"]

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

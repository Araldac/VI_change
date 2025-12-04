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
        out='results/species/LH_PA.csv'
        
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
#set -euo pipefail
#    ulimit -s 65536  
rule add_BioticRelatedness:
    '''
    General rule to Clean, Thin and Select Exotic occurrences, the following rules decompose these three steps
    '''
    input:
        occurrences = rules.add_Climate_PA.output.out ,
        distributionData = 'data/distribution/LH_dis.RData' ,
        relatedSpecies = 'data/variables/biotic/LH_tribe_genus.csv',
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        #out='results/species/LH_PA_B.csv'
        
    log:
        err = "logs/AssignVariables/cleanThinSelectE.err",
        out = "logs/AssignVariables/cleanThinSelectE.out"
    conda:
	    '../envs/AssignVariables.yaml'

    shell:
        r"""
        Rscript workflow/scripts/AssignVariables/add_BioticRelatedness.R \
        {input.occurrences} \
        {input.distributionData} \
        {input.relatedSpecies} \
        {input.antmaps_pol} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

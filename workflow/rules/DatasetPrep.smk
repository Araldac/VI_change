configfile: 'config/InternshipSMKConfig.yaml'

#rule cleanThinSelectE:
#    '''
#    General rule to Clean, Thin and Select Exotic occurrences, the following rules decompose these three steps
#    '''
#    input:
#        occurrencesGBIF = 'data/occurrences/LH_GBIF.csv' ,
#        distributionData = 'data/distribution/LH_dis.RData' ,
#       sf_lak = 'data/raster/sf/lak.RData' ,
#        sf_land = 'data/raster/sf/land.RData' ,
#        sf_sea = 'data/raster/sf/sea.RData' ,
#        sf_unland = 'data/raster/sf/unland.RData',
#        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'
#
#    output:
#        out='results/species/DatasetPrep/LH_ctsE.csv'
#        
#    log:
#        err = "logs/DatasetPrep/cleanThinSelectE.err",
#        out = "logs/DatasetPrep/cleanThinSelectE.out"
#    conda:
#	    '../envs/DatasetPrep.yaml'
#
#    shell:
#        r"""
#        Rscript workflow/scripts/DatasetPrep/cleanThinSelectE.R \
#        {input.occurrencesGBIF} \
#        {input.distributionData} \
#        {input.sf_lak} \
#        {input.sf_land} \
#        {input.sf_sea} \
#        {input.sf_unland} \
#        {input.antmaps_pol} \
#        {output.out} \
#        >{log.out} 2> {log.err}
#        """
rule selectE:
    '''
    Selecting only the Exotic occurrences

    '''
    input:
        occurrencesGBIF = 'data/occurrences/{sp}_GBIF.csv' ,
        #data_thinned = 'results/species/DatasetPrep/{sp}_thin.csv',
        distributionData = 'data/distribution/LH_dis.RData' ,
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/DatasetPrep/{sp}_selE.csv'
        
    log:
        err = "logs/DatasetPrep/{sp}_selectE.err",
        out = "logs/DatasetPrep/{sp}_selectE.out"
    conda:
	    '../envs/DatasetPrep.yaml'

    params:
        sp_name = lambda wildcards: config["species_name"][wildcards.sp]

    shell:     
        r"""
        Rscript workflow/scripts/DatasetPrep/selectE.R \
            {input.occurrencesGBIF} \
            {input.distributionData} \
            {input.antmaps_pol} \
            "{params.sp_name}" \
            {output.out} \
            >{log.out} 2>{log.err}
        """


rule cleaning:
    '''
    Cleanning data using methods from Sebastien Ollier.
    
    '''
    input:
        selE = 'results/species/DatasetPrep/{sp}_selE.csv' ,
        #distributionData = 'data/distribution/LH_dis.RData' ,
        sf_lak = 'data/raster/sf/lak.RData' ,
        sf_land = 'data/raster/sf/land.RData' ,
        sf_sea = 'data/raster/sf/sea.RData' ,
        sf_unland = 'data/raster/sf/unland.RData'
        #antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/DatasetPrep/{sp}_cle.csv'
        
    log:
        err = "logs/DatasetPrep/{sp}_cleaning.err",
        out = "logs/DatasetPrep/{sp}_cleaning.out"
    conda:
	    '../envs/DatasetPrep.yaml'

    shell:
        r"""
        Rscript workflow/scripts/DatasetPrep/cleaning.R \
        {input.selE} \
        {input.sf_lak} \
        {input.sf_land} \
        {input.sf_sea} \
        {input.sf_unland} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule thinning:
    '''
    Thinning data using methods from Sebastien Ollier.
    
    '''
    input:
        data_cleaned = 'results/species/DatasetPrep/{sp}_cle.csv',
        raster = 'data/variables/Climate/pca_Worldclim_25p_2.tif'
        #distributionData = 'data/distribution/LH_dis.RData' ,
        #sf_lak = 'data/raster/sf/lak.RData' ,
        #sf_land = 'data/raster/sf/land.RData' ,
        #sf_sea = 'data/raster/sf/sea.RData' ,
        #sf_unland = 'data/raster/sf/unland.RData',
        #antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/DatasetPrep/{sp}_thin.csv'
        
    log:
        err = "logs/DatasetPrep/{sp}_thinning.err",
        out = "logs/DatasetPrep/{sp}_thinning.out"
    conda:
	    '../envs/DatasetPrep.yaml'

    shell:
        r"""
        Rscript workflow/scripts/DatasetPrep/thinning.R \
        {input.data_cleaned} \
        {input.raster} \
        {output.out} \
        >{log.out} 2> {log.err}
        """


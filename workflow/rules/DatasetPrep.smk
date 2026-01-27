configfile: 'config/InternshipSMKConfig.yaml'

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
        sf_lak = 'data/raster/sf/lak.RData' ,
        sf_land = 'data/raster/sf/land.RData' ,
        sf_sea = 'data/raster/sf/sea.RData' ,
        sf_unland = 'data/raster/sf/unland.RData'

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
    Thinning data using grid with geothinner package and the climate raster as grid cell
    
    '''
    input:
        data_cleaned = 'results/species/DatasetPrep/{sp}_cle.csv',
        raster = 'data/variables/Climate/pca_Worldclim_25p_2.tif'

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


configfile: 'config/InternshipSMKConfig.yaml'

rule cleanThinSelectE:
    '''
    General rule to Clean, Thin and Select Exotic occurrences, the following rules decompose these three steps
    '''
    input:
        occurrencesGBIF = 'data/occurrences/LH_GBIF.csv' ,
        distributionData = 'data/distribution/LH_dis.RData' ,
        sf_lak = 'data/raster/sf/lak.RData' ,
        sf_land = 'data/raster/sf/land.RData' ,
        sf_sea = 'data/raster/sf/sea.RData' ,
        sf_unland = 'data/raster/sf/unland.RData',
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/LH_ctsE.csv'
        
    log:
        err = "logs/DatasetPrep/cleanThinSelectE.err",
        out = "logs/DatasetPrep/cleanThinSelectE.out"
    conda:
	    '../envs/DatasetPrep.yaml'

    shell:
        r"""
        Rscript workflow/scripts/DatasetPrep/cleanThinSelectE.R \
        {input.occurrencesGBIF} \
        {input.distributionData} \
        {input.sf_lak} \
        {input.sf_land} \
        {input.sf_sea} \
        {input.sf_unland} \
        {input.antmaps_pol} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule cleaning:
    '''
    Cleanning data using methods from Sebastien Ollier.
    
    '''
    input:
        occurrencesGBIF = 'data/occurrences/LH_GBIF.csv' ,
        #distributionData = 'data/distribution/LH_dis.RData' ,
        sf_lak = 'data/raster/sf/lak.RData' ,
        sf_land = 'data/raster/sf/land.RData' ,
        sf_sea = 'data/raster/sf/sea.RData' ,
        sf_unland = 'data/raster/sf/unland.RData',
        #antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/LH_cle.csv'
        
    log:
        err = "logs/DatasetPrep/cleaning.err",
        out = "logs/DatasetPrep/cleaning.out"
    conda:
	    '../envs/DatasetPrep.yaml'

    shell:
        r"""
        Rscript workflow/scripts/DatasetPrep/cleaning.R \
        {input.occurrencesGBIF} \
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
        data_cleaned = rules.cleaning.output.out,
        #distributionData = 'data/distribution/LH_dis.RData' ,
        #sf_lak = 'data/raster/sf/lak.RData' ,
        #sf_land = 'data/raster/sf/land.RData' ,
        #sf_sea = 'data/raster/sf/sea.RData' ,
        #sf_unland = 'data/raster/sf/unland.RData',
        #antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/LH_thin.csv'
        
    log:
        err = "logs/DatasetPrep/thinning.err",
        out = "logs/DatasetPrep/thinning.out"
    conda:
	    '../envs/DatasetPrep.yaml'

    shell:
        r"""
        Rscript workflow/scripts/DatasetPrep/thinning.R \
        {input.data_cleaned} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule selectE:
    '''
    Sedlecting only the Exotic occurrences
    !!!!!!Attention!!!!! Selecting E part is specific to Linepithme humile, need to chn¡¡ange for other species
    '''
    input:
        data_thinned = rules.thinning.output.out ,
        distributionData = 'data/distribution/LH_dis.RData' ,
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/species/LH_selE.csv'
        
    log:
        err = "logs/DatasetPrep/selectE.err",
        out = "logs/DatasetPrep/selectE.out"
    conda:
	    '../envs/DatasetPrep.yaml'
    params:
        sp_name = config['species_name'][0] # Access first species name from list

    shell:
        r"""
        Rscript workflow/scripts/DatasetPrep/selectE.R \
        {input.data_thinned} \
        {input.distributionData} \
        {input.antmaps_pol} \
        "{params.sp_name}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """
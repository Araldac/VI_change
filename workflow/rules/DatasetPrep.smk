

rule cleanThinSelectE:
    '''
    Cleanning and thinning data using methods from Sebastien Ollier. Sedlecting only the Exotic occurrences
    
    !!!!!!Attention!!!!! Selecting E part is specific to Linepithme humile, need to chn¡¡ange for other species
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


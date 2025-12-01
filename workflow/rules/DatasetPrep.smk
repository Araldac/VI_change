

rule cleanThinSelectE:
    '''
    Cleanning and thinning data using methods from Sebastien Ollier. Sedlecting only the Exotic occurrences
    
    !!!!!!Attention!!!!! Selecting E part is specific to Linepithme humile, need to chn¡¡ange for other species
    '''
    input:
        occurrencesGBIF = 'data/occurrences/LH_GBIF.csv' ,
        distributionData = 'data/distribution/LH_Dis.RData' ,
        sf_lak = 'data/raster/sf/lak.RData' ,
        sf_land = 'data/raster/sf/land.RData' ,
        sf_sea = 'data/raster/sf/sea.RData' ,
        sf_unland = 'data/raster/sf/unland.RData'

    output:
        out='results/species/LH_ctsE.csv'

    shell:
        r"""
                Rscript workflow/scripts/DatasetPrep/cleanThinSelectE.R \
                --occurrencesGBIF {input.occurrencesGBIF} \
                --distributionData {input.distributionData} \
                --sf_lak {input.sf_lak} \
                --sf_land {input.sf_land} \
                --sf_sea {input.sf_sea} \
                --sf_unland {input.sf_unland} \
                --output {output.out} \
                > {log} 2>&1
                """


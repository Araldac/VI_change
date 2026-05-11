configfile: 'config/InternshipSMKConfig.yaml'

#sp = ["LH"]

#print("DEBUG Native_Countries type:", config.get("Native_Countries"))
#print("DEBUG First_Record_Date value:", config.get("First_Record_Date"))
#print("DEBUG species_name type:", config.get("species_name"))
#print("DEBUG species_name type:", config.get("Tribe_list"))

rule Trade_HistoricalInterpolated:
    '''
   From the Tradhist dataset, assign the current country names ot the entries and divide the flow by the counrey size
   Interpolate NAs by the closer data avaliable
    '''
    input:
        trade1 = 'data/variables/Trade/TRADHIST_BITRADE_BITARIFF_1.xlsx' ,
        trade2 = 'data/variables/Trade/TRADHIST_BITRADE_BITARIFF_2.xlsx' ,
        trade3 = 'data/variables/Trade/TRADHIST_BITRADE_BITARIFF_3.xlsx' ,
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'

    output:
        out='results/variables/Trade_HistoricalInterpolated.csv'
        
    log:
        err = "logs/VariablesPrep/Trade_HistoricalInterpolated.err",
        out = "logs/VariablesPrep/Trade_HistoricalInterpolated.out"
    conda:
	    '../envs/VariablesPrep.yaml'

    shell:
        r"""
        Rscript workflow/scripts/VariablesPrep/Trade_HistoricalInterpolated.R \
        {input.trade1} \
        {input.trade2} \
        {input.trade3} \
        {input.antmaps_pol} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule Trade_SumImports:
    '''
    From the first occurrences at each country, sum the imports from the countries where the species is present. Needs to be adapted to each species
    '''
    input:
        historicaltrade = rules.Trade_HistoricalInterpolated.output.out ,
        firstRecord_Countries = 'data/variables/Trade/{sp}_first_records.csv' ,

    output:
        out='results/variables/Trade_{sp}_SumImports.csv'
        
    log:
        err = "logs/VariablesPrep/Trade_{sp}_Sumimports.err",
        out = "logs/VariablesPrep/Trade_{sp}_Sumimports.out"
    conda:
	    '../envs/VariablesPrep.yaml'
    
    params:
        firstRecord = lambda wildcards: config["First_Record_Date"][wildcards.sp],
        nativeCountries = lambda wildcards: config["Native_Countries"][wildcards.sp]
        #firstRecord = lambda wildcards: (
        #    [d[wildcards.sp] for d in config["First_Record_Date"] if wildcards.sp in d]
        #    ),
        #nativeCountries = lambda wildcards: (
        #    [d[wildcards.sp] for d in config["Native_Countries"] if wildcards.sp in d]
        #    )

        #nativeCountries = config["Native_Countries"][0]["LH"],
        #firstRecord = config["First_Record_Date"][0]["LH"]

    shell:
        r"""
        Rscript --vanilla workflow/scripts/VariablesPrep/Trade_SumImports.R \
        {input.historicaltrade} \
        {input.firstRecord_Countries} \
        "{params.firstRecord}" \
        "{params.nativeCountries}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """


rule Climate_DownloadPCA:
    '''
    Load the 2PCA Axes from the NAS - Error: ca't be loaded from the wsl, needs to be runned on Windows
    '''
    input:
        ax1 = 'data/variables/Climate/pca_Worldclim_25p_1.tif',
        ax2 = 'data/variables/Climate/pca_Worldclim_25p_2.tif'
    output:
        out='results/variables/Climate_PCA.tif'
        
    log:
        err = "logs/VariablesPrep/Climate_DownloadPCA.err",
        out = "logs/VariablesPrep/Climate_DownloadPCA.out"
    conda:
	    '../envs/VariablesPrep.yaml'

    shell:
        r"""
        Rscript --vanilla workflow/scripts/VariablesPrep/Climate_DownloadPCA.R \
        {input.ax1} \
        {input.ax2} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule HM_RasterCreation:
    '''
    From the first occurrences at each country, sum the imports from the countries where the species is present. Needs to be adapted to each species
    '''
    input:
        pop_1970 = 'data/variables/HM/popd_1970AD.asc',
        pop_2025 = 'data/variables/HM/popd_2025AD.asc',
        anth_1970 = 'data/variables/HM/anthromes1970AD.asc',
        anth_2025 = 'data/variables/HM/anthromes2025AD.asc',

    output:
        out_1970='results/variables/hm_1970.tif',
        out_2025='results/variables/hm_2025.tif'
    log:
        err = "logs/VariablesPrep/HM_RasterCreation.err",
        out = "logs/VariablesPrep/HM_RasterCreation.out"
    conda:
	    '../envs/VariablesPrep.yaml'
    

    shell:
        r"""
        Rscript --vanilla workflow/scripts/VariablesPrep/HM_RasterCreation.R \
        {input.pop_1970} \
        {input.pop_2025} \
        {input.anth_1970} \
        {input.anth_2025} \
        {output.out_1970} \
        {output.out_2025} \
        >{log.out} 2> {log.err}
        """

rule HM_ComparisonHF2016:
    '''
    From the first occurrences at each country, sum the imports from the countries where the species is present. Needs to be adapted to each species
    '''
    input:
        pop_2016= 'data/variables/HM/popd_2016AD.asc',
        anth_2016 = 'data/variables/HM/anthromes2016AD.asc',
        HF_2016 = 'data/variables/HM/CSP_human_modif_index_30s_2016.tif'


    output:
        out_2016='results/variables/hm_2016.RData'
    log:
        err = "logs/VariablesPrep/HM_ComparisonHF2016.err",
        out = "logs/VariablesPrep/HM_ComparisonHF2016.out"
    conda:
	    '../envs/VariablesPrep.yaml'
    

    shell:
        r"""
        Rscript --vanilla workflow/scripts/VariablesPrep/HM_ComparisonHF2016.R \
        {input.pop_2016} \
        {input.anth_2016} \
        {input.HF_2016} \
        {output.out_2016} \
        >{log.out} 2> {log.err}
        """

rule Biotic_DistributionN:
    '''
    Keep only native distirbution of species 
    '''
    input:
        distributionData = 'data/distribution/LH_dis.RData'
        
    output:
        out='results/variables/N_dis.RData'
    log:
        err = "logs/VariablesPrep/Biotic_DistributionN.err",
        out = "logs/VariablesPrep/Biotic_DistributionN.out"
    conda:
	    '../envs/VariablesPrep.yaml'
    
    shell:
        r"""
        Rscript --vanilla workflow/scripts/VariablesPrep/Biotic_DistributionN.R \
        {input.distributionData} \
        {output.out} \
        >{log.out} 2> {log.err}
        """

rule Biotic_MPD:
    '''
    create and mpd matrix for all the species studied and each bentity
    '''
    '''
    Keep only native distirbution of species 
    '''
    input:
        distributionData = 'results/variables/N_dis.RData',
        tree = 'data/variables/Biotic/clean_genus_tree.tre',
        antmaps_pol = 'data/distribution/AntmapsPolGeneral.RData'
        
    output:
        output_MPD ='results/variables/Biotic_MPD_{sp}.csv'

    log:
        err = "logs/VariablesPrep/Biotic_MPD_{sp}.err",
        out = "logs/VariablesPrep/Biotic_MPD_{sp}.out"
    conda:
	    '../envs/VariablesPrep.yaml'
    
    params:
        sp_name = lambda wildcards: config["species_name"][wildcards.sp]
    
    shell:
        r"""
        Rscript --vanilla workflow/scripts/VariablesPrep/Biotic_MPD.R \
        {input.distributionData} \
        {input.antmaps_pol}\
        {input.tree}\
        "{params.sp_name}" \
        {output.output_MPD} \
        >{log.out} 2> {log.err}
        """



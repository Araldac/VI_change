configfile: 'config/InternshipSMKConfig.yaml'

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
        firstRecord_Countries = 'data/variables/Trade/LH_first_records.csv' ,

    output:
        out='results/variables/Trade_LH_SumImports.csv'
        
    log:
        err = "logs/VariablesPrep/Trade_LH_Sumimports.err",
        out = "logs/VariablesPrep/Trade_LH_Sumimports.out"
    conda:
	    '../envs/VariablesPrep.yaml'
    
    params:
        nativeCountries = config["Native_Countries"][0]["LH"],
        firstRecord = config["First_Record_Date"][0]["LH"]

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
    Load the 2PCA Axes from the NAS 
    '''
    output:
        out='results/variables/Climate_PCA.RData'
        
    log:
        err = "logs/VariablesPrep/Climate_DownloadPCA.err",
        out = "logs/VariablesPrep/Climate_DownloadPCA.out"
    conda:
	    '../envs/VariablesPrep.yaml'

    params:
        path_NAS =  config["Path"]["OB_NAS_Climate"]

    shell:
        r"""
        Rscript --vanilla workflow/scripts/VariablesPrep/Climate_DownloadPCA.R \
        "{params.path_NAS}" \
        {output.out} \
        >{log.out} 2> {log.err}
        """






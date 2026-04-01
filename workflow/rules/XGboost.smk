configfile: 'config/InternshipSMKConfig.yaml'

model=[ "Equal", "Weighted"]
method = ["XG", "RF"]

rule applyXGboost:
    '''
    XGboost or RF fro shap to weighted or eql 
    '''
    input:
        data = 'results/species/AssignVariables/{sp}_allv.csv'

    output:
        out_metrics = 'results/species/XGboost/{sp}_applyXGboost_metrics_{model}_{method}.csv',
        out_shap = 'results/species/XGboost/{sp}_applyXGboost_shap_{model}_{method}.csv'
        
    log:
        err = "logs/XGboost/{sp}_applyXGboost_{model}_{method}.err",
        out = "logs/XGboost/{sp}_applyXGboost_{model}_{method}.out"
    conda:
	    '../envs/XGboost.yaml'
    
    params:
        model_type = "{model}",
        methods_type = "{method}"

    shell:
        r"""
        python workflow/scripts/XGboost/applyXGboost.py \
        {input.data} \
        "{params.model_type}"\
        "{params.methods_type}"\
        {output.out_metrics} \
        {output.out_shap} \
        >{log.out} 2> {log.err}
        """

rule bind_SHAP:
    '''
    XGboost to weighted or eql 
    '''
    input:
        list_files = expand("results/species/XGboost/{species}_applyXGboost_shap_{t}_{met}.csv", species=sp, t = model, met= method)

    output:
        out = 'results/species/XGboost/bind_SHAP.csv'
        
    log:
        err = "logs/XGboost/bind_SHAP.err",
        out = "logs/XGboost/bind_SHAP.out"
    conda:
	    '../envs/XGboost.yaml'

#    shell:
#        r"""
#        Rscript workflow/scripts/XGboost/bind_results.R \
#        {input.list_files} \
#        {output.out} \
#        >{log.out} 2> {log.err}
#        """
    shell:
        r"""
cat <<'EOF' > {rule}.$$.tmp.R

library(dplyr)
library(stringr)
list_files <- strsplit('{input.list_files}', " +")
list_files<-unlist(list_files)

print(list_files)

all_oc <- lapply(list_files, function(f) {{
df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)

a <- str_extract(f, "RF")
if (is.na(a)){{
    a <- stringr::str_extract(f, "XG")
}}

if(!"PA_N" %in% colnames(df))
 {{
   df$PA_N <- c(rep(1:10, each= 10), rep(11:20, each= 10))
 }}
 

df$PA_N <- as.character(df$PA_N)
df$Method <- a
return(df)
}}) %>% dplyr::bind_rows()  

head(all_oc)
write.csv(all_oc, '{output.out}')

EOF
Rscript {rule}.$$.tmp.R
rm {rule}.$$.tmp.R

        """

rule bind_metrics:
    '''
    XGboost to weighted or eql 
    '''
    input:
        list_files = expand("results/species/XGboost/{species}_applyXGboost_metrics_{t}_{met}.csv", species=sp, t = model, met= method)

    output:
        out = 'results/species/XGboost/bind_metrics.csv'
        
    log:
        err = "logs/XGboost/bind_metrics.err",
        out = "logs/XGboost/bind_metrics.out"
    conda:
	    '../envs/XGboost.yaml'

#    shell:
#        r"""
#        Rscript workflow/scripts/XGboost/bind_results.R \
#        {input.list_files} \
#        {output.out} \
#        >{log.out} 2> {log.err}
#        """
    shell:
        r"""
cat <<'EOF' > {rule}.$$.tmp.R

library(dplyr)
library(stringr)
list_files <- strsplit('{input.list_files}', " +")
list_files<-unlist(list_files)

print(list_files)

all_oc <- lapply(list_files, function(f) {{
a <- str_extract(f, "Weighted")
if (is.na(a)){{
    a <- stringr::str_extract(f, "Equal")
}}

df <- read.csv(f, sep = ",", stringsAsFactors = FALSE)
df$model <- a
df$PA_N <- as.character(df$PA_N)
return(df)
}}) %>% dplyr::bind_rows()  

head(all_oc)
write.csv(all_oc, '{output.out}')

EOF
Rscript {rule}.$$.tmp.R
rm {rule}.$$.tmp.R
        """
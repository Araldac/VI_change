###############################################################################
# Project: Intersnship UNIL 2026
# Athor: Aitana
# Description: apply XGboost weigthed
# Date: 03/12/2025
################################################################################
import sys

occurrences = sys.argv[1]
model = sys.argv[2]
methods = sys.argv[3]
output_metrics = sys.argv[4]
output_shap_raw = sys.argv[5]

#import modules 
import glob
import os
import time
import warnings
import sklearn
import shap
import xgboost as xgb
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import enlighten
from tqdm import tqdm 

from sklearn.ensemble import RandomForestClassifier
from sklearn.utils.class_weight import compute_sample_weight
from sklearn.metrics import (
    balanced_accuracy_score,
    confusion_matrix,
    f1_score,
    precision_score,
    recall_score,
    roc_auc_score,
)
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.svm import SVC
from xgboost import XGBClassifier

################################################################################
#Functions
################################################################################

def features_ml_models(X, y, methods, model, random_seed, variables, is_binary=False, is_balanced = False, plot = False):
    """Train and evaluate ML models on feature data, extracting both metrics and confusion matrices.

    Args:
        X (pd.DataFrame): Feature matrix
        y (pd.Series): Target variable (either binary or multiclass)
        random_seed (int): Random seed for the bootstrap sample
        is_binary (bool): Whether this is binary classification

    Returns:
        tuple: (metrics_df, confusion_df, SHAP_values) with model performance metrics and confusion matrix data
    """
    X = X.reset_index(drop=True)
    
    # Encode target variables
    target_encoder = LabelEncoder()
    y_encoded = target_encoder.fit_transform(y)

    # Special handling for binary classification to ensure proper encoding
    if is_binary and len(target_encoder.classes_) == 2:
        # Ensure "Active" is encoded as 1 and "Inactive" as 0 if these are the class names
        if (
            "Active" in target_encoder.classes_
            and "Inactive" in target_encoder.classes_
        ):
            if target_encoder.transform(["Active"])[0] != 1:
                target_encoder.classes_ = target_encoder.classes_[::-1]
                y_encoded = target_encoder.transform(y)

    # Bootstrap sample: presences and background points are drawn separately, with replacement,
    # keeping their numbers. The model is trained on this sample and evaluated on the rows that
    # were not drawn (out-of-bag rows).
    rng = np.random.default_rng(random_seed)
    idx = np.concatenate([
        rng.choice(np.flatnonzero(y_encoded == k), size=np.sum(y_encoded == k), replace=True)
        for k in np.unique(y_encoded)
    ])
    oob = np.setdiff1d(np.arange(len(y_encoded)), idx)
    X_train, y_train = X.iloc[idx], y_encoded[idx]
    X_test, y_test = X.iloc[oob], y_encoded[oob]

    if methods == "XG":    
        models = {
            "XGB": XGBClassifier(random_state=random_seed)
        }

    # Create lists to store the results and confusion matrix data
    metrics_results = []
    confusion_data = []

    # Iterate over the models
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        for model_name, model in models.items():
            start_time = time.time()


            # Fit the model with 
            
                        # Fit the model with
            if is_balanced == False:  
                model.fit(X_train, y_train, sample_weight=compute_sample_weight("balanced", y_train) )
            
            else:
                model.fit(X_train, y_train )

            # Predictions
            y_pred = model.predict(X_test)
            y_pred_proba = model.predict_proba(X_test)

            # Calculate accuracy
            accuracy = model.score(X_test, y_test)

            # Calculate ROC AUC - handle binary and multiclass differently
            if is_binary:
                roc_auc = roc_auc_score(y_test, y_pred_proba[:, 1])
            else:
                roc_auc = roc_auc_score(
                    y_test, y_pred_proba, multi_class="ovr", average="weighted"
                )

            # Calculate balanced accuracy
            balanced_accuracy = balanced_accuracy_score(y_test, y_pred)

            # Calculate F1 score
            f1 = f1_score(y_test, y_pred, average="weighted")

            # Calculate precision
            precision = precision_score(
                y_test, y_pred, average="weighted", zero_division=0
            )

            # Calculate recall
            recall = recall_score(y_test, y_pred, average="weighted")

            # SHAP values 

            if model_name == "XGB":
                #print("XGB")
                explainer = shap.TreeExplainer(model)
                explanation = explainer(X_train)
                shap_values = explanation.values
                # make sure the SHAP values add up to marginal predictions
                #np.abs(shap_values.sum(axis=1) + explanation.base_values - y_test).max()
                
                #new_index = [ix for ix_test_fold in ix_test for ix in ix_test_fold]
                if plot == True :
                    shap.plots.beeswarm(explanation) #, feature_names=X.columns)
                #shap.summary_plot(explanation)
                
                #shap_values_df = pd.DataFrame(shap_values,  columns=X.columns)
                vals= np.abs(shap_values).mean(0)

                feature_importance = pd.DataFrame(list(zip(X_train.columns, vals)))
                feature_importance_df = feature_importance.T


            train_time = time.time() - start_time

            # Append the metrics to the results list


            # Get and process confusion matrix
            cm = confusion_matrix(y_test, y_pred)

            # Convert confusion matrix to DataFrame format
            for i in range(len(cm)):
                for j in range(len(cm)):
                    confusion_data.append(
                        {
                            "model": model_name,
                            "actual_label": i,
                            "predicted_label": j,
                            "count": cm[i][j],
                        }
                    )

            #print(f"{model_name} Done")

    # Convert results to DataFrames
    confusion_df = pd.DataFrame(confusion_data)

    # Map the encoded labels back to original classes if possible
    original_classes = target_encoder.classes_
    if len(confusion_df) > 0:
        confusion_df["actual_label"] = confusion_df["actual_label"].apply(
            lambda x: original_classes[x] if x < len(original_classes) else x
        )
        confusion_df["predicted_label"] = confusion_df["predicted_label"].apply(
            lambda x: original_classes[x] if x < len(original_classes) else x
        )

        metrics_results.append(
        {
            "model": model,
            "methods": methods, 
            "roc_auc": roc_auc,
            "accuracy": accuracy,
            "balanced_acc": balanced_accuracy,
            "f1_score": f1,
            "precision": precision,
            "recall": recall,
            "time": train_time,
            "TP" : confusion_df.loc[3, "count"],
            "TN" : confusion_df.loc[0, "count"],
            "FP" : confusion_df.loc[1, "count"],
            "FN" :  confusion_df.loc[2, "count"]
        }

    )
        metrics_df = pd.DataFrame(metrics_results)
        
    return metrics_df, feature_importance_df


################################################################################
#Code
################################################################################

#load files
a = pd.read_csv(occurrences)
print(a.loc[1, 'Taxa'])
sp_name = a.loc[1, 'Taxa']

#set parameters
S_levels = ['S1', 'S2']
variables = ["PCA_1", "PCA_2", "HM", "trade", "MPD"]


if model == "Weighted":
    # Initialize storage
    metrics_rows = []
    shap_values_rows = []

    replicates = 100
    seed = 0     
    #max_count = replicates*2
    #manager = enlighten.get_manager()
    #tocks = manager.counter(total=max_count, desc='Tocks', unit='tocks')

    for s in S_levels:
        print(f"Starting models Weighted for {sp_name} for s {s}")
        for rep in tqdm(range(1, replicates + 1)):
            #print(f"species {sp_name} s {s} rep {seed}/100")
            # 1. Filter or Prepare the specific dataset slice
            mask = ((a['S'] == s))
            x = a.loc[mask, variables]
            Y = a.loc[mask, 'Occurrence']

            # 2. Run your function
            metrics_df, shap_df =  features_ml_models(x, Y,methods, model, seed, variables, is_binary=True, is_balanced=False) 
            

            # 3. Store Metrics
            # Add metadata columns
            metrics_df['Taxa'] = sp_name
            metrics_df['S'] = s
            metrics_df['PA_N'] = "Weighted"
            metrics_df['Replicate'] = rep
            metrics_df["Method"] = methods
            
            metrics_rows.append(metrics_df)
                
            # 3. Store SHAP
            # Add metadata columns
            shap_df.columns = shap_df.iloc[0]
            shap_df = shap_df[1:]
            shap_df['Taxa'] = sp_name
            shap_df['S'] = s
            shap_df['PA_N'] = "Weighted"
            shap_df['Model'] = "Weighted"
            shap_df['Replicate'] = rep
            shap_df["Method"] = methods
            
            shap_values_rows.append(shap_df)

            seed += 1
            #tocks.update()
            

    #manager.stop()

    # 5. Combine all metrics into one DataFrame and save as csv
    final_metrics_df = pd.concat(metrics_rows, ignore_index=True)
    final_metrics_df.to_csv(output_metrics)

    shap_final_df = pd.concat(shap_values_rows, ignore_index=True)

    #order columns 
    cols = ['Taxa', 'S', 'Method', 'PA_N', 'Replicate', 'Model', 'PCA_1', 'PCA_2', 'HM', 'trade', 'MPD']
    available_cols = [c for c in cols if c in shap_final_df.columns]
    
    shap_final_df = shap_final_df[available_cols]

    print(shap_final_df.head())
    shap_final_df.to_csv(output_shap_raw)

    
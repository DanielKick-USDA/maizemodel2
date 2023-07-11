# +
import os, re, json, shutil 

#os.system('pip install tqdm') # TODO add to dockerfile

# jupyter labextension install jupyterlab-plotly # run in shell
# ^ this doesn't look to be working. Here's the work around: save each plot to one file name
# fig.write_html('00_plotly_current.html')
# then in a split window one need only refresh to get the current plot. 
# jk changing the renderer fixes this -v  # https://stackoverflow.com/questions/54064245/plot-ly-offline-mode-in-jupyter-lab-not-displaying-plots
import plotly.io as pio
pio.renderers.default = 'iframe' # or 'notebook' or 'colab' or 'jupyterlab'

import numpy as np
import pandas as pd
import math

import scipy.stats as stats        
from scipy import stats # for qq plot
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score
from sklearn.decomposition import PCA

    # import tensorflow as tf
    # from tensorflow import keras
    # from tensorflow.keras import layers
    # import tf_keras_vis
    # from tf_keras_vis.saliency import Saliency
    # from tf_keras_vis.utils import normalize

# for simple MLP for ensembling
from sklearn.neural_network import MLPRegressor

    # import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px
import plotly.figure_factory as ff
import plotly.graph_objects as go

import datetime # For timing where %time doesn't work
import tqdm
import pickle as pkl
# useful pattern for speeding up runs ----v
# path = "./data_intermediates/cvFoldCenterScaleDict.p"
# if os.path.exists(path):
#     cvFoldCenterScaleDict = pkl.load(open(path, 'rb'))
# else:
#     pkl.dump(cvFoldCenterScaleDict, open(path, 'wb'))
# -

overwrite_saved_files = False



pio.renderers.default = [
    'iframe',
#     'notebook',
#     'colab',
#     'jupyterlab'
][0]
pio.templates.default = "plotly_white"

"../data/atlas/models/3_finalize_model_syr__rep_G/"
"../data/atlas/models/3_finalize_model_syr__rep_S/"
"../data/atlas/models/3_finalize_model_syr__rep_W/"
"../data/atlas/models/3_finalize_model_syr__rep_full/"


# # Compare Goodness of Fit Across and Within Models

# ## Preparation

# +
# classes to hold model results
class ml_results:
    """
    # .
    # |- All
    # |  |- knn
    # |  |- rnr
    # |  |- svrl
    # |  |- rf
    # |
    # |- G
    # |- S
    # |- WOneHot
    """
    def __init__(self, project_folder):
        self.project_folder = project_folder
        self.results = {}
        
    def _re_filter_list(self, in_list, regex ):
        return([e for e in in_list if re.findall(regex, e)])        
        
    def retrieve_results(self):
        dirs_in_proj = os.listdir(self.project_folder)
        res_dirs =  self._re_filter_list(in_list = dirs_in_proj, regex = 'hps_res') 
        
        for res_dir in res_dirs:
            files_in_res_dir = os.listdir(self.project_folder+res_dir)
            res_files = self._re_filter_list(
                            in_list = files_in_res_dir, 
                            regex = '^Best_hps\w*\.json$')            
            for res_file in res_files:
                with open(self.project_folder+'/'+res_dir+'/'+res_file, 'r') as f:
                    dat = json.load(f)        
        
                # Make keys to use
                results_key_lvl_0 = res_dir.replace('hps_res_', '')
                results_key_lvl_1 = res_file.replace('Best_hps_', '').split('_')[0]
                
                results_key_lvl_2 = res_file.replace('Best_hps_', '').replace('.json', '').split('_')[-1]                    #        
                # Check to see if lvl 0 key exists. Update that key if it does, otherwise add it with lvl 1 key within it.
                if results_key_lvl_0 in self.results.keys():
                    if results_key_lvl_1 in self.results[results_key_lvl_0].keys():
                        self.results[results_key_lvl_0][results_key_lvl_1].update({results_key_lvl_2:dat})                        
                    else:
                        self.results[results_key_lvl_0].update({results_key_lvl_1:{results_key_lvl_2:dat}})
#                     self.results[results_key_lvl_0].update({results_key_lvl_1:dat})

                else:
                    self.results.update({results_key_lvl_0:{results_key_lvl_1:{results_key_lvl_2:dat}}})
    
class tf_results(ml_results):
    """
    # .
    # |- full
    # |  |- '0'
    # |  |   |- 'use_tf_model_rep'  \\
    # |  |   |- 'y_train'           |
    # |  |   |- 'yHat_train'        > output from evalModelLean.py
    # |  |   |- 'y_test'            |
    # |  |   |- 'yHat_test'         /
    # |  |   |- 'loss'          \\
    # |  |   |- 'mae'           |
    # |  |   |- 'val_loss'      > From history
    # |  |   |- 'val_mae'       /
    # |  |
    # |  |- '1'
    # |  |  ...
    # |  |- '9'
    # |
    # |- G
    # |- S
    # |- W
    """
    # Replaced to fit with tf model format
    def retrieve_results(self):
        dirs_in_proj = os.listdir(self.project_folder+'/eval')            
        # We need to process these separately or merge one into the other.
        res_predictions = [e for e in dirs_in_proj if re.findall('predictions' , e)]
        res_histories = [e for e in dirs_in_proj if re.findall('history' , e)]

        results_key_lvl_0 = self.project_folder.split('/')[-2].split('_')[-1]

        ## Processing predictions
        for res_file in res_predictions:                
            results_key_lvl_1 = re.findall('rep_\d+', res_file)[0].replace('rep_', '')

            with open(self.project_folder+'/eval'+'/'+res_file, 'r') as f:
                dat = json.load(f)

            #Check to see if lvl 0 key exists. Update that key if it does, otherwise add it with lvl 1 key within it.
            # This is overkill for tf results since we only have one level of `results_key_lvl_0`
            if results_key_lvl_0 in self.results.keys():
                self.results[results_key_lvl_0].update({results_key_lvl_1:dat})
            else:
                self.results.update({results_key_lvl_0:{results_key_lvl_1:dat}})

        ## Processing histories
        for res_file in res_histories:                
            results_key_lvl_1 = re.findall('rep_\d+', res_file)[0].replace('rep_', '')

            with open(self.project_folder+'/eval'+'/'+res_file, 'r') as f:
                dat = json.load(f)

            add_dict = {
                        'loss':dat['loss'],
                        'mae':dat['mae'],
                        'val_loss':dat['val_loss'],
                        'val_mae':dat['val_mae']
                    }
            if results_key_lvl_0 in self.results.keys():
                if results_key_lvl_1 in self.results[results_key_lvl_0]:
                    self.results[results_key_lvl_0][results_key_lvl_1].update(add_dict)
            else:
                self.results.update({results_key_lvl_0:{results_key_lvl_1:add_dict}}) 

# update lmlike to act more like bglr_results
class lmlike_results(ml_results):
    # Replaced to fit with the lm/lmer output from R
    def retrieve_results(self):
        dirs_in_proj = os.listdir(self.project_folder+'/results')  
        # find all the blups 
        dirs_in_proj = [e for e in dirs_in_proj if re.match("lm_.*", e)] 
        # limit to those with results
        res_names = [e for e in dirs_in_proj if 'fm_predictions.csv' in os.listdir(self.project_folder+'results/'+e)]

        for res_name in res_names:
            # These files associated with each result name.       
            # _formula.txt -----> Used for 'params'
            # _predictions.csv -> Used for 'y.*'
            # .rds -------------> Saved, but only used in R

            res_dict = {'use_lmlike_model': res_name}
            
            formula_path = self.project_folder+'/results/'+res_name+"/fm_formula.txt"
            predictions_path = self.project_folder+'/results/'+res_name+"/fm_predictions.csv"

            if os.path.exists(formula_path):
                with open(formula_path) as f:
                    dat = f.readlines()
                res_dict.update({'params': dat})
            if os.path.exists(predictions_path):
                dat = pd.read_csv(predictions_path)
                res_dict.update({
                       'y_train' : dat.loc[dat.type == "Train", 'y'   ].to_list(), 
                    'yHat_train' : dat.loc[dat.type == "Train", 'yHat'].to_list(), 
                        'y_test' : dat.loc[dat.type == "Test",  'y'   ].to_list(), 
                     'yHat_test' : dat.loc[dat.type == "Test",  'yHat'].to_list()
                })            
            
            # find out which group to use:
            has_G = has_S = has_W = has_multi = False
            if re.match(".*G.*", res_name):
                has_G = True
            if re.match(".*S.*", res_name):
                has_S = True
            if re.match(".*W.*", res_name):
                has_W = True
            if (has_G & has_S) | (has_G & has_W) | (has_S & has_W):
                has_multi = True
                
                
            # Because there are multiples, 
            if has_multi:
                results_key_lvl_0 = 'Multi'
            elif has_G:
                results_key_lvl_0 = 'G'
            elif has_S:
                results_key_lvl_0 = 'S'
            elif has_W:
                results_key_lvl_0 = 'W'
                
            results_key_lvl_1 = "lm"

            # Extract replicate number or impute (first may not have 0 in the name)
            if not re.findall("Rep|rep", res_name):
                results_key_lvl_2 = '0'
            else:
                results_key_lvl_2 = re.findall("\d+$", res_name)
            results_key_lvl_2 = 'rep'+results_key_lvl_2[0]
            
            if results_key_lvl_0 not in self.results.keys():
                self.results.update({results_key_lvl_0:{results_key_lvl_1:{results_key_lvl_2:res_dict}}})
            elif results_key_lvl_1 not in self.results[results_key_lvl_0].keys():
                self.results[results_key_lvl_0].update({results_key_lvl_1:{results_key_lvl_2:res_dict}})
            elif results_key_lvl_2 not in self.results[results_key_lvl_0][results_key_lvl_1].keys():
                self.results[results_key_lvl_0][results_key_lvl_1][results_key_lvl_2] = res_dict


# +
# based on the lmlike_results() function we're going to package up the blups using bglr_results()
# `BGLR` is an r library from cimmyt that can run blups and bayesian alphabet models as a Bayesian Generalized Linear Regression.
class bglr_results(ml_results):
    # Replaced to fit with the lm/lmer output from R
    def retrieve_results(self):
        dirs_in_proj = os.listdir(self.project_folder)
        # find all the blups 
        dirs_in_proj = [e for e in dirs_in_proj if re.match("BLUP_.*", e)] 
        # limit to those with results
        res_names = [e for e in dirs_in_proj if 'BlupYHats.csv' in os.listdir(self.project_folder+'/'+e)]

        for res_name in res_names:
            # Associated files: 
            # 'BLUP.R' ----------- Defines the blup to run
            # 'runBLUP' ---------- sbatch file that runs the blup
            # 'slurm-{\d+}.out' -- log file
            #
            # 'BlupYHats.csv' ---- Contains columns `0` (row index), `Y`, `YTrain` (test data is NaN), `YHat`
            # 'BlupRunTime.csv' -- Contains `start`, `end`, `duration` of the BGLR() run (excluding prep). 
            #
            # 'fm.rds' ----------- Fitted model in case we need to use it.
            # '*.dat' ------------ BGLR generated files

            res_dict = {'use_blup_model': res_name}
            predictions_path = self.project_folder+'/'+res_name+"/BlupYHats.csv"

            # parse dir name into formula of form
            """G+S+W+GxS+GxW+SxW"""
            formula_chunks = res_name.replace('BLUP_', '').split('_')
            
            formula_chunks = [
                '+'.join([e1 for e1 in e]) if re.findall('x', e) != ['x'] # find additive effects and put a '+' between each char
                else e # all the chunks with an 'x' are interactions so leave them be
                for e in formula_chunks]
            formula = '+'.join(formula_chunks)
            res_dict.update({'params': formula})

            # don't have to check if the path exists because we limited the `res_names` to those with BlupYHats.csv above
            dat = pd.read_csv(predictions_path)
            dat['type'] = ''
            mask = dat.YTrain.isna()
            dat.loc[mask, 'type'] = 'Test'
            dat.loc[~mask, 'type'] = 'Train'
            
            # backup non center/scaled results
            dat['Y_no_cs'] = dat['Y']
            dat['YHat_no_cs'] = dat['YHat']
            
            # we used the training data to center/scale to avoid information leakage
            train_mean = np.mean(list(dat.loc[dat.type == "Train", 'Y']))
            train_sd = np.std(list(dat.loc[dat.type == "Train", 'Y']))            

            dat['Y'] = ((dat['Y'] - train_mean)/train_sd)
            dat['YHat'] = ((dat['YHat'] - train_mean)/train_sd)
            
            res_dict.update({
                   'y_train' : dat.loc[dat.type == "Train", 'Y'   ].to_list(), 
                'yHat_train' : dat.loc[dat.type == "Train", 'YHat'].to_list(), 
                    'y_test' : dat.loc[dat.type == "Test",  'Y'   ].to_list(), 
                 'yHat_test' : dat.loc[dat.type == "Test",  'YHat'].to_list(),
                # as long as we have it, let's save the un-center/scaled estimates too 
                   'y_train_no_cs' : dat.loc[dat.type == "Train", 'Y_no_cs'   ].to_list(), 
                'yHat_train_no_cs' : dat.loc[dat.type == "Train", 'YHat_no_cs'].to_list(), 
                    'y_test_no_cs' : dat.loc[dat.type == "Test",  'Y_no_cs'   ].to_list(), 
                 'yHat_test_no_cs' : dat.loc[dat.type == "Test",  'YHat_no_cs'].to_list()
            })

            # Note! We assume there is only one replicate of each model since it's a linear model.
            # We'll tweak our groupings too. ML has All and TF has full, 
            #  here we add multi to capture models with 2+ data sources.
            # match fo find out which group to use:
            has_G = has_S = has_W = has_multi = False
            if re.match(".*G.*", res_name):
                has_G = True
            if re.match(".*S.*", res_name):
                has_S = True
            if re.match(".*W.*", res_name):
                has_W = True
            if (has_G & has_S) | (has_G & has_W) | (has_S & has_W):
                has_multi = True
                
                
            # Because there are multiples, 
            if has_multi:
                results_key_lvl_0 = 'Multi'
            elif has_G:
                results_key_lvl_0 = 'G'
            elif has_S:
                results_key_lvl_0 = 'S'
            elif has_W:
                results_key_lvl_0 = 'W'
                
            results_key_lvl_1 = "BLUP"

            # Extract replicate number or impute (first may not have 0 in the name)
            if not re.findall("Rep|rep", res_name):
                results_key_lvl_2 = '0'
            else:
                results_key_lvl_2 = re.findall("\d+$", res_name)
            results_key_lvl_2 = 'rep'+results_key_lvl_2[0]
            
#             print(res_name, results_key_lvl_1) debugging


            
#             self.results.update({results_key_lvl_0:{results_key_lvl_1:{results_key_lvl_2:res_dict}}})
#             if has_G:

            if results_key_lvl_0 not in self.results.keys():
                self.results.update({results_key_lvl_0:{results_key_lvl_1:{results_key_lvl_2:res_dict}}})
            elif results_key_lvl_1 not in self.results[results_key_lvl_0].keys():
                self.results[results_key_lvl_0].update({results_key_lvl_1:{results_key_lvl_2:res_dict}})
            elif results_key_lvl_2 not in self.results[results_key_lvl_0][results_key_lvl_1].keys():
                self.results[results_key_lvl_0][results_key_lvl_1][results_key_lvl_2] = res_dict
# -

# | . |rep |  train_rmse   | train_r2      | test_rmse    | test_r2       |
# |---|----|---------------|---------------|--------------|---------------|
# | G | 0  |  0            |  0            | 0            |  2.220446e-16 |
# | G | 1  |  0            |  1.110223e-16 | 0            |  0            |
# | G | 2  |  0            |  1.110223e-16 | 0            |  0            |
# | G | 4  |  0            |  1.110223e-16 | 0            |  0            |
# | G | 6  |  1.110223e-16 | -1.110223e-16 | 0            |  2.220446e-16 |
# | G | 9  | -1.110223e-16 |  1.110223e-16 | 0            |  0            |
# | S | 3  | -1.110223e-16 |  2.220446e-16 | 0            |  0            |
# | W | 0  |  1.110223e-16 | -1.110223e-16 | 0            |  0            |
# | W | 1  |  1.110223e-16 | -1.110223e-16 | 0            |  0            |
# | W | 2  |            0  |  1.110223e-16 | 0            |  2.220446e-16 |
# | W | 5  | -1.110223e-16 |  1.110223e-16 | 0            |  0            |
# | SO| 0  |  0            |  0            | 2.220446e-16 | -2.220446e-16 |
# | SO| 5  |  5.551115e-17 |  0            | 0            |  0            |
# | CO| 2  |  3.552714e-15 | -5.684342e-14 | 0            | -1.110223e-16 |
# | CO| 7  |  1.110223e-16 |  0            | 0            |  0            |
# | CO| 8  |  0            |  0            |-1.110223e-16 |  1.110223e-16 |
# | CO| 9  |  1.110223e-16 |  0            | 0            |  0            |

bglr_res = bglr_results(project_folder="../ext_data/kick_et_al_2023/models/3_finalize_model_BGLR_BLUPS")
bglr_res.retrieve_results()
bglr_res = bglr_res.results

ml_res = ml_results(project_folder = "../ext_data/kick_et_al_2023/models/3_finalize_classic_ml_10x/")
ml_res.retrieve_results()
ml_res = ml_res.results

# +
tf_res = {}

for proj in ["3_finalize_model_syr__rep_G", 
             "3_finalize_model_syr__rep_S",
             "3_finalize_model_syr__rep_W",
             "3_finalize_model_syr__rep_full",
             "3_finalize_model_syr__rep_cat"
            ]:
    temp = tf_results(project_folder = "../ext_data/kick_et_al_2023/models/"+proj+"/")
    temp.retrieve_results()
    temp = temp.results

    if list(tf_res.keys()) == []:
        tf_res = temp
    else:
        tf_res.update(temp)
# -

lmlike_res = lmlike_results(project_folder="../ext_data/kick_et_al_2023/models/3_finalize_lm/")
lmlike_res.retrieve_results()
lmlike_res = lmlike_res.results

# +
# temp = ml_res['All']['knn']

## Create Empty Summary DataFrame ==============================================
# Make a df with all the keys so we can populate it with the statistics we want.
# make a df with all the keys for each experiment we ran. 
keys_to_get_ys = []
for key0 in ml_res.keys():
    for key1 in ml_res[key0].keys():
        for key2 in ml_res[key0][key1].keys():
            keys_to_get_ys.append(['ml_res', key0, key1, key2])
        
for key0 in tf_res.keys():
    for key1 in tf_res[key0].keys():
        keys_to_get_ys.append(['tf_res', key0, key1])

        
for key0 in lmlike_res.keys():
    for key1 in lmlike_res[key0].keys():
        for key2 in lmlike_res[key0][key1].keys():
            keys_to_get_ys.append(['lmlike_res', key0, key1, key2])

        
for key0 in bglr_res.keys():
    for key1 in bglr_res[key0].keys():
        for key2 in bglr_res[key0][key1].keys():
            keys_to_get_ys.append(['bglr_res', key0, key1, key2])
            
summary_df = pd.DataFrame(keys_to_get_ys, columns = ['source', 'key0', 'key1', 'key2'])


summary_df[['train_r','train_r2','train_mse','train_rmse', 
            'test_r', 'test_r2', 'test_mse', 'test_rmse']] = np.nan

## Populate Summary DataFrame with Error Statistics ============================

def replace_na_w_0(in_df = temp,
                   col_vars = ['y_train', 'yHat_train']):
    for col_var in col_vars:
        na_mask = in_df[col_var].isna()
        n_missings = sum(na_mask)
        if n_missings > 0:
            print('Replacing '+str(n_missings)+' with 0.')
            in_df.loc[na_mask, col_var] = 0
    return(in_df)

def calc_performances(observed, predicted, outtype = 'list'):
    r = stats.pearsonr(observed, predicted)[0] # index 1 is the p value
#     r2 = r**2
    r2 = r2_score(observed, predicted)
    MSE = mean_squared_error(observed, predicted)
    RMSE = math.sqrt(MSE)
    
    #additional metics 
    # relative RMSE
    relRMSE = RMSE / np.nanmean(observed)
    # normalized RMSE
    normRMSE = RMSE / (np.nanmax(observed) - np.nanmin(observed))
    
    
    if outtype == 'list':
        return([r, r2, MSE, RMSE,
               relRMSE, normRMSE
               ])
    else:
        return({'r':r,
               'r2':r2,
              'mse':MSE,
             'rmse':RMSE,
          'relRMSE':relRMSE,
         'normRMSE':normRMSE
               })

for i in summary_df.index:
    source = summary_df.loc[i, 'source']
    key0 = summary_df.loc[i, 'key0']
    key1 = summary_df.loc[i, 'key1']
    key2 = summary_df.loc[i, 'key2']
    
    
    if source == 'ml_res':
        temp = pd.DataFrame()
        temp['y_train'] = ml_res[key0][key1][key2]['y_train']
        temp['yHat_train'] = ml_res[key0][key1][key2]['yHat_train']
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_train', 'yHat_train'])
        summary_df.loc[i, ['train_r','train_r2','train_mse','train_rmse',
                          'train_relRMSE', 'train_normRMSE']] = calc_performances(observed = temp['y_train'], predicted = temp['yHat_train'] )

        temp = pd.DataFrame()
        temp['y_test'] = ml_res[key0][key1][key2]['y_test']
        temp['yHat_test'] = ml_res[key0][key1][key2]['yHat_test']
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_test', 'yHat_test'])
        summary_df.loc[i, ['test_r', 'test_r2', 'test_mse', 'test_rmse',
                          'test_relRMSE', 'test_normRMSE']] = calc_performances(observed = temp['y_test'], predicted = temp['yHat_test'])


    if source == 'tf_res':
        temp = pd.DataFrame()
        temp['y_train'] = tf_res[key0][key1]['y_train']
        temp['yHat_train'] = [e[0] for e in tf_res[key0][key1]['yHat_train']]
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_train', 'yHat_train'])
        summary_df.loc[i, ['train_r','train_r2','train_mse','train_rmse',
                          'train_relRMSE', 'train_normRMSE']] = calc_performances(observed = temp['y_train'], predicted = temp['yHat_train'] )

        temp = pd.DataFrame()
        temp['y_test'] = tf_res[key0][key1]['y_test']
        temp['yHat_test'] = [e[0] for e in tf_res[key0][key1]['yHat_test']]
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_test', 'yHat_test'])
        summary_df.loc[i, ['test_r', 'test_r2', 'test_mse', 'test_rmse',
                          'test_relRMSE', 'test_normRMSE']] = calc_performances(observed = temp['y_test'], predicted = temp['yHat_test'])
        
        
    if source == 'lmlike_res':
        temp = pd.DataFrame()
        temp['y_train'] = lmlike_res[key0][key1][key2]['y_train']
        temp['yHat_train'] = lmlike_res[key0][key1][key2]['yHat_train']
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_train', 'yHat_train'])
        summary_df.loc[i, ['train_r','train_r2','train_mse','train_rmse',
                          'train_relRMSE', 'train_normRMSE']] = calc_performances(observed = temp['y_train'], predicted = temp['yHat_train'] )

        temp = pd.DataFrame()
        temp['y_test'] = lmlike_res[key0][key1][key2]['y_test']
        temp['yHat_test'] = lmlike_res[key0][key1][key2]['yHat_test']
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_test', 'yHat_test'])
        summary_df.loc[i, ['test_r', 'test_r2', 'test_mse', 'test_rmse',
                          'test_relRMSE', 'test_normRMSE']] = calc_performances(observed = temp['y_test'], predicted = temp['yHat_test'])

        
    if source == 'bglr_res':
        #print(key0, key1, key2)
        
        
        temp = pd.DataFrame() 
        temp['y_train'] = bglr_res[key0][key1][key2]['y_train']
        temp['yHat_train'] = bglr_res[key0][key1][key2]['yHat_train']
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_train', 'yHat_train'])
        summary_df.loc[i, ['train_r','train_r2','train_mse','train_rmse',
                          'train_relRMSE', 'train_normRMSE']] = calc_performances(observed = temp['y_train'], predicted = temp['yHat_train'] )

        temp = pd.DataFrame()
        temp['y_test'] = bglr_res[key0][key1][key2]['y_test']
        temp['yHat_test'] = bglr_res[key0][key1][key2]['yHat_test']
        temp = replace_na_w_0(in_df = temp, col_vars = ['y_test', 'yHat_test'])
        summary_df.loc[i, ['test_r', 'test_r2', 'test_mse', 'test_rmse',
                          'test_relRMSE', 'test_normRMSE']] = calc_performances(observed = temp['y_test'], predicted = temp['yHat_test'])

# +
## Add in a Naieve estimate -- Guessing the Training Mean Every Time ===========
ys = tf_res['G']['0']['y_train']
yNaive = [0 for e in range(len(ys))]

train_MSE = mean_squared_error(ys, yNaive)
train_RMSE = math.sqrt(train_MSE)
train_r2 = r2_score(ys, yNaive)
train_relRMSE = train_RMSE / np.nanmean(ys)
train_normRMSE = train_RMSE / (np.nanmax(ys) - np.nanmin(ys))

ys = tf_res['G']['0']['y_test']
yNaive = [0 for e in range(len(ys))]

test_MSE = mean_squared_error(ys, yNaive)
test_RMSE = math.sqrt(test_MSE)
test_r2 = r2_score(ys, yNaive)
test_relRMSE = test_RMSE / np.nanmean(ys)
test_normRMSE = test_RMSE / (np.nanmax(ys) - np.nanmin(ys))


# pd.concat(summary_df , ['Mean', 'Mean', 0, np.nan, np.nan, train_MSE, train_RMSE, np.nan, np.nan, test_MSE, test_RMSE])
# source	key0	key1	train_r	train_r2	train_mse	train_rmse	test_r	test_r2	test_mse	test_rmse


temp = pd.DataFrame(
    zip(
    ['Mean', 'G', 'Mean',   np.nan, train_r2, train_MSE, train_RMSE, train_relRMSE, train_normRMSE,
                            np.nan,  test_r2,  test_MSE,  test_RMSE,  test_relRMSE,  test_normRMSE],
    ['Mean', 'S', 'Mean',   np.nan, train_r2, train_MSE, train_RMSE, train_relRMSE, train_normRMSE,
                            np.nan,  test_r2,  test_MSE,  test_RMSE,  test_relRMSE,  test_normRMSE],
    ['Mean', 'W', 'Mean',   np.nan, train_r2, train_MSE, train_RMSE, train_relRMSE, train_normRMSE,
                            np.nan,  test_r2,  test_MSE,  test_RMSE,  test_relRMSE,  test_normRMSE],
    ['Mean', 'All', 'Mean', np.nan, train_r2, train_MSE, train_RMSE, train_relRMSE, train_normRMSE,
                            np.nan,  test_r2,  test_MSE,  test_RMSE,  test_relRMSE,  test_normRMSE],
    ['Mean', 'cat', 'Mean', np.nan, train_r2, train_MSE, train_RMSE, train_relRMSE, train_normRMSE,
                            np.nan,  test_r2,  test_MSE,  test_RMSE,  test_relRMSE,  test_normRMSE])

).T



temp = temp.rename(columns ={
    0:'source',
    1:'key0',
    2:'key1',
    3:'train_r',
    4:'train_r2',
    5:'train_mse',
    6:'train_rmse',
    7:'train_relRMSE',
    8:'train_normRMSE',
    9: 'test_r',
    10:'test_r2',
    11:'test_mse',
    12:'test_rmse',
    13:'test_relRMSE',
    14:'test_normRMSE'
})

summary_df = pd.concat([summary_df, temp])    

# +
summary_df['model_class'] = ''
summary_df['data_source'] = ''
summary_df['model'] = ''
summary_df['replicate'] = ''

## Model Groupings =============================================================
summary_df.loc[summary_df.source == 'ml_res', 'model_class'] = 'ML' 
summary_df.loc[summary_df.source == 'tf_res', 'model_class'] = 'DNN' 
summary_df.loc[summary_df.source == 'lmlike_res', 'model_class'] = 'LM' 
summary_df.loc[summary_df.source == 'bglr_res', 'model_class'] = 'BLUP' 
summary_df.loc[summary_df.source == 'Mean', 'model_class'] = 'LM' 

## Model Names =================================================================
summary_df['model'] = summary_df['key1']

mask = ((summary_df.model_class == 'DNN') & (summary_df.key0 == 'G'))
summary_df.loc[mask, 'model'] = 'DNN-Con.' 
mask = ((summary_df.model_class == 'DNN') & (summary_df.key0 == 'W'))
summary_df.loc[mask, 'model'] = 'DNN-Con.' 
mask = ((summary_df.model_class == 'DNN') & (summary_df.key0 == 'S'))
summary_df.loc[mask, 'model'] = 'DNN-Con.' 
mask = ((summary_df.model_class == 'DNN') & (summary_df.key0 == 'cat'))
summary_df.loc[mask, 'model'] = 'DNN-Con.' 
mask = ((summary_df.model_class == 'DNN') & (summary_df.key0 == 'full'))
summary_df.loc[mask, 'model'] = 'DNN-Sim.' 

## Data Groupings ==============================================================
# # copy and then overwrite the values we want to change
kv_pairs = [
    ['WOneHot', 'W'],
    ['All',     'Multi'],
    ['full',    'Multi'],
    ['cat',     'Multi']
]
summary_df['data_source'] = summary_df['key0']
for i in range(len(kv_pairs)):
    ith_key = kv_pairs[i][0]
    ith_value = kv_pairs[i][1]   
    mask = (summary_df.key0 == ith_key)
    summary_df.loc[mask, 'data_source'] = ith_value 


## Replicate Numbers ===========================================================
mask = summary_df.model_class == 'ML'
summary_df.loc[mask, 'replicate']  = summary_df.loc[mask, 'key2'].str.replace('rep', '')

mask = summary_df.model_class == 'DNN'
summary_df.loc[mask, 'replicate']  = summary_df.loc[mask, 'key1']#.str.replace('rep', '')

mask = summary_df.model_class == 'LM'
summary_df.loc[mask, 'replicate']  = summary_df.loc[mask, 'key2'].str.replace('rep', '')

# empty values (from naive lm/mean) set to replicate 0
summary_df.loc[summary_df.replicate.isna(), 'replicate'] = 0 

# +
# parse lmlikes
summary_df['annotation'] = ''
# mask = summary_df.model_class == 'LM'


# Note -- ubuntu seesm to have no issue with "*" but windows is representing it as "\uf02a"
# to get around this I have duplicated some of the lines below. 
lmlike_additional_info = pd.DataFrame([
# Name     blue/p  annotation                   # Single Data Sources
['Mean',       'Fixed', 'Intercept Only'],      # |--Simple Fixed Models
# ['g8f',        'Fixed', '31% Varience'],        # |  .
# ['g50f',       'Fixed', '50% Varience'],        # |  .
# ['s*f',        'Fixed', 'All Factors'],         # |  .
# ['s\uf02af',        'Fixed', 'All Factors'],         # |  .     --------------------- dupe for encoding
# ['w5f',        'Fixed', 'Top 5 Factors'],       # |  . 
# ['w*f',        'Fixed', 'All Factors'],         # |  .
# ['w\uf02af',        'Fixed', 'All Factors'],         # |  .     --------------------- dupe for encoding
#                                                 # |--Simple Rand. Models
# ['g8r',        'Rand.', '31% Varience'],        #    .
# ['s*r',        'Rand.', 'All Factors'],         #    .
# ['s\uf02ar',        'Rand.', 'All Factors'],         #    .     --------------------- dupe for encoding
# ['w5r',        'Rand.', 'Top 5 Factors'],       #    .

#                                                 # Multiple Sources of Data
# ['g8fw5fs*f',  'Fixed', 'G+S+W'],               # |--Fixed
# ['g8fw5fs\uf02af',  'Fixed', 'G+S+W'],               # |--Fixed --------------------- dupe for encoding
# ['g8fw5f',     'Fixed', 'G+W'],                 # |  .
# ['g8fw5f_gxw', 'Fixed', 'G+W+G:W'],             # |  . - Interaction
#                                                 # |--Rand.
# ['g8rw5r',     'Rand.', '(1|G)+(1|W)'],         #    .
# ['g8rw5r_gxw', 'Rand.', '(1|G)+(1|W)+(1|G:W)'], #    . - Interaction
], columns = ['key1', 'effect_type', 'annotation'])


for i in range(lmlike_additional_info.shape[0]):
    mask = (summary_df.key1 == lmlike_additional_info.loc[i, 'key1'])
    summary_df.loc[mask, 'model'] = lmlike_additional_info.loc[i, 'effect_type']
    summary_df.loc[mask, 'annotation'] = lmlike_additional_info.loc[i, 'annotation']
# -

# ### For final figs

# +
# tmp = summary_df
# tmp.head()

# +
# tmp2 = tmp

# tmp2.loc[tmp2.model == 'rnr', 'model'] =  "Radius NR"
# tmp2.loc[tmp2.model == 'knn', 'model'] =  "KNN"
# tmp2.loc[tmp2.model == 'rf', 'model'] =  "Rand.Forest"
# tmp2.loc[tmp2.model == 'svrl', 'model'] =  "SVR (linear)"

# tmp2.loc[tmp2.model == 'lm', 'model'] =  "LM"

# tmp2.loc[tmp2.annotation == 'Intercept Only', 'model'] =  "Training Mean"

# +
# tmp2.to_csv("../output/r_performance_across_models.csv")

# +
## Edit the keys so there are consistent groupings =============================

summary_df.loc[summary_df['key0'] == 'full', 'key0'] = 'All'

# get replicate for tf
summary_df['rep'] = [int(e) if re.findall('\d$', e) else 0 for e in summary_df['key1'] ]
summary_df['key1'] = [e if not re.findall('\d', e) else 'dnn' for e in summary_df['key1'] ]
# then get replicate for ml
summary_df['extracted_rep_num'] = [int(e.replace('rep', '')) if re.findall('rep\d+', str(e)) else -1 for e in summary_df['key2'] ]
mask = (summary_df['source'] == 'ml_res')
summary_df.loc[mask, 'rep'] = summary_df.loc[mask, 'extracted_rep_num']
summary_df = summary_df.drop(columns = ['key2', 'extracted_rep_num'])

summary_df.loc[summary_df['key0'] == 'WOneHot', 'key0'] = 'W'
# -

summary_df.head()

# # Load data

# ## Set up data -- used in salinence, pulling observations

phenotype = pd.read_csv('../ext_data/kick_et_al_2023/data/processed/tensor_ref_phenotype.csv')
soil = pd.read_csv('../ext_data/kick_et_al_2023/data/processed/tensor_ref_soil.csv')
weather = pd.read_csv('../ext_data/kick_et_al_2023/data/processed/tensor_ref_weather.csv')

# +
# Specified Options -----------------------------------------------------------
## General ====
projectName = 'kt' # this is the name of the folder which will be made to hold the tuner's progress.
# hp = kt.HyperParameters()
## Tuning ====
splitIndex = 0
maxTrials  = 40  
numEpochs  = 1000 
# kfolds     = 10 
cvSeed     = 646843
max_pseudorep = 10 # <-- New. How many times should the model be run to account for random initialization? 
## Model ====
# set needed data
needGpca = True
needS    = True
needW    = True

# I don't foresee a benefit to constraining all HP optimizations to follow the same path of folds.
# To make the traning more repeatable while still allowing for a pseudorandom walk over the folds 
# we define the rng up front and will write it out in case it is useful in the future. 
random_cv_starting_seed = int(round(np.random.uniform()*1000000))
k_cv_rng = np.random.default_rng(random_cv_starting_seed)
with open('random_cv_starting_seed.json', 'w') as f:
    json.dump({'random_cv_starting_seed':random_cv_starting_seed}, f)

# +
# Index prep -------------------------------------------------------------------
path     = '../ext_data/kick_et_al_2023/' 
pathData = path+'data/processed/'

indexDictList = json.load(open(pathData+'indexDictList_syr.txt')) 

trainIndex    = indexDictList[splitIndex]['Train']
trainGroups   = indexDictList[splitIndex]['TrainGroups']
testGroups    = indexDictList[splitIndex]['TestGroups'] 
testIndex     = indexDictList[splitIndex]['Test']

# +

# Data prep -------------------------------------------------------------------
# Y ===========================================================================
Y = np.load(pathData+'Y.npy')
# G ===========================================================================
if needGpca:
    G = np.load(pathData+'G_PCA_1.npy') 
# S ===========================================================================
if needS:
    S = np.load(pathData+'S.npy')
# W =========================================================================== 
if needW:
    W = np.load(pathData+'W.npy')
data_list = [G, S, W]

"""
Testing:
1. Are there NaN in the genomic data that should not be so?
"""
all_idx = trainIndex+testIndex
print("Total indices ---", len(all_idx))
print("+ctrl NaNs in G -", np.isnan(G).sum())
print("   In selection -", np.isnan(G[all_idx]).sum())
print("")
assert 0 == np.isnan(G[all_idx]).sum(), 'Halted if there were nans found.'


nTestGroups = len(set(testGroups))
x = data_list
y = Y

# +
i = 0
cvFoldCenterScaleDict = {}

# Calculate values for center/scaling each fold
ith_tr_idx = trainIndex

YStd  = y[ith_tr_idx].std()
YMean = y[ith_tr_idx].mean()

GStd  = np.apply_along_axis(np.nanstd,  0, G[ith_tr_idx])
GMean = np.apply_along_axis(np.nanmean, 0, G[ith_tr_idx])

SStd  = np.apply_along_axis(np.std,  0, S[ith_tr_idx])
SMean = np.apply_along_axis(np.mean, 0, S[ith_tr_idx])

# we want to center and scale _but_ some time x feature combinations have an sd of 0. (e.g. nitrogen application)
# to get around this we'll make it so that if sd == 0, sd = 1.
WStd  = np.apply_along_axis(np.std,  0, W[ith_tr_idx])
WStd[WStd == 0] = 1
WMean = np.apply_along_axis(np.mean, 0, W[ith_tr_idx])

cvFoldCenterScaleDict.update({i:{'YStd' : YStd,
                                 'YMean': YMean,
                                 'GStd' : GStd,
                                 'GMean': GMean,
                                 'SStd' : SStd,
                                 'SMean': SMean,
                                 'WStd' : WStd,
                                 'WMean': WMean}})
# -

x_all = x

# +
# Set up data: ----------------------------------------------------------------
i = list(cvFoldCenterScaleDict.keys())[0]

# Center and scale testing and training data: ---------------------------------
## Training data: =============================================================
x_all[0] = ((x_all[0] - cvFoldCenterScaleDict[i]['GMean']) / cvFoldCenterScaleDict[i]['GStd'])
x_all[0] =   x_all[0].astype('float32') 

x_all[1] = ((x_all[1] - cvFoldCenterScaleDict[i]['SMean']) / cvFoldCenterScaleDict[i]['SStd'])
x_all[1] =   x_all[1].astype('float32')

x_all[2] = ((x_all[2] - cvFoldCenterScaleDict[i]['WMean']) / cvFoldCenterScaleDict[i]['WStd'])
x_all[2] =   x_all[2].astype('float32')

# +
# Training loop ----

foldwise_data_list = []

"""
This is now a list of tensors instead of one tensor (to prep for the big model)
We'll do this a little different by iterating over the inputs. Then we can be agnostic as to 
what inputs are going in -- just going off position.

This will need to happen for the k fold evaluation below
"""
x_train = [x_tensor[trainIndex] for x_tensor in x] 
x_test  = [x_tensor[testIndex]  for x_tensor in x] 
y_train = y[trainIndex] 
y_test  = y[testIndex]

# Center and scale testing and training data: ---------------------------------
## Training data: =============================================================
y_train = (y_train - cvFoldCenterScaleDict[i]['YMean']) / cvFoldCenterScaleDict[i]['YStd']

x_train[0] = ((x_train[0] - cvFoldCenterScaleDict[i]['GMean']) / cvFoldCenterScaleDict[i]['GStd'])
x_train[0] =   x_train[0].astype('float32')

x_train[1] = ((x_train[1] - cvFoldCenterScaleDict[i]['SMean']) / cvFoldCenterScaleDict[i]['SStd'])
x_train[1] =   x_train[1].astype('float32')

x_train[2] = ((x_train[2] - cvFoldCenterScaleDict[i]['WMean']) / cvFoldCenterScaleDict[i]['WStd'])
x_train[2] =   x_train[2].astype('float32')

## Test | Validation data: ====================================================
y_test = (y_test - cvFoldCenterScaleDict[i]['YMean']) / cvFoldCenterScaleDict[i]['YStd']

x_test[0] = ((x_test[0] - cvFoldCenterScaleDict[i]['GMean']) / cvFoldCenterScaleDict[i]['GStd'])
x_test[0] =   x_test[0].astype('float32')

x_test[1] = ((x_test[1] - cvFoldCenterScaleDict[i]['SMean']) / cvFoldCenterScaleDict[i]['SStd'])
x_test[1] =   x_test[1].astype('float32')

x_test[2] = ((x_test[2] - cvFoldCenterScaleDict[i]['WMean']) / cvFoldCenterScaleDict[i]['WStd'])
x_test[2] =   x_test[2].astype('float32')

foldwise_data_list.append({'train':{'y':y_train,
                                    'x':x_train},
                            'test':{'y':y_test,
                                    'x':x_test} 
                      })

# +
x_train = foldwise_data_list[0]['train']['x']
y_train = foldwise_data_list[0]['train']['y']
x_test  = foldwise_data_list[0]['test' ]['x']
y_test  = foldwise_data_list[0]['test' ]['y']

# ### Load Models, Rebuild if needed.
# -

y_test.shape


# # Visualization

# ## How does aggregation change the predictive accuracy?

# +
def get_mod_multi_yyhat(model_type = 'lm', rep_n = '0', split = 'train'):
    rep_n = str(rep_n)
    rep_n_report = rep_n
    
    if model_type == 'lm':
        temp_res = lmlike_res['Multi'][model_type]
        rep_n = 'rep'+rep_n 
    elif model_type == 'bglr':
        temp_res = bglr_res['Multi']['BLUP']
        rep_n = 'rep'+rep_n 
    elif True in [model_type == e for e in ['knn', 'rf', 'rnr', 'svrl']]:
        temp_res = ml_res['All'][model_type]
        rep_n = 'rep'+rep_n 
    elif True in [model_type == e for e in ['full', 'cat']]:
        temp_res = tf_res[model_type]
    else:
        print("temp_res must be one of 'lm', 'bglr', 'knn', 'rf', 'rnr', 'svrl', 'full', 'cat'")
        
    temp_res = temp_res[rep_n]
            
    if split not in ['train', 'test']:
        print("split must be 'train' or 'test'")
    else:
        temp = pd.DataFrame()
        temp['y'] = temp_res['y_'+split]
        if True in [model_type == e for e in ['full', 'cat']]:
            temp['yHat'] = [e[0] for e in temp_res['yHat_'+split]]
        else:
            temp['yHat'] = temp_res['yHat_'+split]
        temp['type'] = model_type
        temp['rep']  = rep_n_report
        temp['split']= split
        temp = temp.loc[:, ['type', 'rep', 'split', 'y', 'yHat']]
    return(temp)
    


# +
# get_mod_multi_yyhat(model_type = 'lm', rep_n = '0', split = 'train')

# +
# be able to generate a list of lists with all the models and reps for a split
# [['lm', '0', 'train'],
#  ...
#  ['cat', '9', 'train']]

def mk_mod_comb_list(
    mods_list = ['lm', 'bglr', 'knn', 'rf', 'rnr', 'svrl', 'full', 'cat'],
    reps_list = [str(irep) for irep in range(10)],
    split = 'train'):
    out = list()
    for imod in mods_list:
        for irep in reps_list:
            out = out+[[imod, irep, split]]
    return(out)


mods_list = ['lm', 'bglr', 'knn', 'rf', 'rnr', 'svrl', 'full', 'cat']
reps_list = [str(irep) for irep in range(10)]

mod_comb_list_train = mk_mod_comb_list(
    mods_list = mods_list,
    reps_list = reps_list,
    split = 'train')

mod_comb_list_test = mk_mod_comb_list(
    mods_list = mods_list,
    reps_list = reps_list,
    split = 'test')

# +
# Create a np array with all of the models' predictions
mod_train_yhats = pd.concat([
    get_mod_multi_yyhat(
        model_type = e[0], 
        rep_n = e[1], 
        split = e[2]).loc[:, ['yHat']
                    ].rename(columns = {'yHat': e[0]+'_'+e[1]})
     for e in mod_comb_list_train], axis = 1)

mod_test_yhats = pd.concat([
    get_mod_multi_yyhat(
        model_type = e[0], 
        rep_n = e[1], 
        split = e[2]).loc[:, ['yHat']
                    ].rename(columns = {'yHat': e[0]+'_'+e[1]})
     for e in mod_comb_list_test], axis = 1)

# +
mod_train_y = get_mod_multi_yyhat(
    model_type = 'lm',
    rep_n = '0',
    split = 'train').loc[:, ['y']]


mod_test_y = get_mod_multi_yyhat(
    model_type = 'lm',
    rep_n = '0',
    split = 'test').loc[:, ['y']]

# +
# Fill in missings with 0

for e in list(mod_train_yhats):
    mask = mod_train_yhats[e].isna()
    mod_train_yhats.loc[mask, e] = 0

for e in list(mod_test_yhats):
    mask = mod_test_yhats[e].isna()
    mod_test_yhats.loc[mask, e] = 0


# -

def get_rmse(observed, predicted):
    MSE = mean_squared_error(observed, predicted)
    RMSE = math.sqrt(MSE)
    return(RMSE)





# ## Setup train/test predictions and covariates

# +
mod_train = pd.concat([
    phenotype.loc[trainIndex, ['Pedigree', 'F', 'M', 'ExperimentCode', 'Year']
            ].reset_index().drop(columns = 'index'),
    mod_train_y,
    mod_train_yhats
], axis = 1)

mod_test = pd.concat([
    phenotype.loc[testIndex, ['Pedigree', 'F', 'M', 'ExperimentCode', 'Year']
            ].reset_index().drop(columns = 'index'),
    mod_test_y,
    mod_test_yhats
], axis = 1)


# +
# get standard deviation of a set of models
# How much do models vary over replicates?
def get_mod_rep_std(mod_list = ['lm_0', 'lm_1'],
                    df = mod_train):
    return(df.loc[:, mod_list].std(axis = 1).mean())

def get_mod_std(mod_list = ['lm_0', 'lm_1'],
                    df = mod_train):
    return(list(df.loc[:, mod_list].std(axis = 0)))
    

# get_mod_rep_std(mod_list = ['lm_0', 'bglr_1'],
#                 df = mod_train)

# +
# How much variability is there within each model type (in train set?)
mod_types = ['lm', 'bglr', 'knn', 'rf', 'rnr', 'svrl', 'full', 'cat']


replicate_variability_summary = pd.DataFrame([
    [e, get_mod_rep_std(
        mod_list = [e1 for e1 in list(mod_train) if re.findall(e+'_.+', e1)], 
        df = mod_train)
    ] for e in mod_types], columns = ['mod', 'std'])

replicate_variability_summary


# +
# Based on the above, I'm limiting the number of replicates of the models that would be expected to converge
# and have std on the order of e-17 
# - lm
# - knn
# - rnr

# drop all but the 0th rep for these models
# FIXME do we want to downsample or nah? --------------------------------------------------------------------------
# for lst in [[e+'_'+str(9-i) for i in range(9)] for e in ['lm', 'knn', 'rnr']]:
#     mod_train = mod_train.drop(columns=lst)
#     mod_test = mod_test.drop(columns=lst)

# +
def get_mod_cols(
    df = mod_train,
    mod_type = 'lm'):
    e = mod_type
    col_list = [e1 for e1 in list(df) if re.findall(e+'_.+', e1)]
    return(col_list)

def get_all_mod_cols(
    df = mod_train,
    mod_types = ['bglr', 'cat']):
    col_list = [get_mod_cols(
        df = df,
        mod_type = e) for e in mod_types]
    # flatten list
    out = []
    for i in range(len(col_list)):
        out += col_list[i]
    return(out)

# +
# calculate several weighting options based on TESTING set


all_mod_cols = get_all_mod_cols(df = mod_test, mod_types = mod_types)

# uniform weighting ------------------------------------------------------------
uniform_weights_test = [1/len(all_mod_cols) for i in range(len(all_mod_cols))]

# uniform weighting wrt model --------------------------------------------------
# take n model types and convert to weight per replicate
uniform_by_type_weights_test = [1/len(mod_types) for i in range(len(mod_types))]
n_rep_by_type = [len(get_mod_cols(df = mod_test, mod_type = e)) for e in mod_types]
# apply to column names
uniform_by_type_weights_test = [uniform_by_type_weights_test[i]/n_rep_by_type[i] for i in range(len(uniform_by_type_weights_test))] 
# convert to model types
temp_types = [mod_col.split('_')[0] for mod_col in all_mod_cols]
#convert to index
temp_idxs = [[i for i in range(len(mod_types)) if mod_types[i] == mod_col][0] for mod_col in temp_types]
# convert to weigths
uniform_by_type_weights_test = [uniform_by_type_weights_test[i] for i in temp_idxs]


# weigting wrt inv rmse ------------------------------------------------------------
# take inverse of rmse and convert to sum to one
temp = [1/get_rmse(mod_test.y, mod_test[e]) for e in all_mod_cols]
inv_rmse_weights_test = [e/np.sum(temp) for e in temp]

# weigting wrt inv std -------------------------------------------------------------
temp = get_mod_std(mod_list = all_mod_cols, df = mod_test)
temp = [1/e for e in temp]
inv_std_weights_test = [e/np.sum(temp) for e in temp]


# weigting wrt inv var -------------------------------------------------------------
temp = get_mod_std(mod_list = all_mod_cols, df = mod_test)
temp = [e**2 for e in temp]
temp = [1/e for e in temp]
inv_var_weights_test = [e/np.sum(temp) for e in temp]

# +
# calculate several weighting options based on TRAINING set
all_mod_cols = get_all_mod_cols(df = mod_train, mod_types = mod_types)

# uniform weighting ------------------------------------------------------------
uniform_weights = [1/len(all_mod_cols) for i in range(len(all_mod_cols))]

# uniform weighting wrt model --------------------------------------------------
# take n model types and convert to weight per replicate
uniform_by_type_weights = [1/len(mod_types) for i in range(len(mod_types))]
n_rep_by_type = [len(get_mod_cols(df = mod_train, mod_type = e)) for e in mod_types]
# apply to column names
uniform_by_type_weights = [uniform_by_type_weights[i]/n_rep_by_type[i] for i in range(len(uniform_by_type_weights))] 
# convert to model types
temp_types = [mod_col.split('_')[0] for mod_col in all_mod_cols]
#convert to index
temp_idxs = [[i for i in range(len(mod_types)) if mod_types[i] == mod_col][0] for mod_col in temp_types]
# convert to weigths
uniform_by_type_weights = [uniform_by_type_weights[i] for i in temp_idxs]


# weigting wrt inv rmse ------------------------------------------------------------
# take inverse of rmse and convert to sum to one
temp = [1/get_rmse(mod_train.y, mod_train[e]) for e in all_mod_cols]
inv_rmse_weights = [e/np.sum(temp) for e in temp]

# weigting wrt inv std -------------------------------------------------------------
temp = get_mod_std(mod_list = all_mod_cols, df = mod_train)
temp = [1/e for e in temp]
inv_std_weights = [e/np.sum(temp) for e in temp]


# weigting wrt inv var -------------------------------------------------------------
temp = get_mod_std(mod_list = all_mod_cols, df = mod_train)
temp = [e**2 for e in temp]
temp = [1/e for e in temp]
inv_var_weights = [e/np.sum(temp) for e in temp]
# -



def weighted_sum_cols(mod_list = ['lm_0', 'cat_0'],
                      mod_weights = [0.5, 0.5],
                      df = mod_train):
    # ensure weights sum to 1
    mod_weights = mod_weights/np.sum(mod_weights) 
    temp = df.loc[:, mod_list]
    temp = (temp * mod_weights).sum(axis = 1)
    return(temp)


# +
def ftrain(weight_list):
    temp = weighted_sum_cols(
        mod_list = all_mod_cols,
        mod_weights = weight_list,
        df = mod_train)
    out = get_rmse(
        observed = mod_train['y'], 
        predicted = temp)
    return(out)

def ftest(weight_list):
    temp = weighted_sum_cols(
        mod_list = all_mod_cols,
        mod_weights = weight_list,
        df = mod_test)
    out = get_rmse(
        observed = mod_test['y'], 
        predicted = temp)
    return(out)

# +


list_of_weight_lists = [uniform_weights,
#                         uniform_by_type_weights,
                        inv_std_weights,
                        inv_var_weights,
                        inv_rmse_weights
                        ]
list_of_weight_lists_names = ['uniform_weights',
#                         'uniform_by_type_weights',
                        'inv_std_weights',
                        'inv_var_weights',
                        'inv_rmse_weights'
                        ]
ens_train_rmses = [ftrain(e) for e in list_of_weight_lists]
ens_test_rmses  = [ftest(e) for e in list_of_weight_lists]
# -

# stack model (no validation) --------------------------------------------------
regr = MLPRegressor(random_state=1, hidden_layer_sizes = (100, 100), max_iter=500)
regr.fit(mod_train.loc[:, all_mod_cols], mod_train.loc[:, 'y'])

# +
ens_train_rmses += [get_rmse(
    observed = mod_train['y'], 
    predicted = regr.predict(mod_train.loc[:, all_mod_cols]))]

ens_test_rmses += [get_rmse(
    observed = mod_test['y'], 
    predicted = regr.predict(mod_test.loc[:, all_mod_cols]))]
# -

# stack one neuron to get optimized weights ----------------------------------
# stack model (no validation) --------------------------------------------------
regr = MLPRegressor(random_state=1, hidden_layer_sizes = (1), max_iter=500, activation = 'identity')
regr.fit(mod_train.loc[:, all_mod_cols], mod_train.loc[:, 'y'])

# +
ens_train_rmses += [get_rmse(
    observed = mod_train['y'], 
    predicted = regr.predict(mod_train.loc[:, all_mod_cols]))]

ens_test_rmses += [get_rmse(
    observed = mod_test['y'], 
    predicted = regr.predict(mod_test.loc[:, all_mod_cols]))]
# -

pd.DataFrame(zip(
    list_of_weight_lists_names+['mlp_predictions', 'single_neuron'],
    ens_train_rmses,
    ens_test_rmses), columns = ['EnsembleMethod', 'Train', 'Test'])







# +
# Is there a small MLP that would have performed well?
# Yes, but none are stelar.

# def f(sizes = (100), iters = 500):
#     regr = MLPRegressor(random_state=1, hidden_layer_sizes = sizes, max_iter=iters)
#     regr.fit(mod_train.loc[:, all_mod_cols], mod_train.loc[:, 'y'])
#     out = [get_rmse(
#             observed = mod_train['y'], 
#             predicted = regr.predict(mod_train.loc[:, all_mod_cols])), 
#            get_rmse(
#             observed = mod_test['y'], 
#             predicted = regr.predict(mod_test.loc[:, all_mod_cols]))]
#     return(out)

#  params = [(x, y) for x in [
#      1, 3, 30, 60, 90,
#      (1, 1), (3, 3), (30, 30), (60, 60), (90, 90)                      
#                            ] for y in [1, 3, 30, 60, 90, 300]]
    
# rmses = [f(e[0], e[1]) for e in params]

# rmses_summary = pd.concat([
#     pd.DataFrame(params, columns = ['Neurons', 'Epochs']), 
#     pd.DataFrame(rmses, columns = ['TrainRMSE', 'TestRMSE'])], axis = 1)

# Neurons	Epochs	TrainRMSE	TestRMSE
# (3, 3)	1	    0.585326	0.942080
# 30    	1	    4.322395	0.960786
# (90, 90)	3	    1.301509	1.014392
# (3, 3)	3	    0.456046	1.022075
# -



# +
# look for saturation within a model 

# pass in a mod list so the same code can be reused for all models in addition to a single type

def resample_model_averages(individual_mods = ['cat_'+str(i) for i in range(10)],
                            n_iter = 5,
                            p = None):
    # get performance for all reps individually
    # from 2 -> max rep -1
    # get iter rmses by randomly combining the replicates
    n_mods = len(individual_mods)

    # one model ----
    individual_rmses = [
        get_rmse(observed = mod_test['y'], 
                 predicted= mod_test[e]) for e in individual_mods]
    individual_rmses = pd.DataFrame(zip(
        [1 for i in range(n_mods)],
         [[individual_mods[i]] for i in range(n_mods)],
        individual_rmses), columns = ['n_mods', 'drawn_mods', 'rmse'])

    # 2 to n-1 models ---- 
    def f(individual_mods = individual_mods, draw_mods = 2, n_iter = 5, p = None):
        if type(p) != list:
            p = [1/len(individual_mods) for i in range(len(individual_mods))]

        drawn_mods_list = [list(np.random.choice(individual_mods, draw_mods, p = p)) for i in range(n_iter)]
        drawn_mods_rmse = [get_rmse(observed = mod_test['y'], 
                                    predicted= weighted_sum_cols(
                                        mod_list = drawn_mods,
                                        mod_weights = [1/draw_mods for i in range(draw_mods)],
                                        df = mod_test) 
                                   ) for drawn_mods in drawn_mods_list]
        out = pd.DataFrame(zip(
            [draw_mods for i in range(n_iter)],
            drawn_mods_list,
            drawn_mods_rmse
        ), columns = ['n_mods', 'drawn_mods', 'rmse'])
        return(out)

    out_mid = pd.concat([f(individual_mods = individual_mods, 
                           draw_mods = i,
                           n_iter = n_iter, 
                           p = p
                           ) for i in range(2, (n_mods))], 
                           axis = 0)
    # all models ----
    all_reps_rmses = [
        get_rmse(
            observed = mod_test['y'], 
            predicted= weighted_sum_cols(
                mod_list = individual_mods,
                mod_weights = [1/n_mods for i in range(n_mods)],
                df = mod_test) )]

    all_reps_rmses = pd.DataFrame(zip(
        [n_mods for i in range(n_mods)],
        [individual_mods for i in range(n_mods)],
        all_reps_rmses), columns = ['n_mods', 'drawn_mods', 'rmse'])

    out = pd.concat([individual_rmses,
                     out_mid,
                     all_reps_rmses])
    out = out.reset_index().drop(columns = 'index')
    return(out)
# -



# ## Table 1

mask = (summary_df.data_source == 'Multi')
px.scatter(summary_df.loc[mask, ], x = 'model', y = 'test_rmse')

mask = (summary_df.data_source == 'Multi')
temp = summary_df.loc[mask, ['data_source', 'model', 'replicate', 
                             'train_rmse', 'test_rmse']
                ].groupby(['data_source', 'model']
                ).agg(train_rmse = ('train_rmse', np.mean),
                      test_rmse = ('test_rmse', np.mean)
                ).reset_index()
best_loss = temp.loc[(temp.test_rmse == min(temp.test_rmse)), ['model', 'test_rmse']]

# +
temp = pd.DataFrame(zip(list_of_weight_lists_names+['mlp_predictions'],
    ens_train_rmses,
    ens_test_rmses), columns = ['EnsembleMethod', 'TrainRMSE', 'TestRMSE'])

best_loss_name = list(best_loss['model'])[0]
temp["Percent"+best_loss_name+"RMSE"]= temp['TestRMSE']/float(best_loss['test_rmse'])
# temp.to_csv("../output/Table1.csv", index = False)
temp
# -



# ##  Supplemental Figure 1

# ### Averaging within a model type

# +
mod_list = ['lm', 'bglr', 'knn', 'rf', 'rnr', 'svrl', 'full', 'cat']

# list of resampled rmses for each model
resampled_model_averages_list = [
    resample_model_averages(
        individual_mods = [e+'_'+str(i) for i in range(10)],
        n_iter = 50,
        p = None) 
    for e in mod_list]


for i in range(len(mod_list)):
    temp = resampled_model_averages_list[i]
    temp.loc[:, 'mod_group'] = mod_list[i]
    resampled_model_averages_list[i] = temp
    
temp = pd.concat(resampled_model_averages_list)
# temp.to_csv('../output/SFigure1_Data_RMSE_Ens_Reps.csv', index = False)

px.scatter(temp, x = 'n_mods', y = 'rmse', color = 'mod_group', trendline = 'lowess')
# -





# +
# temp = resample_model_averages(
#     individual_mods = ['cat_'+str(i) for i in range(10)],
#     n_iter = 50,
#     p = None)
# temp.head()

# +
# px.scatter(temp, x = 'n_mods', y = 'rmse', trendline='lowess')

# +
# temp = resample_model_averages(
#     individual_mods = ['bglr_'+str(i) for i in range(10)],
#     n_iter = 50,
#     p = None)
# temp.head()

# +
# px.box(temp, x = 'n_mods', y = 'rmse')
# -

# ### Averaging Across model types (weighting by model type)

# +
# uniform_by_type_weights

# +
# resampled_model_averages_list = [
#     resample_model_averages(
#         individual_mods = [e+'_'+str(i) for i in range(10)],
#         n_iter = 50,
#         p = uniform_by_type_weights) 
#     for e in mod_list]


# for i in range(len(mod_list)):
#     temp = resampled_model_averages_list[i]
#     temp.loc[:, 'mod_group'] = mod_list[i]
#     resampled_model_averages_list[i] = temp
    
# temp = pd.concat(resampled_model_averages_list)
# temp.to_csv('../output/SFigure1_Data_RMSE_Ens_Mods_Unif.csv', index = False)

# px.scatter(temp, x = 'n_mods', y = 'rmse', color = 'mod_group', trendline = 'lowess')
# -





# deprecated? there are files here ending in _redo.csv
    # # all models
    # save_path = '../output/SFigure1_Data_RMSE_Ens_Mods.csv'

    # if os.path.exists(save_path):
    #     temp = pd.read_csv(save_path)
    # else:
    #     temp = resample_model_averages(
    #         individual_mods = all_mod_cols,
    #         n_iter = 500,
    #         p = uniform_by_type_weights)

    # # temp.to_csv(save_path, index = False)
    # temp.head()
    # # px.box(temp, x = 'n_mods', y = 'rmse')
    # px.scatter(temp, x = 'n_mods', y = 'rmse', trendline = 'lowess')

# ### Averaging Across select model types

# +
# best few models
save_path = '../output/SFigure1_Data_RMSE_Ens_SelectMods.csv'


# there's a redo file later. Only run if that doesn't exist. 
if not os.path.exists(save_path.replace('.csv', '')+'_redo.csv'):
    if os.path.exists(save_path):
        temp = pd.read_csv(save_path)
    else:
        temp = resample_model_averages(
            individual_mods = ['bglr_'+str(i) for i in range(10)] + ['cat_'+str(i) for i in range(10)],
            n_iter = 500#,
            #     p = uniform_by_type_weights
            )

    # temp.to_csv(save_path, index = False)
    temp.head()
    # px.box(temp, x = 'n_mods', y = 'rmse')
    px.scatter(temp, x = 'n_mods', y = 'rmse', trendline = 'lowess')

# +

# temp = resample_model_averages(
#     individual_mods = ['bglr_'+str(i) for i in range(10)] + ['cat_'+str(i) for i in range(10)],
#     n_iter = 50#,
# #     p = uniform_by_type_weights
# )
# temp.head()
# # px.box(temp, x = 'n_mods', y = 'rmse'#, box=True#, points="all"
# #          )
# px.scatter(temp, x = 'n_mods', y = 'rmse', trendline = 'lowess')
# -

# ### Average across all pairwise combinations 

# +
save_path = '../output/SFigure1_Data_RMSE_Ens_AllMods.csv'



# there's a redo file later. Only run if that doesn't exist. 
if not os.path.exists(save_path.replace('.csv', '')+'_redo.csv'):
    if os.path.exists(save_path):
        temp = pd.read_csv(save_path)
    else:
        temp_list = []
        for ii in range(len(mod_list)):
            for jj in range(ii, len(mod_list)):
                if ii != jj:
                    # print(mod_list[ii], mod_list[jj])
                    temp = resample_model_averages(
                        individual_mods = [mod_list[ii]+'_'+str(i) for i in range(10)] + [mod_list[jj]+'_'+str(i) for i in range(10)],
                        n_iter = 500#,
                    )
                    temp['Model1'] = mod_list[ii]
                    temp['Model2'] = mod_list[jj]
                    temp_list = temp_list+[temp]
        temp = pd.concat(temp_list)

    #     temp.to_csv(save_path, index = False)
# -



# REDO and make it match expectations: ---------------------------------------
# Copied from below
def simulate_chromosome(chromosome):
    draw_cols = []
    n_mods_of_type = []

    for mod_type in mod_types:
        if chromosome[mod_type] > 0:
            draw_cols += [mod_type+'_'+str(e) for e in list(np.random.randint(0, 10, chromosome[mod_type]))]
            # for ensemble inv to model type
            n_mods_of_type += [chromosome[mod_type] for i in range(chromosome[mod_type])]


    # Calculate the weights to use
    if chromosome['ensemble'] == 'uniform_weights':
        draw_weights = [1/len(draw_cols) for e in draw_cols]

    if chromosome['ensemble'] == 'uniform_by_type_weights': 
        draw_weights = [1/e for e in n_mods_of_type]
        draw_weights = [e/np.sum(draw_weights)  for e in draw_weights]

    if chromosome['ensemble'] == 'inv_std_weights': 
        draw_weights = get_mod_std(mod_list = draw_cols, df = mod_train)
        draw_weights = [1/e for e in draw_weights]
        draw_weights = [e/np.sum(draw_weights) for e in draw_weights]

    if chromosome['ensemble'] == 'inv_var_weights':
        draw_weights = get_mod_std(mod_list = draw_cols, df = mod_train)
        draw_weights = [e**2 for e in draw_weights]
        draw_weights = [1/e for e in draw_weights]
        draw_weights = [e/np.sum(draw_weights) for e in draw_weights]

    if chromosome['ensemble'] == 'inv_rmse_weights':
        draw_weights = [1/get_rmse(mod_train.y, mod_train[e]) for e in draw_cols]
        draw_weights = [e/np.sum(draw_weights) for e in draw_weights]

    # measure
    phenotype = get_rmse(
                observed = mod_test['y'], 
                predicted = weighted_sum_cols(
                       mod_list = draw_cols,
                    mod_weights = draw_weights,
                             df = mod_test))
    return(phenotype)



# +
# (wishlist) I love the idea of makeing a version of this function that would allow for the number of models to vary independently.
# That's easy to build but hard to visualize effectively (stacked surfaces)
# And the genetic algorithm gets at this already.



# def simulate_bimodel_ensemble(
#     Model1 = 'lm',
#     Model2 = 'bglr',
#     ensemble = 'uniform_weights', #'uniform_by_type_weights', 'inv_std_weights', 
#     #                'inv_var_weights', 'inv_rmse_weights',
#     n_mods1 = 2, 
#     n_mods2 = 2, 
#     rmse_sim_iterations  = 50):

#     in_chromosome = {
#             'ensemble': '',
#                   'lm': 0, 
#                 'bglr': 0, 
#                  'knn': 0, 
#                   'rf': 0, 
#                  'rnr': 0, 
#                 'svrl': 0, 
#                 'full': 0, 
#                  'cat': 0   
#         }
#     in_chromosome['ensemble'] = ensemble
#     in_chromosome[Model1] = n_mods1
#     in_chromosome[Model2] = n_mods2

#     out_rmses = [simulate_chromosome(chromosome = in_chromosome) for i in range(rmse_sim_iterations)]
#     # make into nice df
#     out_rmses = pd.DataFrame(out_rmses, columns=['rmse'])
#     out_rmses[['Model1', 'Model2', 'n_mods1', 'n_mods2']] = Model1, Model2, n_mods1, n_mods2

#     return(out_rmses)

# M  = pd.concat([simulate_bimodel_ensemble(
#     Model1 = 'bglr',
#     Model2 = m2,
#     ensemble = 'uniform_weights', #'uniform_by_type_weights', 'inv_std_weights', 
#     #                'inv_var_weights', 'inv_rmse_weights',
#     n_mods1 = x, 
#     n_mods2 = y, 
#     rmse_sim_iterations  = 20
# ) for x in range(11) for y in range(11) for m2 in mod_list])



# M = M.groupby(['Model1', 'Model2', 'n_mods1', 'n_mods2']).agg(rmse = ('rmse', np.mean)).reset_index()

# import plotly.graph_objects as go

# Model1 = list(set(M.Model1))[0]


# fig = go.Figure()
# for i in range(len(set(M.Model2))):
#     Model2 = list(set(M.Model2))[i]
#     ModelColor = ['#8ecae6', '#219ebc', '#023047', '#ffb703', '#fb8500', '#e63946', '#000000', '#8338ec'][i]
    
#     fig.add_trace(go.Mesh3d(x = M.loc[(M.Model2 == Model2), 'n_mods1'], 
#                             y = M.loc[(M.Model2 == Model2), 'n_mods2'], 
#                             z = M.loc[(M.Model2 == Model2), 'rmse'], 
#                             name = Model2,
# #                             legendgroup = Model2,
# #                             legendgrouptitle  = {'text': Model2},
#                             showlegend = True,
#                             color= ModelColor,
#                             opacity = 0.28))


# fig.update_layout(
#     title = "RMSE from Ensemble Containing "+Model1,
    
#     scene = dict(
#         xaxis_title='# of '+Model1+' Models',
#         yaxis_title='# of Other Models',
#         zaxis_title='RMSE'),
# #         width=700,
# #         margin=dict(r=20, b=10, l=10, t=10)
# )



# fig.show()

# import plotly
# plotly.offline.plot(fig, filename='demo3d.html') 
# -

def simulate_bimodel_ensemble(
    Model1 = 'lm',
    Model2 = 'bglr',
    ensemble = 'uniform_weights', #'uniform_by_type_weights', 'inv_std_weights', 
    #                'inv_var_weights', 'inv_rmse_weights',
    n_mods = 2, 
    rmse_sim_iterations  = 50):

    in_chromosome = {
            'ensemble': '',
                  'lm': 0, 
                'bglr': 0, 
                 'knn': 0, 
                  'rf': 0, 
                 'rnr': 0, 
                'svrl': 0, 
                'full': 0, 
                 'cat': 0   
        }
    in_chromosome['ensemble'] = ensemble
    in_chromosome[Model1] = n_mods
    in_chromosome[Model2] = n_mods

    out_rmses = [simulate_chromosome(chromosome = in_chromosome) for i in range(rmse_sim_iterations)]
    # make into nice df
    out_rmses = pd.DataFrame(out_rmses, columns=['rmse'])
    out_rmses[['Model1', 'Model2', 'ensemble', 'n_mods']] = Model1, Model2, ensemble, n_mods

    return(out_rmses)







# +
save_path = '../output/SFigure1_Data_RMSE_Ens_SelectMods_redo.csv'

if os.path.exists(save_path):
    sim_res = pd.read_csv(save_path)
else:
    uniq_mod_combinations = [[mod_list[x], mod_list[y]] for x in range(len(mod_list)) for y in range(x, len(mod_list))]

    sim_list = [
        simulate_bimodel_ensemble(
            Model1 = uniq_mod_combination[0], #'lm',
            Model2 = uniq_mod_combination[1], #'bglr',
            ensemble = ee,
            n_mods = i, 
            rmse_sim_iterations  = 50)
        for i in range(1, 11) 
        for uniq_mod_combination in uniq_mod_combinations 
        for ee in [
            'uniform_weights', 'uniform_by_type_weights', 'inv_std_weights', 
            'inv_var_weights', 'inv_rmse_weights'
    ]]

    sim_res = pd.concat(sim_list)
    sim_res = sim_res.loc[(sim_res.n_mods != 0)]
    sim_res.to_csv(save_path, index = False)
    # 5m 18s
# -











def simulate_allmodel_ensemble(
    ensemble = 'uniform_weights', #'uniform_by_type_weights', 'inv_std_weights', 
    #                'inv_var_weights', 'inv_rmse_weights',
    n_mods = 2, 
    rmse_sim_iterations  = 50):

    in_chromosome = {
            'ensemble': '',
                  'lm': 0, 
                'bglr': 0, 
                 'knn': 0, 
                  'rf': 0, 
                 'rnr': 0, 
                'svrl': 0, 
                'full': 0, 
                 'cat': 0   
        }
    in_chromosome['ensemble'] = ensemble

    out_rmses = [simulate_chromosome(chromosome = in_chromosome) for i in range(rmse_sim_iterations)]
    # make into nice df
    out_rmses = pd.DataFrame(out_rmses, columns=['rmse'])
    out_rmses[['ensemble', 'n_mods']] = ensemble, n_mods

    return(out_rmses)

# +
# n_mods = 2

# draw_reps = np.random.choice([key for key in in_chromosome.keys() if key != ensemble],  n_mods)
# for draw_rep in draw_reps:
#     in_chromosome[draw_rep] += 1



# +
# simulate_allmodel_ensemble(
#     ensemble = 'uniform_weights', #'uniform_by_type_weights', 'inv_std_weights', 
#     #                'inv_var_weights', 'inv_rmse_weights',
#     n_mods = 2, 
#     rmse_sim_iterations  = 50)
# -



# +
save_path = '../output/SFigure1_Data_RMSE_Ens_AllMods_redo.csv'

if os.path.exists(save_path):
    sim_res = pd.read_csv(save_path)
else:
    sim_list = [
        simulate_allmodel_ensemble(
            ensemble = ee,
            n_mods = i, 
            rmse_sim_iterations  = 50)
        for i in range(1, 11) 
        for ee in [
            'uniform_weights', 'uniform_by_type_weights', 'inv_std_weights', 
            'inv_var_weights', 'inv_rmse_weights'
    ]]

    sim_res = pd.concat(sim_list)
    sim_res = sim_res.loc[(sim_res.n_mods != 0)]
    sim_res.to_csv(save_path, index = False)
# -

px.box(sim_res, x = 'n_mods', y = 'rmse', color = 'ensemble')





def simulate_any_n_model_ensemble(
    ensemble = 'uniform_weights', #'uniform_by_type_weights', 'inv_std_weights', 
    #                'inv_var_weights', 'inv_rmse_weights',
    n_mods = 2, 
    rmse_sim_iterations  = 50):

    in_chromosome = {
            'ensemble': '',
                  'lm': 0, 
                'bglr': 0, 
                 'knn': 0, 
                  'rf': 0, 
                 'rnr': 0, 
                'svrl': 0, 
                'full': 0, 
                 'cat': 0   
        }
    in_chromosome['ensemble'] = ensemble

    draw_models = list(np.random.choice([key for key in in_chromosome.keys() if key != 'ensemble'], n_mods))
    for draw_model in draw_models:
        in_chromosome[draw_model] += 1
    
    out_rmses = [simulate_chromosome(chromosome = in_chromosome) for i in range(rmse_sim_iterations)]
    # make into nice df
    out_rmses = pd.DataFrame(out_rmses, columns=['rmse'])
    out_rmses[['ensemble', 'n_mods']] = ensemble, n_mods

    return(out_rmses)


# +
save_path = '../output/SFigure1_Data_RMSE_Ens_Any_n_Mods_redo.csv'

if os.path.exists(save_path):
    sim_res = pd.read_csv(save_path)
else:
    sim_list = [
        simulate_any_n_model_ensemble(
            ensemble = ee,
            n_mods = i, 
            rmse_sim_iterations  = 1)
        for i_sim_independent in range(50)
        for i in range(1, 81) 
        for ee in [
            'uniform_weights', 'uniform_by_type_weights', 'inv_std_weights', 
            'inv_var_weights', 'inv_rmse_weights'
    ]]

    sim_res = pd.concat(sim_list)
    sim_res = sim_res.loc[(sim_res.n_mods != 0)]
    sim_res.to_csv(save_path, index = False)
# -





# ## Figure 1

# ### draw 2 models



# +
# single model performance
# summary_df.loc[summary_df.data_source == 'Multi', ['data_source', 'model_class', 'model', 'replicate', 'test_rmse']
#          ].to_csv('../output/Figure1_Data_1Mod.csv', index = False)
# -















temp = [[x, y] for x in all_mod_cols for y in all_mod_cols]
temp = pd.DataFrame(temp, columns=['Model1', 'Model2'])
temp['RMSE'] = np.nan
temp.head()

# +
# best few models
save_path = '../output/Figure1_Data_Heatmap.csv'

if os.path.exists(save_path):
    temp = pd.read_csv(save_path)
else:
    for i in tqdm.tqdm(range(temp.shape[0])):
        temp.loc[i, 'RMSE'] = get_rmse(
            observed = mod_test['y'], 
            predicted = weighted_sum_cols(
                   mod_list = [temp.loc[i, 'Model1'], temp.loc[i, 'Model2']],
                mod_weights = [0.5, 0.5],
                         df = mod_test))

# temp.to_csv(save_path, index = False)
temp.head()
# -

temp_wide = temp.pivot(index='Model1', columns='Model2', values='RMSE').reset_index()
temp_wide.index = temp_wide['Model1']
temp_wide = temp_wide.drop(columns='Model1')

px.imshow(temp_wide)





# +
# px.scatter_3d(temp, 
#               x = "Model1", y = "Model2", z = "RMSE", 
#               color = "RMSE", 
# #          size = "RMSE"
#              )
# -

# # Genetic Algorithm

# +
def rand_unif_chromosome(): 
    chromosome = {
        'ensemble': "",
              'lm': 0, 
            'bglr': 0, 
             'knn': 0, 
              'rf': 0, 
             'rnr': 0, 
            'svrl': 0, 
            'full': 0, 
             'cat': 0   
    }
    for key in chromosome.keys():
        if key == 'ensemble':
            chromosome[key] = np.random.choice([
                'uniform_weights', 'uniform_by_type_weights', 'inv_std_weights', 
                'inv_var_weights', 'inv_rmse_weights'])
        else:
            chromosome[key] = np.random.randint(0, 10)
    return(chromosome)

def check_at_least_one_mod(chromosome):
    total_mods = np.sum([chromosome[key] for key in chromosome.keys() if key != 'ensemble'])
    if total_mods > 0 :
        return(True)
    else:
        return(False)

# mutate
def mutate_chromosome(chromosome, n = 1): 
    chromosome = chromosome.copy()
    for i in range(n):
        key = np.random.choice(['ensemble']+mod_types)
        if key == 'ensemble':
            chromosome[key] = np.random.choice([
                'uniform_weights', 'uniform_by_type_weights', 'inv_std_weights', 
                'inv_var_weights', 'inv_rmse_weights'])
        else:
            chromosome[key] = np.random.randint(0, 11)
    return(chromosome)

# simulate
def simulate_chromosome(chromosome, calc_phenotype_with = 'test'):
    draw_cols = []
    n_mods_of_type = []

    for mod_type in mod_types:
        if chromosome[mod_type] > 0:
            draw_cols += [mod_type+'_'+str(e) for e in list(np.random.randint(0, 10, chromosome[mod_type]))]
            # for ensemble inv to model type
            n_mods_of_type += [chromosome[mod_type] for i in range(chromosome[mod_type])]


    # Calculate the weights to use
    if chromosome['ensemble'] == 'uniform_weights':
        draw_weights = [1/len(draw_cols) for e in draw_cols]

    if chromosome['ensemble'] == 'uniform_by_type_weights': 
        draw_weights = [1/e for e in n_mods_of_type]
        draw_weights = [e/np.sum(draw_weights)  for e in draw_weights]

    if chromosome['ensemble'] == 'inv_std_weights': 
        draw_weights = get_mod_std(mod_list = draw_cols, df = mod_train)
        draw_weights = [1/e for e in draw_weights]
        draw_weights = [e/np.sum(draw_weights) for e in draw_weights]

    if chromosome['ensemble'] == 'inv_var_weights':
        draw_weights = get_mod_std(mod_list = draw_cols, df = mod_train)
        draw_weights = [e**2 for e in draw_weights]
        draw_weights = [1/e for e in draw_weights]
        draw_weights = [e/np.sum(draw_weights) for e in draw_weights]

    if chromosome['ensemble'] == 'inv_rmse_weights':
        draw_weights = [1/get_rmse(mod_train.y, mod_train[e]) for e in draw_cols]
        draw_weights = [e/np.sum(draw_weights) for e in draw_weights]

    # measure
    if calc_phenotype_with == 'test':
        phenotype = get_rmse(
                    observed = mod_test['y'], 
                    predicted = weighted_sum_cols(
                           mod_list = draw_cols,
                        mod_weights = draw_weights,
                                 df = mod_test))
    elif calc_phenotype_with == 'train':
        phenotype = get_rmse(
                    observed = mod_train['y'], 
                    predicted = weighted_sum_cols(
                           mod_list = draw_cols,
                        mod_weights = draw_weights,
                                 df = mod_train))
    else:
        print('"'+calc_phenotype_with+'" is neither of the expected values "train" or "test". Returning np.nan')
        phenotype = np.nan
        
    return(phenotype)

# propagate
# -



# +
# Simulate the best model

save_path = '../output/01_GA_test_best.csv'
if os.path.exists(save_path):
    GA_test_best = pd.read_csv(save_path)
else:
    GA_test_best = best_GA = pd.DataFrame({'RMSE': [
        simulate_chromosome(chromosome = {
        'ensemble': "inv_rmse_weights",
              'lm': 2, 
            'bglr': 8, 
             'knn': 0, 
              'rf': 4, 
             'rnr': 0, 
            'svrl': 2, 
            'full': 0, 
             'cat': 6   
        }) for i in range(50)]})

    
save_path = '../output/01_GA_test_best_inv_var.csv'
if os.path.exists(save_path):
    GA_test_best_inv_var = pd.read_csv(save_path)
else:
    GA_test_best_inv_var = best_GA = pd.DataFrame({'RMSE': [
        simulate_chromosome(chromosome = {
        'ensemble': "inv_var_weights",
              'lm': 2, 
            'bglr': 8, 
             'knn': 0, 
              'rf': 4, 
             'rnr': 0, 
            'svrl': 2, 
            'full': 0, 
             'cat': 6   
        }) for i in range(50)]})
    
# best_GA['Model'] = 'GA'
# -

px.box(best_GA, x = 'Model', y = 'RMSE')

# +
# Influence of weighting by inverse variation istead of rmse
best_GA_var = pd.DataFrame({'RMSE': [

simulate_chromosome(chromosome = {
        'ensemble': "inv_var_weights",
              'lm': 2, 
            'bglr': 8, 
             'knn': 0, 
              'rf': 4, 
             'rnr': 0, 
            'svrl': 2, 
            'full': 0, 
             'cat': 6   
    }) for i in range(50)]})


best_GA_var['Model'] = 'GA_Var'
# -

px.box(pd.concat([best_GA, best_GA_var]), x = 'Model', y = 'RMSE')





# +
# # Settings -------------------------------------------------------------------
# starting_pop_size    = 100 # Initial Population
# selection_iterations = 300  # Select-Mutate-Evaluate Cycles
# selection_pop_size   = 15  # population size used for mutation
# rand_pop_size        = 5   # also randomly draw x indices 
# rmse_sim_iterations  = 50  # resamplings of RMSE

# # Initialization -------------------------------------------------------------

# # for chromosomes and summary info
# Population_History = []
# # for distribution of rmses info. will match index in Population_History
# out_rmses_list = []

# for i in range(starting_pop_size):
#     in_chromosome = rand_unif_chromosome()
    
#     # reinitialize chromosome if all models are set to 0
#     while not check_at_least_one_mod(in_chromosome):
#         in_chromosome = rand_unif_chromosome()

#     out_rmses = np.array([simulate_chromosome(chromosome= in_chromosome) for i in range(rmse_sim_iterations)])

#     in_chromosome['Iteration'] = 0
#     in_chromosome['RMSE'] = out_rmses.mean()
    
#     Population_History += [in_chromosome]
#     out_rmses_list += [out_rmses]
    
    
# Population_History = pd.concat([pd.DataFrame(Population_History[i], index = [i]) for i in range(starting_pop_size)])

# # Evolution ------------------------------------------------------------------
# for iteration in tqdm.tqdm(range(1, selection_iterations+1)):
#     # Selection:
#     selected_chromosomes = Population_History.sort_values('RMSE').index[0:selection_pop_size]
    
#     if rand_pop_size > 0 :
#         selected_chromosomes = list(selected_chromosomes)+list(np.random.choice(Population_History.index, rand_pop_size))

#     for idx in selected_chromosomes:
#         idx_chromosome = dict(Population_History.loc[idx, ['ensemble']+mod_types]).copy()
#         # Mutate
#         mutate_n_times = int(np.random.geometric(p=0.5, size=1)) # use a geometric distribution to sometimes get big jumps
#         # https://distribution-explorer.github.io/discrete/geometric.html
#         in_chromosome = mutate_chromosome(chromosome = idx_chromosome, n = mutate_n_times)

#         # Confirm mutation is new and valid (not all 0)
#         while not (check_at_least_one_mod(in_chromosome) & (idx_chromosome != in_chromosome)):
#             in_chromosome = mutate_chromosome(chromosome = idx_chromosome, n = 1)

#         out_rmses = np.array([simulate_chromosome(chromosome= in_chromosome) for i in range(rmse_sim_iterations)])

#         in_chromosome['Iteration'] = iteration
#         in_chromosome['RMSE'] = out_rmses.mean()

#         # This is sloppy, but that's probably okay
#         Population_History = pd.concat([
#             Population_History, 
#             pd.DataFrame(in_chromosome, index = [max(Population_History.index)+1])])

#         out_rmses_list += [out_rmses]
        
# # Make distributions easier to work with
# Population_RMSE_Dists = np.concatenate(out_rmses_list).reshape([len(out_rmses_list), rmse_sim_iterations])


# path_summary = "./genetic_algo_Population_History.p"
# path_rmse_dists = "./genetic_algo_rmse_dists.p"

# pkl.dump(Population_History, open(path_summary, 'wb'))
# pkl.dump(Population_RMSE_Dists, open(path_rmse_dists, 'wb'))

# +
path_summary = "./genetic_algo_Population_History.p"
path_rmse_dists = "./genetic_algo_rmse_dists.p"
if (os.path.exists(path_summary) & os.path.exists(path_rmse_dists)):
    Population_History = pkl.load(open(path_summary, 'rb'))
    Population_RMSE_Dists = pkl.load(open(path_rmse_dists, 'rb'))
else:
    # Settings -------------------------------------------------------------------
    starting_pop_size    = 100 # Initial Population
    selection_iterations = 300  # Select-Mutate-Evaluate Cycles
    selection_pop_size   = 15  # population size used for mutation
    rand_pop_size        = 5   # also randomly draw x indices 
    rmse_sim_iterations  = 50  # resamplings of RMSE

    # Initialization -------------------------------------------------------------

    # for chromosomes and summary info
    Population_History = []
    # for distribution of rmses info. will match index in Population_History
    out_rmses_list = []

    for i in range(starting_pop_size):
        in_chromosome = rand_unif_chromosome()

        # reinitialize chromosome if all models are set to 0
        while not check_at_least_one_mod(in_chromosome):
            in_chromosome = rand_unif_chromosome()

        out_rmses = np.array([simulate_chromosome(chromosome= in_chromosome) for i in range(rmse_sim_iterations)])

        in_chromosome['Iteration'] = 0
        in_chromosome['RMSE'] = out_rmses.mean()

        Population_History += [in_chromosome]
        out_rmses_list += [out_rmses]


    Population_History = pd.concat([pd.DataFrame(Population_History[i], index = [i]) for i in range(starting_pop_size)])

    # Evolution ------------------------------------------------------------------
    for iteration in tqdm.tqdm(range(1, selection_iterations+1)):
        # Selection:
        selected_chromosomes = Population_History.sort_values('RMSE').index[0:selection_pop_size]

        if rand_pop_size > 0 :
            selected_chromosomes = list(selected_chromosomes)+list(np.random.choice(Population_History.index, rand_pop_size))

        for idx in selected_chromosomes:
            idx_chromosome = dict(Population_History.loc[idx, ['ensemble']+mod_types]).copy()
            # Mutate
            mutate_n_times = int(np.random.geometric(p=0.5, size=1)) # use a geometric distribution to sometimes get big jumps
            # https://distribution-explorer.github.io/discrete/geometric.html
            in_chromosome = mutate_chromosome(chromosome = idx_chromosome, n = mutate_n_times)

            # Confirm mutation is new and valid (not all 0)
            while not (check_at_least_one_mod(in_chromosome) & (idx_chromosome != in_chromosome)):
                in_chromosome = mutate_chromosome(chromosome = idx_chromosome, n = 1)

            out_rmses = np.array([simulate_chromosome(chromosome= in_chromosome) for i in range(rmse_sim_iterations)])

            in_chromosome['Iteration'] = iteration
            in_chromosome['RMSE'] = out_rmses.mean()

            # This is sloppy, but that's probably okay
            Population_History = pd.concat([
                Population_History, 
                pd.DataFrame(in_chromosome, index = [max(Population_History.index)+1])])

            out_rmses_list += [out_rmses]

    # Make distributions easier to work with
    Population_RMSE_Dists = np.concatenate(out_rmses_list).reshape([len(out_rmses_list), rmse_sim_iterations])
    
    pkl.dump(Population_History, open(path_summary, 'wb'))
    pkl.dump(Population_RMSE_Dists, open(path_rmse_dists, 'wb'))
    
    
    
if overwrite_saved_files == True:
    Population_History.to_csv("./genetic_algo_Population_History.csv")    
# -

px.scatter(Population_History, x = 'Iteration', y = 'RMSE', trendline = 'lowess')

# +
## Version of simulation where phenotype is measured by _TRAINING_ RMSE
# I'm trying to see if this changes the result substantually. I would have thought that
# weighting by rmse would be best for training 


path_summary = "./genetic_algo_Population_History2.p"
path_rmse_dists = "./genetic_algo_rmse_dists2.p"
if (os.path.exists(path_summary) & os.path.exists(path_rmse_dists)):
    Population_History = pkl.load(open(path_summary, 'rb'))
    Population_RMSE_Dists = pkl.load(open(path_rmse_dists, 'rb'))
else:
    # Settings -------------------------------------------------------------------
    starting_pop_size    = 100 # Initial Population
    selection_iterations = 300  # Select-Mutate-Evaluate Cycles
    selection_pop_size   = 15  # population size used for mutation
    rand_pop_size        = 5   # also randomly draw x indices 
    rmse_sim_iterations  = 50  # resamplings of RMSE

    # Initialization -------------------------------------------------------------

    # for chromosomes and summary info
    Population_History = []
    # for distribution of rmses info. will match index in Population_History
    out_rmses_list = []

    for i in range(starting_pop_size):
        in_chromosome = rand_unif_chromosome()

        # reinitialize chromosome if all models are set to 0
        while not check_at_least_one_mod(in_chromosome):
            in_chromosome = rand_unif_chromosome()

        out_rmses = np.array([simulate_chromosome(chromosome= in_chromosome) for i in range(rmse_sim_iterations)])

        in_chromosome['Iteration'] = 0
        in_chromosome['RMSE'] = out_rmses.mean()

        Population_History += [in_chromosome]
        out_rmses_list += [out_rmses]


    Population_History = pd.concat([pd.DataFrame(Population_History[i], index = [i]) for i in range(starting_pop_size)])

    # Evolution ------------------------------------------------------------------
    for iteration in tqdm.tqdm(range(1, selection_iterations+1)):
        # Selection:
        selected_chromosomes = Population_History.sort_values('RMSE').index[0:selection_pop_size]

        if rand_pop_size > 0 :
            selected_chromosomes = list(selected_chromosomes)+list(np.random.choice(Population_History.index, rand_pop_size))

        for idx in selected_chromosomes:
            idx_chromosome = dict(Population_History.loc[idx, ['ensemble']+mod_types]).copy()
            # Mutate
            mutate_n_times = int(np.random.geometric(p=0.5, size=1)) # use a geometric distribution to sometimes get big jumps
            # https://distribution-explorer.github.io/discrete/geometric.html
            in_chromosome = mutate_chromosome(chromosome = idx_chromosome, n = mutate_n_times)

            # Confirm mutation is new and valid (not all 0)
            while not (check_at_least_one_mod(in_chromosome) & (idx_chromosome != in_chromosome)):
                in_chromosome = mutate_chromosome(chromosome = idx_chromosome, n = 1)

            out_rmses = np.array([simulate_chromosome(chromosome= in_chromosome,
                                                      ###############################
                                                      calc_phenotype_with = 'train' # <- This has us use the training set for phenotype calc
                                                      ###############################    Does this change the best ensemble?
                                                     ) for i in range(rmse_sim_iterations)])

            in_chromosome['Iteration'] = iteration
            in_chromosome['RMSE'] = out_rmses.mean()

            # This is sloppy, but that's probably okay
            Population_History = pd.concat([
                Population_History, 
                pd.DataFrame(in_chromosome, index = [max(Population_History.index)+1])])

            out_rmses_list += [out_rmses]

    # Make distributions easier to work with
    Population_RMSE_Dists = np.concatenate(out_rmses_list).reshape([len(out_rmses_list), rmse_sim_iterations])
    
    pkl.dump(Population_History, open(path_summary, 'wb'))
    pkl.dump(Population_RMSE_Dists, open(path_rmse_dists, 'wb'))
    
    
    
if overwrite_saved_files == True:
    Population_History.to_csv("./genetic_algo_Population_History2.csv")    
# -

# Additional component of Supplemental Table 3
Population_History.groupby(['ensemble', 'lm', 'bglr', 'knn', 'rf', 'rnr', 'svrl', 'full', 'cat']
                          ).agg(Iteration = ('Iteration', np.min),
                                RMSE = ('RMSE', np.mean)
                          ).reset_index(
                          ).sort_values('RMSE').head(10)

# +
# Simulate the best model


def chromosome_performance(Name = "GA_test_best",
                        chromosome = GA_test_best,
                        n = 5):
    chromosomes = [{
            'ensemble': ith_ensemble,
                  'lm': chromosome['lm'], 
                'bglr': chromosome['bglr'], 
                 'knn': chromosome['knn'], 
                  'rf': chromosome['rf'], 
                 'rnr': chromosome['rnr'], 
                'svrl': chromosome['svrl'], 
                'full': chromosome['full'], 
                 'cat': chromosome['cat']   
            } for ith_ensemble in ['uniform_weights', 'uniform_by_type_weights', 'inv_std_weights', 
                    'inv_var_weights', 'inv_rmse_weights']]

    res_train  = [pd.DataFrame({'RMSE': [simulate_chromosome(chromosome = chromosome, calc_phenotype_with = 'train') for i in range(n)]}
                            ).assign(Ensemble = chromosome['ensemble'], Split = 'Train') 
                 for chromosome in chromosomes]
    res_test  = [pd.DataFrame({'RMSE': [simulate_chromosome(chromosome = chromosome, calc_phenotype_with = 'test') for i in range(n)]}
                            ).assign(Ensemble = chromosome['ensemble'], Split = 'Test') 
                 for chromosome in chromosomes]
    return(pd.concat(res_train+res_test).assign(Name = Name))



save_path = '../output/01_GA_performance_summary.csv'
if os.path.exists(save_path):
    performance_summary = pd.read_csv(save_path)
else:
    performance_summary = pd.concat([
        chromosome_performance(Name = "Best in Test",
                                chromosome = {
                'ensemble': "inv_rmse_weights",
                      'lm': 2, 
                    'bglr': 8, 
                     'knn': 0, 
                      'rf': 4, 
                     'rnr': 0, 
                    'svrl': 2, 
                    'full': 0, 
                     'cat': 6   
                                },
                               n = 50),
        chromosome_performance(Name = "Best in Train",
                                chromosome = {
                'ensemble': "inv_rmse_weights",
                      'lm': 0, 
                    'bglr': 1, 
                     'knn': 1, 
                      'rf': 0, 
                     'rnr': 10, 
                    'svrl': 0, 
                    'full': 0, 
                     'cat': 0   
                                },
                               n = 50)])
    performance_summary.to_csv(save_path)
# -



px.box(performance_summary, x = 'Ensemble', y = 'RMSE', color = 'Split', facet_col='Name')



# +
def q25(x): return(np.percentile(x, q = 25))
def q75(x): return(np.percentile(x, q = 75))
def iqr(x): return(q75(x)-q25(x))

performance_summary.groupby(['Name', 'Split', 'Ensemble']).agg(
    Min = ('RMSE', np.min),
    Q25 = ('RMSE', q25),
    Mean = ('RMSE', np.mean),
    Median = ('RMSE', np.median),
    Q75 = ('RMSE', q75),
    Max = ('RMSE', np.max),
    IQR = ('RMSE', iqr),
    StD = ('RMSE', np.std)
              ).reset_index(
).sort_values(['Name', 'Split', 'Mean'])
# -







# +
best_GA2 = pd.DataFrame({'RMSE': [

simulate_chromosome(chromosome = {
        'ensemble': "inv_rmse_weights",
              'lm': 0, 
            'bglr': 1, 
             'knn': 1, 
              'rf': 0, 
             'rnr': 10, 
            'svrl': 0, 
            'full': 0, 
             'cat': 0   
    }, calc_phenotype_with = 'train') for i in range(50)]})


best_GA2['Model'] = 'GA'

px.box(best_GA2, x = 'Model', y = 'RMSE')

# +
best_GA2 = pd.DataFrame({'RMSE': [

simulate_chromosome(chromosome = {
        'ensemble': "inv_rmse_weights",
              'lm': 0, 
            'bglr': 1, 
             'knn': 1, 
              'rf': 0, 
             'rnr': 10, 
            'svrl': 0, 
            'full': 0, 
             'cat': 0   
    }) for i in range(50)]})


best_GA2['Model'] = 'GA'

px.box(best_GA2, x = 'Model', y = 'RMSE')

# +
## is weighting by rmse a risky strategy? Are there some that do more poorly?
# -





# +
# # look for saturation within a model 

# # pass in a mod list so the same code can be reused for all models in addition to a single type

# def resample_model_averages(individual_mods = ['cat_'+str(i) for i in range(10)],
#                             n_iter = 5,
#                             p = None):
#     # get performance for all reps individually
#     # from 2 -> max rep -1
#     # get iter rmses by randomly combining the replicates
#     n_mods = len(individual_mods)

#     # one model ----
#     individual_rmses = [
#         get_rmse(observed = mod_test['y'], 
#                  predicted= mod_test[e]) for e in individual_mods]
#     individual_rmses = pd.DataFrame(zip(
#         [1 for i in range(n_mods)],
#          [[individual_mods[i]] for i in range(n_mods)],
#         individual_rmses), columns = ['n_mods', 'drawn_mods', 'rmse'])

#     # 2 to n-1 models ---- 
#     def f(individual_mods = individual_mods, draw_mods = 2, n_iter = 5, p = None):
#         if type(p) != list:
#             p = [1/len(individual_mods) for i in range(len(individual_mods))]

#         drawn_mods_list = [list(np.random.choice(individual_mods, draw_mods, p = p)) for i in range(n_iter)]
#         drawn_mods_rmse = [get_rmse(observed = mod_test['y'], 
#                                     predicted= weighted_sum_cols(
#                                         mod_list = drawn_mods,
#                                         mod_weights = [1/draw_mods for i in range(draw_mods)],
#                                         df = mod_test) 
#                                    ) for drawn_mods in drawn_mods_list]
#         out = pd.DataFrame(zip(
#             [draw_mods for i in range(n_iter)],
#             drawn_mods_list,
#             drawn_mods_rmse
#         ), columns = ['n_mods', 'drawn_mods', 'rmse'])
#         return(out)

#     out_mid = pd.concat([f(individual_mods = individual_mods, 
#                            draw_mods = i,
#                            n_iter = n_iter, 
#                            p = p
#                            ) for i in range(2, (n_mods))], 
#                            axis = 0)
#     # all models ----
#     all_reps_rmses = [
#         get_rmse(
#             observed = mod_test['y'], 
#             predicted= weighted_sum_cols(
#                 mod_list = individual_mods,
#                 mod_weights = [1/n_mods for i in range(n_mods)],
#                 df = mod_test) )]

#     all_reps_rmses = pd.DataFrame(zip(
#         [n_mods for i in range(n_mods)],
#         [individual_mods for i in range(n_mods)],
#         all_reps_rmses), columns = ['n_mods', 'drawn_mods', 'rmse'])

#     out = pd.concat([individual_rmses,
#                      out_mid,
#                      all_reps_rmses])
#     out = out.reset_index().drop(columns = 'index')
#     return(out)

# +
# # best few models
# save_path = '../output/SFigure1_Data_RMSE_Ens_SelectMods.csv'

# if os.path.exists(save_path):
#     temp = pd.read_csv(save_path)
# else:
#     temp = resample_model_averages(
#         individual_mods = ['bglr_'+str(i) for i in range(10)] + ['cat_'+str(i) for i in range(10)],
#         n_iter = 500#,
#         #     p = uniform_by_type_weights
#         )

# temp.to_csv(save_path, index = False)
# temp.head()
# # px.box(temp, x = 'n_mods', y = 'rmse')
# px.scatter(temp, x = 'n_mods', y = 'rmse', trendline = 'lowess')
# -

# # Distributions

# +
# Write out useful tables

mod_train.to_csv('../output/01_mod_train.csv')
mod_test.to_csv('../output/01_mod_test.csv')

# scalings 
pd.DataFrame(zip(all_mod_cols,
                 uniform_weights,
                 uniform_by_type_weights,
                 inv_rmse_weights,
                 inv_std_weights,
                 inv_var_weights), 
             columns = ['all_mod_cols', 
                        'uniform_weights', 
                        'uniform_by_type_weights', 
                        'inv_rmse_weights', 
                        'inv_std_weights', 
                        'inv_var_weights']
            ).to_csv('../output/01_mod_weights.csv')

# same calc on test set
pd.DataFrame(zip(all_mod_cols,
                 uniform_weights_test,
                 uniform_by_type_weights_test,
                 inv_rmse_weights_test,
                 inv_std_weights_test,
                 inv_var_weights_test), 
             columns = ['all_mod_cols', 
                        'uniform_weights', 
                        'uniform_by_type_weights', 
                        'inv_rmse_weights', 
                        'inv_std_weights', 
                        'inv_var_weights']
            ).to_csv('../output/01_mod_weights_test.csv')
# -



mod_train_yhats

# +
# get associated enviromental covariates

# trainIndex
# trainGroups


phe_train = phenotype.loc[trainIndex, ['F', 'M', 'ExperimentCode', 'Year', 'GrainYield'] ]
phe_train
# -

plt_train = mod_train_yhats.copy()
plt_train[['y']] = mod_train[['y']]
plt_train = pd.concat([phe_train.reset_index().drop(columns = 'index'), plt_train], axis=1)

plt_train

plt_train.loc[plt_train.cat_2 > 2000, ]

px.scatter(plt_train, x = 'y', y = 'cat_2')













# +
# mod_train[['y']]
# mod_train_yhats[['lm_0']]
# -

plt_train = mod_train_yhats.copy()
plt_train[['y']] = mod_train[['y']]
plt_train.to_csv('./temp_KLd_test.csv')

plt_train



px.scatter(plt_train, x = 'y', y = 'bglr_0')

px.histogram(plt_train, x = 'y')

px.ecdf(mod_train_yhats[['lm_0']])

import scipy

kl_d = scipy.special.kl_div(
    plt_train['y'], 
    plt_train['bglr_0']
)

kl_d

plt_train['y']

plt_train['bglr_0']

x = plt_train['y'].copy()
x1 = plt_train['bglr_0'].copy()

x = x.sort_values()
x1 = x1.sort_values()

# +

x
# -

px.histogram(kl_d)

kl_d = scipy.special.kl_div(
    plt_train['y'], 
    plt_train['rf_0']
)





# ## Meanshift for VCA

from sklearn.cluster import MeanShift

# +
if True:
    G = np.load('../ext_data/kick_et_al_2023/data/processed/G_PCA_1.npy')
    pRef = pd.read_csv('../ext_data/kick_et_al_2023/data/processed/tensor_ref_phenotype.csv')
    uniqG = pRef.Pedigree.drop_duplicates().index
    uniqGnotna = [True if e == 0 else False for e in np.isnan(G[uniqG,]).sum(axis = 1) ]
    min_idx = uniqG[uniqGnotna]
    G_filt = G[min_idx, :]

    bws = [100+(5*i) for i in range(21)]
    ncols = len(bws)

if not os.path.exists('../output/01_MeanShift.npy'):
    out_np = np.zeros((3199, ncols))

    def try_band(bw = 2):
        tic = time.time()
        G_clustering = MeanShift(bandwidth=bw).fit(G_filt)
        toc = time.time()
        print(toc - tic)
        print(G_clustering.labels_.max())
        return(G_clustering)

    for i in range(len(bws)):
        print(str(i)+"/"+str(len(bws)))
        out = try_band(bw = bws[i])
        out_np[:, i] = out.labels_

    with open('../output/01_MeanShift.npy', 'wb') as f:
        np.save(f, out_np)

    out_np.tofile('../output/01_MeanShift.csv', sep = ',')
    GMeanShifts = out_np
else:
    GMeanShifts = np.load('../output/01_MeanShift.npy')
# -

pd.DataFrame(zip(bws, GMeanShifts.max(axis = 0)))

# +
# bind mean shift clusters into df to be accessed elsewhere
temp = pd.DataFrame(GMeanShifts).set_axis(['bw'+str(e) for e in bws], axis = 1)
temp.index = min_idx

temp = temp.join(pRef.loc[:, ['Pedigree', 'F', 'M']], how = 'left')

temp = temp.loc[:, ['Pedigree', 'F', 'M'] + [e for e in list(temp) if e not in ['Pedigree', 'F', 'M']]]
temp.to_csv('../output/01_G_Clusters_MeanShift.csv')
# -







# +
from sklearn.cluster import OPTICS

if True:
    G = np.load('../ext_data/kick_et_al_2023/data/processed/G_PCA_1.npy')
    pRef = pd.read_csv('../ext_data/kick_et_al_2023/data/processed/tensor_ref_phenotype.csv')
    uniqG = pRef.Pedigree.drop_duplicates().index
    uniqGnotna = [True if e == 0 else False for e in np.isnan(G[uniqG,]).sum(axis = 1) ]
    min_idx = uniqG[uniqGnotna]
    G_filt = G[min_idx, :]

    #min_samps = np.linspace(100, 1000, 900)
    min_samps =[1]+[5+(5*i) for i in range(19)]+[100+(50*i) for i in range(19)]
    ncols = len(min_samps)

if not os.path.exists('../output/01_OPTICS.npy'):
    out_np = np.zeros((3199, ncols))

    def try_band(min_samp = 2):
        tic = time.time()
        G_clustering = OPTICS(min_samples=min_samp).fit(G_filt)
        toc = time.time()
        print(toc - tic)
        print(G_clustering.labels_.max())
        return(G_clustering)

    for i in range(len(min_samps)):
        print(str(i)+"/"+str(len(min_samps)))
        out = try_band(min_samp = min_samps[i])
        out_np[:, i] = out.labels_

    with open('../output/01_OPTICS.npy', 'wb') as f:
        np.save(f, out_np)

    out_np.tofile('../output/01_OPTICS.csv', sep = ',')
    GOPTICS = out_np
else:
    GOPTICS = np.load('../output/01_OPTICS.npy')

# +
# bind OPTICS clusters into df to be accessed elsewhere
temp = pd.DataFrame(GOPTICS).set_axis(['min_samp'+str(e) for e in min_samps], axis = 1)
temp.index = min_idx

temp = temp.join(pRef.loc[:, ['Pedigree', 'F', 'M']], how = 'left')

temp = temp.loc[:, ['Pedigree', 'F', 'M'] + [e for e in list(temp) if e not in ['Pedigree', 'F', 'M']]]
temp.to_csv('../output/01_G_Clusters_OPTICS.csv')
# -





















temp = pd.DataFrame(GMeanShifts).set_axis(['i'+str(i) for i in range(GMeanShifts.shape[1])], axis=1).reset_index()
temp = pd.wide_to_long(temp, stubnames='i', 
                       i = 'index', 
                       j = 'ith_bandwidth'
                      ).reset_index()
temp.head()

tempG = pd.DataFrame(G_filt[:, 0:3]).set_axis(['PC'+str(i+1) for i in range(3)], axis=1).reset_index()

tempG = tempG.merge(temp)
tempG.head()

import plotly.express as px
px.scatter_3d(tempG, x = 'PC1', y= 'PC2', z = 'PC3', color = 'i', animation_frame='ith_bandwidth')















import pandas as pd
import numpy as np
import featuretools as ft
import seaborn as sns; sns.set()
from matplotlib import pyplot
from importlib import import_module
import json
import sys
import copy

class SEM:
    """
    Singleton SEM class. It's an adapter to use the R lavaan package for `Structural Equation Modeling`.
    """
    class __SEM:
        def __init__(self):
            self.robj = import_module('rpy2.robjects')
            self.rinterface = import_module('rpy2.rinterface')

        def load_data(self, data_file):
            """
            Loads data.
            """
            if data_file.endswith(".xlsx"):
                self.data_file = data_file
                self.data = pd.read_excel(data_file)
            elif data_file.endswith(".csv"):
                self.data_file = data_file
                self.data = pd.read_csv(data_file, sep="\t")
            else:
                raise Exception("Unknown extension for the data file: {}".format(data_file.split(".")[-1]))
            self.data = self.data.fillna(0)

        def auto_build(self, model_description):
            columns = []
            if isinstance(model_description["factors"], dict):
                factors_dict = model_description["factors"]
                for k, v in factors_dict.items():
                    columns.extend(v)
            else:
                columns.extend(model_description["factors"])

            # print("Data Columns: ", self.data.columns.tolist())
            # print("Extract Columns: ", columns)
            factors_df = self.data[columns]
            factors_df["customer_id"] = list(range(self.data.shape[0]))
            es = ft.EntitySet(id = 'customer_experience_entity')
            es = es.entity_from_dataframe(entity_id = 'c_id', dataframe = factors_df, index = 'customer_id')
            features, feature_names = ft.dfs(entityset = es, target_entity = 'c_id', max_depth = 2, verbose=True)
            feature_matrix_enc, features_enc = ft.encode_features(features, feature_names)

            original_factors = set(feature_matrix_enc.columns.tolist())

            feature_matrix_enc = feature_matrix_enc.dropna(axis=1)

            after_naelimination = set(feature_matrix_enc.columns.tolist())

            print("Dropped columns with na: ", list(original_factors - after_naelimination))

            feature_matrix_enc = feature_matrix_enc.loc[:, (feature_matrix_enc != 0).any(axis=0)]

            after_allzeros = set(feature_matrix_enc.columns.tolist())

            print("Dropped columns with all zeros: ", after_naelimination - after_allzeros)

            # print(feature_matrix_enc.head())
            # print("Original Columns: ", columns)
            # print("Generated Columns: ", feature_matrix_enc.columns.tolist())
            corr_matrix = feature_matrix_enc.corr()
            corr_matrix = corr_matrix.dropna(axis=1, how='all')
            corr_matrix = corr_matrix.dropna(axis=0, how='all')

            print("Dropped columns with na in correlation matrix: ", list(after_naelimination - set(corr_matrix.columns.tolist())))
            feature_matrix_enc = feature_matrix_enc[corr_matrix.columns.tolist()]

            for it in range(10):
                willdropped = set([])
                corr_matrix = feature_matrix_enc.corr()
                cols = corr_matrix.columns.tolist()
                for i in range(len(cols)):
                    row = cols[i]
                    if row in willdropped:
                        pass
                    for j in range(i+1, len(cols)):
                        col = cols[j]
                        if col in willdropped:
                            pass
                        val = corr_matrix[row][col]
                        if np.abs(val) > 0.95:
                            print("{} , {} = {}".format(row, col, val))
                            willdropped.add(col)
                if len(list(willdropped)) == 0:
                    break
                print("Iteration: ", it+1, " Highly correlated columns have been dropped!: ", list(willdropped))
                feature_matrix_enc = feature_matrix_enc.drop(columns=list(willdropped))

            correlation_matrix = feature_matrix_enc.corr()
            covariance_matrix = feature_matrix_enc.cov()
            cond_number = np.linalg.cond(correlation_matrix.values)
            print("Condition number: {}".format(cond_number))

            copy_model = copy.deepcopy(model_description)
            current_columns = feature_matrix_enc.columns.tolist()

            def replace_marks(s):
                s = s.replace("=", "equals")
                s = s.replace(".", "dot")
                s = s.replace(",", "comma")
                return s


            current_columns = ["_".join(replace_marks(c).split(" ")) for c in current_columns]
            feature_matrix_enc.columns = current_columns
            print("Cols: ", current_columns)

            if isinstance(copy_model["factors"], dict):
                factors_dict = copy_model["factors"]
                new_factors_dict = {}
                for k, v in factors_dict.items():
                    newv = []
                    for c in v:
                        replace = list(filter(lambda x : x.startswith("_".join(replace_marks(c).split(" "))), current_columns))
                        newv.extend(replace)
                    if len(newv) > 0:
                        new_factors_dict[k] = newv
                    else:
                        raise Exception("Latent variable {} has been dropped! Rearrange your initial model description.".format(k))
                copy_model["factors"] = new_factors_dict
            else:
                newv = []
                for c in copy_model["factors"]:
                    replace = list(filter(lambda x : x.startswith("_".join(replace_marks(c).split(" "))), current_columns))
                    newv.extend(replace)
                if len(newv) > 0:
                    copy_model["factors"] = newv
                else:
                    raise Exception("All loading factors have been dropped! Rearrange your initial model description.")

            others = []

            others.extend(copy_model["observations"])
            copy_model["observations"] = ["_".join(replace_marks(c).split(" ")) for c in copy_model["observations"]]
            if isinstance(copy_model["kpis"], dict):
                kpis_dict = copy_model["kpis"]
                for k, v in kpis_dict.items():
                    others.extend(v)
                    copy_model["kpis"][k] = ["_".join(replace_marks(c).split(" ")) for c in v]
            else:
                others.extend(copy_model["kpis"])
                copy_model["kpis"] = ["_".join(replace_marks(c).split(" ")) for c in copy_model["kpis"]]

            feature_matrix_enc = feature_matrix_enc.reset_index(inplace=False).drop("customer_id", axis=1)

            others_df = self.data[others]
            current_columns = ["_".join(replace_marks(c).split(" ")) for c in others_df.columns]
            others_df.columns = current_columns

            feature_matrix_enc = pd.concat([feature_matrix_enc, others_df], axis=1)
            feature_matrix_enc.to_csv("/tmp/autodata.csv", sep="\t", index=False)
            print(feature_matrix_enc.head())

            model = sem.build_model(copy_model, "auto_model")
            result = sem.fit_model("/tmp/autodata.csv", model, "auto_model", verbose="FALSE")
            return result


            #a4_dims = (48, 32)
            #fig, ax = pyplot.subplots(figsize=a4_dims)
            #corrfig = sns.heatmap(ax=ax, data=correlation_matrix)
            #pyplot.show()

            #fig, ax = pyplot.subplots(figsize=a4_dims)
            #corrfig = sns.heatmap(ax=ax, data=covariance_matrix)
            #pyplot.show()

            #fig, ax = pyplot.subplots(figsize=a4_dims)
            #normalized = (feature_matrix_enc - feature_matrix_enc.mean()) / (feature_matrix_enc.max() - feature_matrix_enc.min())
            #corrfig = sns.heatmap(ax=ax, data=normalized.cov())
            #pyplot.show()


        def build_model(self, model_description, model_name):
            """
            Builds model from the model description.
            model_description : {factors : ..., observations : ..., kpis : ..., latent_connections : ...}
            """

            model = "{} <- \n\"".format(model_name)

            #grouped factors
            if isinstance(model_description["factors"], dict):
                factors_dict = model_description["factors"]
                for k, v in factors_dict.items():
                    model += "{} =~ ".format(k)
                    for i in range(len(v)):
                        model += v[i]
                        if i != len(v) - 1:
                            model += " + "
                        else:
                            model += "\n"
            else:
                factors_dict = {"factors" : model_description["factors"]}
                model = "factors =~ "
                factors = factors_dict["factors"]
                for i in range(len(factors_dict)):
                    model += factors[i]
                    if i != len(factors_dict) - 1:
                        model += " + "
                    else:
                        model += "\n"
                model_description["latent_connections"].append("factors", "cx", "~")

            observations = model_description["observations"]
            model += "cx =~ "
            for i in range(len(observations)):
                model += observations[i]
                if i != len(observations) - 1:
                    model += " + "
                else:
                    model += "\n"

            if isinstance(model_description["kpis"], dict):
                kpis_dict = model_description["kpis"]
                for k, v in kpis_dict.items():
                    model += "{} =~ ".format(k)
                    for i in range(len(v)):
                        model += v[i]
                        if i != len(v) - 1:
                            model += " + "
                        else:
                            model += "\n"
            else:
                kpis_dict = {"kpis" : model_description["kpis"]}
                kpis = kpis_dict["kpis"]
                for i in range(len(kpis)):
                    model += kpis[i]
                    if i != len(kpis) - 1:
                        model += " + "
                    else:
                        model += " ~ cx\n"

            for (source, target, connection_type) in model_description["latent_connections"]:
                model += "{} {} {}\n".format(source, connection_type, target)

            model += "\""

            return model

        def fit_model(self, data_file, model, model_name, verbose="FALSE"):
            rscript = '''library("lavaan")

            data <- read.csv(file="{}", header=TRUE, sep="\t")
            {}
            {} = tryCatch(
            {{
              {} <- sem({}, data, std.lv = TRUE, std.ov = TRUE,
                        control = list(maxit = 100000),
                        estimator = "ULS",
                        meanstructure = TRUE,
                        optim.method = "BFGS",
                        verbose={})
              c("OK")
            }}, warning = function(w) {{
              c("WARNING", w)
            }}, error = function(e) {{
              c("ERROR", e)
            }}
            )
            '''.format(data_file, model, "result_" + model_name, "fit_" + model_name, model_name, verbose)
            self.robj.r(rscript)
            result = self.robj.globalenv["result_" + model_name]
            return result


        def evaluate(self, model_name):
            rscript = '''
            {} <- fitmeasures({}, c("npar", "chisq", "df", "cfi", "gfi", "rmsea", "srmr"))
            '''.format("fits_" + model_name, "fit_" + model_name)
            self.robj.r(rscript)
            values = self.robj.globalenv["fits_" + model_name]
            metrics = {"number_of_parameters" : values[0],
                       "chi_square" : values[1],
                       "degree_of_freedom" : values[2],
                       "comparative_fit_index" : values[3],
                       "goodness_of_fit_index" : values[4],
                       "root_mean_square_of_approximation" : values[5],
                       "standardized_root_mean_square_residual" : values[6]}
            return metrics


        def save_model_vis(self, model_name, file_name, file_type="pdf"):
            rscript = '''
                library("semPlot")
                semPaths({}, "std", nCharNodes = 35, layout="tree",
                    intercepts = FALSE, pastel = TRUE, residuals = FALSE, label.prop = 0.92, width = 40, height = 30,
                    sizeMan = 7, sizeLat = 8, font = 4, fade=FALSE, reorder=FALSE, filetype="{}", filename="{}")
            '''.format("fit_" + model_name, file_type, file_name)
            self.robj.r(rscript)


    __instance = None
    def __init__(self):
        raise Exception("SEM is a singleton. Use the `get_instance` method")

    @classmethod
    def get_instance(cls):
        if cls.__instance is None:
            cls.__instance = cls.__SEM()
        return cls.__instance

if __name__ == "__main__":
    sem = SEM.get_instance()

    model_description = {
        "factors" : {"dijital_kanal" : ["hata_islem_sayisi", "islem_ortalama_tiklama", "gezdigi_sayfa_sayisi", "basarili_giris_sayisi", "cevrim_ici_sure"],
                     "sosyal_medya" : ["begeni", "takip", "pozitif_yorum", "negatif_yorum", "sosyal_medya_reklamlari"]},
        "observations" : ["memnuniyet", "tavsiye"],
        "kpis" : {"buyume" : ["capraz_satis"], "elde_tutma" : ["devam_eden_musteri", "terk"], "geri_kazanma" : ["aktiflestirilmis_musteri", "geri_kazanilmis_musteri"]},
        "latent_connections" : [("cx", "dijital_kanal", "~"), ("cx", "sosyal_medya", "~"), ("buyume", "cx", "~"), ("elde_tutma", "cx", "~"),
                                ("elde_tutma", "buyume", "~"), ("geri_kazanma", "cx", "~"), ("geri_kazanma", "elde_tutma", "~")]
    }

    model_description2 = {
        "factors" : {"dijital_kanal" : ["hata_islem_sayisi", "islem_ortalama_tiklama", "cevrim_ici_sure", "basarili_giris_sayisi", "gezdigi_sayfa_sayisi"],
                     "sosyal_medya" : ["begeni", "takip", "pozitif_yorum", "negatif_yorum", "sosyal_medya_reklamlari"]},
        "observations" : ["memnuniyet", "tavsiye"],
        "kpis" : ["capraz_satis", "devam_eden_musteri", "terk", "aktiflestirilmis_musteri", "geri_kazanilmis_musteri"],
        "latent_connections" : [("cx", "dijital_kanal", "~"), ("cx", "sosyal_medya", "~")]
    }

    model_description3 = {
        "factors" : {"dijital_kanal" : ["hata_islem_sayisi", "islem_ortalama_tiklama", "cevrim_ici_sure", "basarili_giris_sayisi", "gezdigi_sayfa_sayisi"],
                     "sosyal_medya" : ["begeni", "takip", "pozitif_yorum", "negatif_yorum", "sosyal_medya_reklamlari"],
                     "klasik_kanal" : ["sube_kullanim", "sube_sure", "atm_kullanim", "personel_ilgisi"]},
        "observations" : ["memnuniyet", "tavsiye"],
        "kpis" : {"buyume" : ["capraz_satis"], "elde_tutma" : ["devam_eden_musteri", "terk"], "geri_kazanma" : ["aktiflestirilmis_musteri", "geri_kazanilmis_musteri"]},
        "latent_connections" : [("cx", "dijital_kanal", "~"), ("cx", "sosyal_medya", "~"), ("buyume", "cx", "~"), ("elde_tutma", "cx", "~"),
                                ("elde_tutma", "buyume", "~"), ("geri_kazanma", "cx", "~"), ("geri_kazanma", "elde_tutma", "~")]
    }

    model_airline = {
        "factors" : {"profile" : ["age", "gender"],
                     "flight_profile" : ["airline_status", "price_sensitivity", "year_of_first_flight",
                                         "no_of_flights_pa", "percentage_of_flight_with_other_airlines", "no_of_other_loyalty_cards"],
                     "travel" : ["type_of_travel", "class", "day_of_month", "airline_name", "origin_city", "destination_city"],
                     "timing" : ["scheduled_departure_hour", "departure_delay_minutes", "arrival_delay_minutes",
                                 "flight_cancelled", "flight_time_minutes", "flight_distance"]},
        "observations" : ["satisfaction"],
        "kpis" : {"spending" : ["shopping_amount_at_airport", "eating_and_drinking_at_airport"]},
        "latent_connections" : [("cx", "profile", "~"), ("cx", "flight_profile", "~"), ("cx", "travel", "~"), ("cx", "timing", "~"),
                                ("spending", "cx", "~")]
    }

    #sem.load_data("../data/airline.xlsx")
    #sem.auto_build(model_airline)

    sem.load_data("../data/generated.csv")
    sem.auto_build(model_description3)



    #model = sem.build_model(model_description, "random_model")
    #print(model)
    #result = sem.fit_model("../data/generated.csv", model, "random_model", verbose="FALSE")
    #if result[0] == "OK":
    #    metrics = sem.evaluate("random_model")
    #    print(metrics)
    #    sem.save_model_vis("random_model", "../models/random_model")
    #else:
    #    print(result)
    #    print(result[0][0])
    #    print(result[1][0])

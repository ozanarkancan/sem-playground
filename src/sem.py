import pandas as pd
import numpy as np
import featuretools as ft
import seaborn as sns; sns.set()
from matplotlib import pyplot
from importlib import import_module
import json
import sys

class SEM:
    class __SEM:
        def __init__(self):
            self.robj = import_module('rpy2.robjects')
            self.rinterface = import_module('rpy2.rinterface')

        def load_data(self, data_file):
            if data_file.endswith(".xlsx"):
                self.data_file = data_file
                self.data = pd.read_excel(data_file)
            elif data_file.endswith(".csv"):
                self.data_file = data_file
                self.data = pd.read_csv(data_file, sep="\t")
            else:
                raise Exception("Unknown extension for the data file: {}".format(data_file.split(".")[-1]))

        def build_model(self, model_description, model_name):
            """
            model_description : {factors : ..., observations : ..., kpis : ..., latent_connections : ...}
            """

            model = "{} <- \n'".format(model_name)

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
                factors = list(factors_dict.values())
                for i in range(len(factors_dict)):
                    model += factors[i]
                    if i != len(factors_dict) - 1:
                        model += " + "
                    else:
                        model += "\n"

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
                kpis = list(kpis_dict.values())
                for i in range(len(kpis_dict)):
                    model += kpis[i]
                    if i != len(kpis_dict) - 1:
                        model += " + "
                    else:
                        model += " ~ cx\n"

            for (source, target, connection_type) in model_description["latent_connections"]:
                model += "{} {} {}\n".format(source, connection_type, target)

            model += "'"

            return model

        def fit_model(self, data_file, model, model_name, verbose="FALSE"):
            rscript = '''library("lavaan")

            data <- read.csv(file="{}", header=TRUE, sep="\t")
            {}
            {} = tryCatch(
            {{
              {} <- sem({}, data, std.lv = FALSE, std.ov = FALSE,
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
        "factors" : {"dijital_kanal" : ["hata_islem_sayisi", "islem_ortalama_tiklama", "cevrim_ici_sure", "basarili_giris_sayisi", "gezdigi_sayfa_sayisi"],
                     "sosyal_medya" : ["begeni", "takip", "pozitif_yorum", "negatif_yorum", "sosyal_medya_reklamlari"]},
        "observations" : ["memnuniyet", "tavsiye"],
        "kpis" : {"buyume" : ["capraz_satis"], "elde_tutma" : ["devam_eden_musteri", "terk"], "geri_kazanma" : ["aktiflestirilmis_musteri", "geri_kazanilmis_musteri"]},
        "latent_connections" : [("cx", "dijital_kanal", "~"), ("cx", "sosyal_medya", "~"), ("buyume", "cx", "~"), ("elde_tutma", "cx", "~"),
                                ("elde_tutma", "buyume", "~"), ("geri_kazanma", "cx", "~"), ("geri_kazanma", "elde_tutma", "~")]
    }

    model = sem.build_model(model_description, "random_model")
    print(model)
    result = sem.fit_model("../data/generated.csv", model, "random_model", verbose="FALSE")
    if result[0] == "OK":
        metrics = sem.evaluate("random_model")
        print(metrics)
    else:
        print(result)
        print(result[0][0])
        print(result[1][0])
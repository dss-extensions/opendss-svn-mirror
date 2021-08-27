# -*- coding: utf-8 -*-
# @Time    : 8/9/2021 12:10 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : process.py
# @Software: PyCharm

import py_dss_interface
import pathlib
import os
from Methods import SmartInverterFunction
import pandas as pd


def set_baseline():
    dss.text("New Energymeter.m1 Line.ln5815900-1 1")
    dss.text("Set Maxiterations=20")
    dss.text("set maxcontrolit=100")
    dss.text("Batchedit Load..* daily=default")


def set_time_series_simulation():
    dss.text("set controlmode=Static")
    dss.text("set mode=daily")
    dss.text("set number=24")
    dss.text("set stepsize=1h")


def get_energymeter_results():
    dss.meters_write_name("m1")
    feeder_kwh = dss.meters_register_values()[0]
    feeder_kvarh = dss.meters_register_values()[1]
    loads_kwh = dss.meters_register_values()[4]
    losses_kwh = dss.meters_register_values()[12]
    pv_kwh = loads_kwh + losses_kwh - feeder_kwh

    return feeder_kwh, feeder_kvarh, loads_kwh, losses_kwh, pv_kwh


smart_inverter_functions_list = ["unity-pf", "pf", "volt-var"]

script_path = os.path.dirname(os.path.abspath(__file__))
dss_file = pathlib.Path(script_path).joinpath("Feeders", "8500-Node", "Master.dss")

dss = py_dss_interface.DSSDLL()

bus = "l3104830"

feeder_kwh_list = list()
feeder_kvarh_list = list()
loads_kwh_list = list()
losses_kwh_list = list()
pv_kwh_list = list()

for smart_inverter_function in smart_inverter_functions_list:
    # Process for each smart inverter function
    dss.text(f"Compile [{dss_file}]")
    set_baseline()
    set_time_series_simulation()

    # Add PV system and the smart inverter function
    SmartInverterFunction(dss, bus, 12.47, 8000, 8000, smart_inverter_function)

    dss.text("solve")

    # Read Energymeter results
    energymeter_results = get_energymeter_results()
    feeder_kwh_list.append(energymeter_results[0])
    feeder_kvarh_list.append(energymeter_results[1])
    loads_kwh_list.append(energymeter_results[2])
    losses_kwh_list.append(energymeter_results[3])
    pv_kwh_list.append(energymeter_results[4])

# Save results in a csv file
dict_to_df = dict()
dict_to_df["smart_inverter_function"] = smart_inverter_functions_list
dict_to_df["feeder_kwh"] = feeder_kwh_list
dict_to_df["feeder_kvarh"] = feeder_kvarh_list
dict_to_df["loads_kwh"] = loads_kwh_list
dict_to_df["losses_kwh"] = losses_kwh_list
dict_to_df["pv_kwh"] = pv_kwh_list

df = pd.DataFrame().from_dict(dict_to_df)

output_file = pathlib.Path(script_path).joinpath("outputs", "results.csv")
df.to_csv(output_file, index=False)



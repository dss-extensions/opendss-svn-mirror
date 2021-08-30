# -*- coding: utf-8 -*-
# @Time    : 8/25/2021 12:54 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : example6.py
# @Software: PyCharm

import os
import pathlib
import py_dss_interface
import math

script_path = os.path.dirname(os.path.abspath(__file__))

dss_file = os.path.join(pathlib.Path(script_path), "dss_files", "example6.dss")

dss = py_dss_interface.DSSDLL()

dss.text(f"compile [{dss_file}]")

p_dc_list = list()
v_pu_list = [1.015, 1.025, 1.05, 1.075, 1.09, 1.095]
p_ac_list = list()
v_list = list()

# DC power available of 75
for v_pu in v_pu_list:
    dss.vsources_write_name("source")
    dss.vsources_write_pu(v_pu)
    dss.pvsystems_write_name("PV")
    dss.pvsystems_write_irradiance(0.75)
    dss.solution_solve()

    dss.circuit_set_active_element("PVSystem.PV")

    p_dc_list.append(75)
    pv_p_ac = round(-sum(dss.cktelement_powers()[0:7:2]), 2)
    pv_v_pu = round(dss.cktelement_voltages_mag_ang()[0] / (13800.0 / math.sqrt(3)), 3)

    p_ac_list.append(pv_p_ac)
    v_list.append(pv_v_pu)

# DC power available of 100
for v_pu in v_pu_list:
    dss.vsources_write_name("source")
    dss.vsources_write_pu(v_pu)
    dss.pvsystems_write_name("PV")
    dss.pvsystems_write_irradiance(1)
    dss.solution_solve()

    dss.circuit_set_active_element("PVSystem.PV")

    p_dc_list.append(100)
    pv_p_ac = round(-sum(dss.cktelement_powers()[0:7:2]), 2)
    pv_v_pu = round(dss.cktelement_voltages_mag_ang()[0] / (13800.0 / math.sqrt(3)), 3)

    p_ac_list.append(pv_p_ac)
    v_list.append(pv_v_pu)


print(f"DC power kW: {p_dc_list}")
print(f"Active power kW: {p_ac_list}")
print(f"Voltage pu: {v_list}")

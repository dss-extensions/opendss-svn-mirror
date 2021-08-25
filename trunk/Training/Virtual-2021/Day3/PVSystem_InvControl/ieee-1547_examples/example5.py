# -*- coding: utf-8 -*-
# @Time    : 8/25/2021 12:30 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : example5.py
# @Software: PyCharm

import os
import pathlib
import py_dss_interface
import math

script_path = os.path.dirname(os.path.abspath(__file__))

dss_file = os.path.join(pathlib.Path(script_path), "dss_files", "example5.dss")

dss = py_dss_interface.DSSDLL()

dss.text(f"compile [{dss_file}]")

p_pu_list = [0, 0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.28]
p_ac_list = list()
q_ac_list = list()
v_list = list()

# Voltage at 0.95
for p_pu in p_pu_list:
    dss.vsources_write_name("source")
    dss.vsources_write_pu(0.95)
    dss.pvsystems_write_name("PV")
    dss.pvsystems_write_irradiance(p_pu)
    dss.solution_solve()

    dss.circuit_set_active_element("PVSystem.PV")

    pv_p_ac = round(-sum(dss.cktelement_powers()[0:7:2]), 2)
    pv_q_ac = round(-sum(dss.cktelement_powers()[1:8:2]), 2)
    pv_v_pu = round(dss.cktelement_voltages_mag_ang()[0] / (13800.0 / math.sqrt(3)), 3)

    p_ac_list.append(pv_p_ac)
    q_ac_list.append(pv_q_ac)
    v_list.append(pv_v_pu)

# Voltage at 1.075
for p_pu in p_pu_list:
    dss.vsources_write_name("source")
    dss.vsources_write_pu(1.085)
    dss.pvsystems_write_name("PV")
    dss.pvsystems_write_irradiance(p_pu)
    dss.solution_solve()

    dss.circuit_set_active_element("PVSystem.PV")

    pv_p_ac = round(-sum(dss.cktelement_powers()[0:7:2]), 2)
    pv_q_ac = round(-sum(dss.cktelement_powers()[1:8:2]), 2)
    pv_v_pu = round(dss.cktelement_voltages_mag_ang()[0] / (13800.0 / math.sqrt(3)), 3)

    p_ac_list.append(pv_p_ac)
    q_ac_list.append(pv_q_ac)
    v_list.append(pv_v_pu)

print(f"Voltage pu: {v_list}")
print(f"Active power kW: {p_ac_list}")
print(f"Reactive power kvar: {q_ac_list}")
# -*- coding: utf-8 -*-
# @Time    : 8/25/2021 12:05 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : example3.py
# @Software: PyCharm

import os
import pathlib
import py_dss_interface
import math

script_path = os.path.dirname(os.path.abspath(__file__))

dss_file = os.path.join(pathlib.Path(script_path), "dss_files", "example3.dss")

dss = py_dss_interface.DSSDLL()

dss.text(f"compile [{dss_file}]")

v_pu_list = [0.915, 0.925, 0.95, 0.975, 0.985, 1.015, 1.025, 1.05, 1.075, 1.085]
p_ac_list = list()
q_ac_list = list()
v_list = list()

for v_pu in v_pu_list:
    dss.vsources_write_name("source")
    dss.vsources_write_pu(v_pu)
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

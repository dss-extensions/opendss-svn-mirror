# -*- coding: utf-8 -*-
# @Time    : 8/25/2021 12:00 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : example2.py
# @Software: PyCharm

import os
import pathlib
import py_dss_interface
import math

script_path = os.path.dirname(os.path.abspath(__file__))

dss_file = os.path.join(pathlib.Path(script_path), "dss_files", "example2.dss")

dss = py_dss_interface.DSSDLL()

dss.text(f"compile [{dss_file}]")

p_pu_list = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
p_ac_list = list()
q_ac_list = list()
pf_list = list()

for p_pu in p_pu_list:
    dss.pvsystems_write_name("PV")
    dss.pvsystems_write_irradiance(p_pu)
    dss.solution_solve()

    dss.circuit_set_active_element("PVSystem.PV")

    pv_p_ac = round(-sum(dss.cktelement_powers()[0:7:2]), 2)
    pv_q_ac = round(-sum(dss.cktelement_powers()[1:8:2]), 2)

    if pv_p_ac > 0:
        pf = round(pv_q_ac / abs(pv_q_ac) * pv_p_ac / math.sqrt(pv_p_ac ** 2 + pv_q_ac ** 2), 2)
    else:
        pf = 0

    p_ac_list.append(pv_p_ac)
    q_ac_list.append(pv_q_ac)
    pf_list.append(pf)

print(f"DC power kW: {[p_pu * 100 for p_pu in p_pu_list]}")
print(f"Active power kW: {p_ac_list}")
print(f"Reactive power kvar: {q_ac_list}")
print(f"Power Factor: {pf_list}")
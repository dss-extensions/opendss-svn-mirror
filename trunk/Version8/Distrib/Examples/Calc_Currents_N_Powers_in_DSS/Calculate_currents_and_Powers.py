"""This example demonstrates how OpenDSS calculates currents flowing through elements
using their Y primitives and voltage at their terminals. Then, these currents can be
used for calculating the active and reactive powers for the same element.

This is demonstrated for a simple distribution model and contrasted with the reports in OpenDSS
for users to learn and understand this concept.

by Davis Montenegro - 10/10/2025
"""
# coding: utf-8

# In[1]:


import win32com.client
import pandas as pd
import numpy as np
import math

#import tkmessagebox
def create_energy_meter_fh(DSSObj):
    """Crates an energy meter at the feeder head. Note: Limite to one VSource per ckt, modify it as required"""

    DSSCircuit  = DSSObj.ActiveCircuit
    DSSText     = DSSObj.Text
    DSSVSrc     = DSSCircuit.VSources 
    DSSBus      = DSSCircuit.ActiveBus

    VSIdx           = DSSVSrc.First       # activates the first VSource
    # Get the name of the bus the VSource is connected
    DSSText.Command = f'? VSource.{DSSVSrc.name}.bus1'
    VS_bus          = str(DSSText.Result).split(".")[0]
    DSSCircuit.SetActiveBus(VS_bus)
    VS_PDE          = list(DSSBus.AllPDEatBus)[0]
    DSSText.Command = f'new energymeter.fh element={VS_PDE} terminal=1'
    print('\t ---- Energy Meter created at the feeder head')

def get_linear_currents_element(DSSObj, Elm):
    """ Calculates the linear currents (Yprim * Volts) for the given DSS Element (Elm)"""
    DSSCircuit  = DSSObj.ActiveCircuit
    DSSCktElm   = DSSCircuit.ActiveCktElement

    DSSCircuit.SetActiveElement(Elm)
    myV_list    = list(DSSCktElm.Voltages)
    myV_cmplx   = []
    arr_len     = int((len(myV_list)/2))
    # 1. fix the arrays coming from DSS to match complex numbers
    for idx in range(arr_len):
        myV_cmplx.append(complex(myV_list[idx * 2], myV_list[(idx * 2) + 1]))
    myV_cmplx   = np.array(myV_cmplx)       # To Numpy

    # 2. Adjust the Y primitive matrix to match a complex matrix as well
    Yprim_list  = list(DSSCktElm.Yprim)
    Yprim       = []
    arr_len     = int((len(Yprim_list)/2))

    for idx in range(arr_len):
        Yprim.append(complex(Yprim_list[idx * 2], Yprim_list[(idx * 2) + 1])) 
    # reshape the matrix
    Yprim       = np.array(Yprim)       # To Numpy 
    m_size      = int(math.sqrt(arr_len))
    Yprim       = Yprim.reshape(m_size, m_size)  

    Elm_amps_c  = np.matmul(Yprim,myV_cmplx)
    print('\t ---- Linear currents calculated.')

    return Elm_amps_c

def add_non_linearities_PCE(DSSObj, Elm, Elm_amps_c):
    """Adds non-linearities to the given linear currents (Elm_amps_c), it is expected for the given element (Elm) to be a PCE"""

    DSSCircuit  = DSSObj.ActiveCircuit
    DSSCktElm   = DSSCircuit.ActiveCktElement
    DSSText     = DSSObj.Text
    
    DSSCircuit.SetActiveElement(Elm)
    DSSText.Command = 'get InjCurrent'
    str_array       = str(DSSText.Result).replace('[','').replace(']','').replace('i','j').split(',')
    nl_cmplx_arr    = []
    for str_cmplx in str_array:
        nl_cmplx_arr.append(complex(str_cmplx))
    Elm_num_phases  = DSSCktElm.NumPhases
    Elm_amps_c = Elm_amps_c[:Elm_num_phases] - np.array(nl_cmplx_arr)
    print('\t ---- Non linearities added for PCE.')

    return Elm_amps_c

def add_vsource_curr_injections(DSSObj, VSrc, Elm_amps_c):
    """Adds the currents injected by the given VSource (VSrc) to the given linear currents, this
    is required given that VSources are Norton equivalents, demanding adding the natural and forced
    components of the model for calculating the current at the device terminals."""

    DSSCircuit  = DSSObj.ActiveCircuit
    DSSCktElm   = DSSCircuit.ActiveCktElement
    # Get all the Y currents, it is expected that the first n (n = number of phaes of VSource) are the ones for
    # the VSource. If there are other PCE conected to the same bus, this code needs to be updated to reflect that
    
    DSSCircuit.SetActiveElement(VSrc)
    total_amps  = Elm_amps_c[:(DSSCktElm.NumPhases)]
    myYCurr     = list(DSSCircuit.YCurrents)[:DSSCktElm.NumPhases * 2]
    VS_Y_curr   =   []
    arr_len     = int((len(myYCurr)/2))
    for idx in range(arr_len):
        VS_Y_curr.append(complex(myYCurr[idx * 2], myYCurr[(idx * 2) + 1]))     
    total_amps = total_amps - np.array(VS_Y_curr)
    print('\t ---- Current injection added for VSource Norton equivalent.')

    return total_amps

def get_Elm_voltages_cmplx(DSSObj):
    DSSCircuit  = DSSObj.ActiveCircuit
    DSSCktElm   = DSSCircuit.ActiveCktElement

    Elm_voltages = DSSCktElm.Voltages
    Elm_Nphases  = DSSCktElm.NumPhases

    cmplx_voltages = []
    for i in range(Elm_Nphases):
        idx = i * 2
        cmplx_voltages.append(complex(Elm_voltages[idx], Elm_voltages[idx + 1]))

    return cmplx_voltages

def get_DSS_currents_p_element(model_path:str=''):
    """Prints on the screen the currents per element for the given model after a snapshot run"""

    # Initialize OpenDSS (late binding)
    DSSObj      = win32com.client.dynamic.Dispatch("OpenDSSEngine.DSS")
    DSSText     = DSSObj.Text
    DSSCircuit  = DSSObj.ActiveCircuit
    DSSSolution = DSSCircuit.Solution
    DSSVSrc     = DSSCircuit.VSources
    DSSMeter    = DSSCircuit.Meters
    DSSCktElm   = DSSCircuit.ActiveCktElement
    DSSObj.Start(0)

    DNumActors = 6
    print('Simulation started')
    DSSText.Command = 'ClearAll'
    DSSText.Command = f'compile "{model_path}"'
    # Create an Energy meter to facilitate navigating elements
    create_energy_meter_fh(DSSObj)
    DSSText.Command = 'set maxiterations=50 maxcontroliter=50' 
    DSSSolution.Solve     
    # Refresh the bus distribution to fall within the EM scope  
    DSSText.Command='ReprocessBuses' 
    DSSText.Command='Show powers elem'

    obj_types = ['PDE', 'PCE', 'VSource']
    # Navigate the circuit to get the currents per element
    DSSMeter.First          # Activates the first (and holpefully only) energy meter
    for local_idx in obj_types:
        if local_idx == 'PDE':
            Elm_list = DSSMeter.AllBranchesInZone
        elif local_idx == 'PCE':
            Elm_list = DSSMeter.ZonePCE
        else:
            DSSVSrc.First
            Elm_list = ['VSource.' + DSSVSrc.Name]

        for Elm in Elm_list:
            if Elm != '':
                Elm_amps_c  = get_linear_currents_element(DSSObj, Elm)

                if local_idx == 'PCE':
                    # This means we are working with PCE and need to add the non linearities
                    Elm_amps_c  = add_non_linearities_PCE(DSSObj, Elm, Elm_amps_c)
                elif local_idx == 'VSource':
                    Elm_amps_c  = add_vsource_curr_injections(DSSObj, Elm, Elm_amps_c)

                Elm_voltages = get_Elm_voltages_cmplx(DSSObj)
                
                Elm_kVA      = []
                for j in range(DSSCktElm.NumPhases):
                    Elm_kVA.append((Elm_voltages[j] * Elm_amps_c[j].conjugate()) / 1e3)
                
                print(f"Powers for asset: {DSSCktElm.Name}")
                Elm_powers = {}
                Elm_powers['kW'] = [num.real for num in Elm_kVA]
                Elm_powers['kvar'] = [num.imag for num in Elm_kVA]

                print(pd.DataFrame(Elm_powers))

if __name__  == '__main__':

    ckt_model = "C:/Program Files/OpenDSS/IEEETestCases/4Bus-DY-Bal/4Bus-DY-Bal.DSS"
    get_DSS_currents_p_element(ckt_model)



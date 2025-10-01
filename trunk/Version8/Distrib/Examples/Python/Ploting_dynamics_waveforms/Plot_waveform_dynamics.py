
"""This example demonstrates how to create sinusoidal projections of polar data for reproducing a time-domain equivalent using
sinusoidal bases. 

By Davis Montenegro
10/01/2025"""

import win32com.client
import os
import pandas as pd
import matplotlib.pyplot as plt
import math

def project_polar_data(setup:dict = {}, 
                       mag_list:list=list(),
                       ang_list:list=list()):
    """Projects the given list of polar points into its time-domain waveform using starndard sinusoidal bases.
    This is explained at https://theses.hal.science/tel-01260398/file/MONTENEGRO-MARTINEZ_2015_archivage.pdf or
    https://www.amazon.com/Diakoptics-based-actors-simulating-applications/dp/365982058X """

    time_inc    = 0
    sample_time = 1 / setup['sample freq']
    freq_rad    = setup['frequency'] * 2 * math.pi
    iter_count  = 0
    Waveform    = []
    time_vector = []
    for mag in mag_list:
        time_cell   = (iter_count + 1) * setup['step size']
        cell_wave   = []    # Stores the waveform for the time step
        while time_inc < time_cell:
            sin_arg     = time_inc * freq_rad + (ang_list[iter_count] * math.pi / 180)  
            cell_wave.append(math.sin(sin_arg) * mag)
            time_inc    += sample_time
            time_vector.append(time_inc)
        Waveform    += cell_wave
        iter_count  += 1

    return time_vector, Waveform

def run_simulation_dynamics():
    """Runs the simulation, adds BESS and creats a microgrid. Right after that, 
    captures the data from the BESS and plots a waveform projected voltage"""
    # Initialize OpenDSS (late binding)
    DSSObj      = win32com.client.dynamic.Dispatch("OpenDSSEngine.DSS")
    DSSText     = DSSObj.Text
    DSSCircuit  = DSSObj.ActiveCircuit
    DSSSolution = DSSCircuit.Solution
    DSSMon      = DSSCircuit.Monitors
    DSSObj.Start(0)

    current_directory = os.path.abspath(__file__)
    folder_path, file_name = os.path.split(current_directory)

    print('Simulation started')
    DSSText.Command='ClearAll'
    DSSText.Command=f'compile "{folder_path}/IEEE123Master.dss"' 
    DSSText.Command='set maxiterations=50 maxcontroliter=50'  # not needed but just in case 
    DSSSolution.Solve                      

    # Adds the battery, forms the microgrid and solves for 200 ms
    DSSText.Command = 'redirect add_BESS_and_solve_dyn.dss'

    # Creates the plot config dictionary
    wfrm_setup = {}
    wfrm_setup['step size']     = DSSSolution.StepSize
    wfrm_setup['frequency']     = DSSSolution.Frequency
    wfrm_setup['sample freq']   = DSSSolution.Frequency * 100 # for a good resolution 

    DSSText.Command = 'export monitor stovi'

    # Get the monitor data
    DSSMon.Name     = 'stovi'
    plt.figure(1)
    plt.title("Voltage waveform in time")
    colors          = ['blue', 'red', 'green']
    for channel in range(3):
        time_vector, Waveform   = project_polar_data( wfrm_setup,
                                    DSSMon.Channel((channel * 2) + 1), 
                                    DSSMon.Channel((channel * 2) + 2)
                                    )
        plt.plot(time_vector, Waveform, color=colors[channel], label=f'phase {channel + 1}')
    
    plt.xlabel("Time (s)")
    plt.ylabel("Amplitude")
    plt.legend()
    plt.show()

if __name__ == "__main__":

    run_simulation_dynamics()    

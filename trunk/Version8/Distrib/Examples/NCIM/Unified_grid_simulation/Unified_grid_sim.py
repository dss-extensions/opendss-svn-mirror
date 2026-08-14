
# coding: utf-8

# In[1]:


import win32com.client
from time import process_time
from pathlib import Path
import pandas as pd
import plotly.graph_objects as go

# Initialize OpenDSS (late binding)
DSSObj = win32com.client.dynamic.Dispatch("OpenDSSEngine.DSS")
DSSText = DSSObj.Text
DSSCircuit = DSSObj.ActiveCircuit
DSSSolution = DSSCircuit.Solution

DSSObj.Start(0)

def get_meter_vpu(DSSobj:object={}, EM:str='', time_step:float = 0.0):
   """
   Returns the voltages in pu only for the buses within the are covered by the given energy meter.

   Args:
      DSSobj      : The active DSS object in memory.
      EM  (str)   : Name of the energy meter of interes
      time_step   : The actual time step for which the samples will be taken.
   
   Returns:
      A data frame with the samples including the columns time step, phase, bus name, vpu
   """
   DSSCircuit     = DSSobj.ActiveCircuit
   DSSMeter       = DSSCircuit.Meters
   DSSCktElm      = DSSCircuit.ActiveCktElement
   DSSBus         = DSSCircuit.ActiveBus
   DSSMeter.Name  = EM

   bus_name        = []
   bus_vpu         = []
   bus_phase       = []

   all_pde     = DSSMeter.AllBranchesInZone

   for pde in all_pde:
      DSSCircuit.SetActiveElement(pde)
      buses    = DSSCktElm.BusNames
      for bus in buses:
         bus_n       = bus.split('.')[0]
         if bus_n not in bus_name:
            DSSCircuit.SetActiveBus(bus)
            bus_nodes   = DSSBus.Nodes
            bus_vpus    = DSSBus.PuVMagAngle
            b_idx       = 0
            for node in bus_nodes:
               bus_phase.append(node)
               bus_vpu.append(bus_vpus[b_idx * 2])
               bus_name.append('Bus ' + bus)
               b_idx += 1

   result = {
               'time step' : time_step,
               'phase'     : bus_phase, 
               'bus name'  : bus_name, 
               'vpu'       : bus_vpu}
   
   return pd.DataFrame(result)

def get_generators_vpu(DSSobj:object={}, time_step:float = 0.0):
   """
   Returns the voltages in pu only for the generators across the model.

   Args:
      DSSobj      : The active DSS object in memory.
      time_step   : The actual time step for which the samples will be taken.
   
   Returns:
      A data frame with the samples including the columns time step, phase, bus name, vpu
   """
   DSSCircuit     = DSSobj.ActiveCircuit
   DSSGen         = DSSCircuit.Generators
   DSSCktElm      = DSSCircuit.ActiveCktElement
   DSSBus         = DSSCircuit.ActiveBus
   

   bus_name        = []
   bus_vpu         = []
   bus_phase       = []

   all_gen     = DSSGen.AllNames

   for pde in all_gen:
      DSSCircuit.SetActiveElement(f"Generator.{pde}")
      buses    = DSSCktElm.BusNames
      for bus in buses:
         bus_n       = bus.split('.')[0]
         if bus_n not in bus_name:
            DSSCircuit.SetActiveBus(bus)
            bus_nodes   = DSSBus.Nodes
            bus_vpus    = DSSBus.PuVMagAngle
            b_idx       = 0
            for node in bus_nodes:
               bus_phase.append(node)
               bus_vpu.append(bus_vpus[(node - 1) * 2])
               bus_name.append('Bus ' + bus)

   result = {
               'time step' : time_step,
               'phase'     : bus_phase, 
               'bus name'  : bus_name, 
               'vpu'       : bus_vpu}
   
   return pd.DataFrame(result)

def get_generators_vars(DSSobj:object={}, time_step:float = 0.0):
   """
   Returns the vars only for the generators across the model.

   Args:
      DSSobj      : The active DSS object in memory.
      time_step   : The actual time step for which the samples will be taken.
   
   Returns:
      A data frame with the samples including the columns time step, phase, bus name, vpu
   """
   DSSCircuit     = DSSobj.ActiveCircuit
   DSSGen         = DSSCircuit.Generators
   DSSCktElm      = DSSCircuit.ActiveCktElement
   DSSBus         = DSSCircuit.ActiveBus
   

   bus_name        = []
   bus_vpu         = []
   bus_phase       = []

   all_gen     = DSSGen.AllNames

   for pde in all_gen:
      DSSCircuit.SetActiveElement(f"Generator.{pde}")
      buses    = DSSCktElm.BusNames
      for bus in buses:
         bus_n       = bus.split('.')[0]
         if bus_n not in bus_name:
            DSSCircuit.SetActiveBus(bus)
            bus_nodes   = DSSBus.Nodes
            bus_vpus    = DSSCktElm.Powers
            b_idx       = 0
            for node in bus_nodes:
               bus_phase.append(node)
               bus_vpu.append(bus_vpus[b_idx + 1] / 1e3)
               bus_name.append('Bus ' + bus)
               b_idx += 1

   result = {
               'time step' : time_step,
               'phase'     : bus_phase, 
               'bus name'  : bus_name, 
               'vpu'       : bus_vpu}
   
   return pd.DataFrame(result)

def plot_v_profile_daily(vpu_df:pd.DataFrame = pd.DataFrame(), Title:str='', Yaxis:str='Voltage distribution (pu)'):
    """
    Generates and displays the voltage profile graph using the given vprofile data frame.

    Args:
        vpu_df : Voltage profile data frame, includes time step, phase, bus name , vpu
        Title  : Title for the plot, if not given, it defaults to 'Voltage distribution across the circuit'
        Yaxis  : Label for Y axis, if not provided defaults to 'Voltage distribution (pu)'
    
    Returns:
        No returning data
    """
    # First, check if the df is not empty
    if not vpu_df.empty:
    # example on how to plot the graph (html)
        print('Plotting voltage profile, please wait...')
        v_times = list(set(vpu_df['time step']))
        c_idx   = 0
        fig     = go.Figure()
        for v_time in v_times:
            local_df = vpu_df[(vpu_df['time step'] == v_time)]
            fig.add_trace(go.Box(
                y           = local_df['vpu'],
                name        = str(v_time),
                fillcolor   = 'white',
                line_color  = 'black'
                )
            )
        
        max_vpu = max(vpu_df['vpu'])
        min_vpu = sorted(set(vpu_df['vpu']))
        ymax    = 1.055
        if max_vpu > 1.05:
            ymax = max_vpu *1.01
        
        ymin    = 0.945
        min_idx = 0
        if min_vpu[0] == 0:
            min_idx = 1

        if min_vpu[min_idx] < 0.95:
            ymin = min_vpu[min_idx] * 0.99

        if Title == '':
            Title = 'Voltage distribution across the circuit'
        fig.update_layout(
            title           = Title,
            xaxis_title     = 'Time (h)',
            yaxis_title     = Yaxis,
            width           = 1200,
            height          = 700,
            yaxis_range     = [ymin, ymax]
        )
        print('Plot completed.')

        fig.show()
        print('Plot displayed.')
    else:
        print('The provided data frame is empty, action cancelled')


cwd = Path(__file__).resolve().parent

print('Simulation started')
DSSText.Command='ClearAll'

print(f'Compiling "{cwd}\\master_file.dss"')
DSSText.Command   = f'compile "{cwd}\\master_file.dss"' 
DSSText.Command   = 'set maxiterations=50 maxcontroliter=100' 
l_profile = [0.821183712,
0.871244048,
0.866454685,
0.881348406,
0.883592733,
0.89051824,
0.886362691,
0.919704262,
0.998476941,
1.030003248,
1.013864512,
1.018224202,
1.021123123,
1.019877498,
1.00223521,
0.965844362,
0.962526768,
0.952690726,
0.927134621,
0.898045779,
0.866562256,
0.858712614,
0.816362566,
0.799998298,
]

DSSSolution.Solve                       # Solves Actor 1
tic         = process_time()  # Gets the initial time
t_iter      = []
vpu_data    = pd.DataFrame() # Stores the voltages in pu within the distribution model at each times tep
gen_vpu     = pd.DataFrame() # Stores the voltages for all the generators across the model (Tx)
gen_vars    = pd.DataFrame() # Stores the vars for all generators across the model (Tx)
for time, i in enumerate(l_profile):
   DSSText.Command =f'set loadmult={i} time=({time},0)' 
   DSSText.Command ='solve'
   DSSText.Command ='sample'
   t_iter.append(DSSSolution.Totaliterations)
   step_vpu       = get_meter_vpu(DSSObj, 'sub', time + 1)
   gen_step_vpu   = get_generators_vpu(DSSObj, time + 1)
   get_step_vars  = get_generators_vars(DSSObj, time + 1)
   if vpu_data.empty:
      vpu_data = step_vpu
   else:
      vpu_data = pd.concat([vpu_data, step_vpu], ignore_index=True)

   if gen_vpu.empty:
      gen_vpu = gen_step_vpu
   else:
      gen_vpu = pd.concat([gen_vpu, gen_step_vpu], ignore_index=True)

   if gen_vars.empty:
      gen_vars = get_step_vars
   else:
      gen_vars = pd.concat([gen_vars, get_step_vars], ignore_index=True)

DSSText.Command = 'export monitors all' 
plot_v_profile_daily(vpu_data, f'Voltage distribution across circuit 5')
plot_v_profile_daily(gen_vpu, f'Voltage distribution across the generators in transmission')
plot_v_profile_daily(gen_vars, f'vars distribution across the generators in transmission', 'vars distribution (kvar)')

toc = process_time() # Gets the final time
# Publish results
print('Total time required (s): ')
print(toc-tic)


import csv
with open(f'{cwd}\\num_iterations.csv', 'w', newline='',encoding='utf-8') as f:
   writer = csv.writer(f)
   for item in t_iter:
      writer.writerow(str(item))

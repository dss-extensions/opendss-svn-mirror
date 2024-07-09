"""
"Benchmarking Test Systems for Harmonics Modeling and Simulation against Open Source and Commercial Tools"

Script to reproduce published results in IEEE PES GM 2024.
Authors: Celso Rocha, Andres Ovalle, Robert Arritt, Roger Dugan and Krishnat Patil

Instructions:
    - set harmonics_cancelling_case to True to reproduce results from Table IX and Table X.
    - set harmonics_cancelling_case to True to reproduce results from Table XI.
"""

import win32com.client
from win32com.client import makepy
import sys
import pathlib
import pandas as pd
import numpy as np

harmonics_cancelling_case = True


def get_dfBuses(dssCircuit):

    dic_to_df = {}
    bus_name = list()
    bus_kVbase_list = list()

    for i in range(len(list(dssCircuit.AllBusNames))):
        dssCircuit.SetActiveBusi(i)
        bus_name.append(dssBus.Name)
        bus_kVbase_list.append(dssBus.kVBase)

    dic_to_df["name"] = bus_name
    dic_to_df["kVbase LN"] = bus_kVbase_list

    dfBuses = pd.DataFrame().from_dict(dic_to_df)

    dfBuses["kVbase LL"] = dfBuses["kVbase LN"] * (3.0 ** 0.5)
    dfBuses['ref_paper_name'] = dfBuses['name'].str.split('_').apply(lambda value: value[0] if len(value) == 2 else np.nan)
    dfBuses.set_index('name', inplace=True, drop=True)

    srAux = pd.Series(dssCircuit.AllBusVolts,
                      index=pd.MultiIndex.from_product([dssCircuit.AllNodeNames, ["Mag", "Angle"]],
                                                       names=["NodeName", "Quantity"]))
    srMag = np.sqrt(srAux.loc[slice(None), "Mag"] ** 2 + srAux.loc[slice(None), "Angle"] ** 2)
    srAng = np.arctan2(srAux.loc[slice(None), "Angle"], srAux.loc[slice(None), "Mag"]) * 180 / np.pi
    srAux = pd.DataFrame(data=zip(srMag, srAng), index=dssCircuit.AllNodeNames, columns=["Mag", "Angle"])

    # Convert to pu
    buses_order = srAux.index.str.split(".").str.get(0)
    srVBases = dfBuses.loc[buses_order]["kVbase LN"] * 1000.0
    srAux["Mag"] = srAux["Mag"] / srVBases.values

    srAux = srAux.loc[srAux.index.str.split('.').str.get(1) == '1']
    srAux.index = srAux.index.str.split('.').str.get(0)

    dfBuses[['Mag_DSS', 'Angle_DSS']] = srAux[['Mag', 'Angle']]

    return dfBuses


def place_monitors_vTHD(dssCircuit, dssBus, dssText, lisBuses):

    for bus in lisBuses:

        dssCircuit.SetActiveBus(bus)
        lisPDE = dssBus.AllPDEatBus

        # Find a PDE connected to this element to place monitor.
        for pde in lisPDE:

            if pde.split('.')[0].lower() == 'transformer':
                dssText.Command =f"? {pde}.buses"
                bus1 = dssText.Result.strip('[]').split(',')[0]
            else:
                dssText.Command = f"? {pde}.bus1"
                bus1 = dssText.Result

            if bus1 == bus:
                terminal = 1
            else:
                terminal = 2

            dssText.Command = f"New Monitor.{bus} element={pde} terminal={terminal} mode=0"
            break


def place_monitors_cTHD(dssText, dssTransformer, dssLine, transformers=True, lines=True, lisElementsFullName=[]):

    if transformers:
        lisElementsFullName += [f"transformer.{name}" for name in dssTransformer.AllNames]

    if lines:
        lisElementsFullName += [f"line.{name}" for name in dssLine.AllNames]

    for elem in lisElementsFullName:

        for term in [1, 2]:
            dssText. Command = f"New Monitor.{elem.replace('.', '_')}_Term{term} element={elem} terminal={term} mode=0"


def get_vTHD(dssCircuit, dssMonitor, dssText):

    dicTHD = dict()
    lisBuses = dssCircuit.AllBusNames
    # Export Monitors
    for bus in dssMonitor.AllNames:

        if bus in lisBuses:

            dssText.Command = f"export monitor {bus}"
            path = dssText.Result
            dfMon = pd.read_csv(path)
            dicTHD[bus] = (dfMon.iloc[1:][' V1'].apply(lambda x: x ** 2).sum()) ** 0.5 / dfMon.iloc[0][' V1'] * 100.0

    return dicTHD


def get_cTHD(dssCircuit, dssMonitor, dssText):

    lisMonNames = []
    lisTHD_Term1 = []
    lisTHD_Term2 = []

    lisBuses = dssCircuit.AllBusNames
    # Export Monitors
    for mon in dssMonitor.AllNames:

        if mon in lisBuses:
            continue

        dssText.Command = f"export monitor {mon}"
        path = dssText.Result
        dfMon = pd.read_csv(path)
        cTHD = (dfMon.iloc[1:][' I1'].apply(lambda x: x ** 2).sum()) ** 0.5 / dfMon.iloc[0][' I1'] * 100.0

        mon_name = '_'.join(mon.split("_")[:-1])

        dssText.Command = f"? monitor.{mon}.terminal"

        terminal = dssText.Result

        if mon_name.lower() == 'TCR_Spectrum'.lower():
            continue

        if terminal == '1':
            lisTHD_Term1 += [cTHD]
        else:
            lisTHD_Term2 += [cTHD]

        if mon_name not in lisMonNames:
            lisMonNames += [mon_name]

    dic = {'THD(%)_Left': lisTHD_Term1,
           'THD(%)_Right': lisTHD_Term2}

    df = pd.DataFrame.from_dict(dic)
    df.index = lisMonNames

    return df


# Create a new instance of the DSS
sys.argv = ["makepy", "OpenDSSEngine.DSS"]
makepy.main()
dssObj = win32com.client.Dispatch("OpenDSSEngine.DSS")


if dssObj.Start(0) is False:
    sys.exit("OpenDSS has failed to start!")
else:
    # Assign a variable to each of the interfaces for easier access
    dssText = dssObj.Text
    dssCircuit = dssObj.ActiveCircuit
    dssSolution = dssCircuit.Solution
    dssBus = dssCircuit.ActiveBus
    dssTransformer = dssCircuit.Transformers
    dssLine = dssCircuit.Lines
    dssMonitor = dssCircuit.Monitors

# ---------- Paths ----------
main_path = pathlib.Path(__file__).parent.resolve()
model_path = fr"{main_path}\Model\Master_file.dss"

benchmark_harmonics_cancelling_vTHD = fr"{main_path}\Benchmark Results\Paper Reference Results.csv"
benchmark_harmonics_cancelling_cTHD = fr"{main_path}\Benchmark Results\Paper Reference Results_cTHD.csv"

benchmark_NO_harmonics_cancelling_vTHD = fr"{main_path}\Benchmark Results\Paper Reference Results_NoCanceling.csv"
benchmark_NO_harmonics_cancelling_cTHD = fr"{main_path}\Benchmark Results\Paper Reference Results_cTHD_NoCanceling.csv"

# ---------- Importing benchmarking results ----------
if harmonics_cancelling_case:

    vTHD_reference = benchmark_harmonics_cancelling_vTHD
    cTHD_reference = benchmark_harmonics_cancelling_cTHD

else:

    vTHD_reference = benchmark_NO_harmonics_cancelling_vTHD
    cTHD_reference = benchmark_NO_harmonics_cancelling_cTHD

dfBenchmark = pd.read_csv(filepath_or_buffer=vTHD_reference, index_col=0)
dfBenchmark.index = dfBenchmark.index.astype("str")
dfBenchmark_cTHD = pd.read_csv(filepath_or_buffer=cTHD_reference, index_col=0)

# ---------- Compiling base model ----------
dssText.Command = rf"Compile '{model_path}'"
dssText.Command = rf"Set maxiter=100"

# Adding dummy line to track harmonic flow on the primary of the HVDC - bus 3.
dssText.Command = "New line.hvdc_term1 phases=3 bus1=3_bus3 bus2=3_bus3_intermediate switch=True length=0.001"
dssText.Command = "Edit Transformer.3_301_0_1_1 buses=[3_bus3_intermediate, 301_bus301]"
dssText.Command = "Edit Transformer.3_302_0_1_1 buses=[3_bus3_intermediate, 302_bus302]"

# Adding monitor to capture current spectrum injected by OpenDSS where the TCR is placed.
dssText.Command = "New monitor.TCR_Spectrum_Injected element=Load.Load_at_8_1 mode=0"
dssText.Command = "New monitor.TCR_Spectrum_Injected2 element=Load.Load_at_302_1 mode=0"

# ---------- Model Updates/Fixes ----------
dssText.Command = "Edit Load.Load_at_8_1 conn=delta"
dssText.Command = "Edit Transformer.5_6_0_1_1 xhl=25.02"
dssText.Command = "Edit Capacitor.Capa_at9_1 kvar=6330"
dssText.Command = "batchedit transformer..* %loadloss=0.0"

dssText.Command = "Edit line.9_14_1_1 r1=1.679575 r0=1.679575"  # Caught this error in the impedance of the model provided by PSSE

dssText.Command = "Edit Vsource.Vsrc_at_2_1 pu=1.0599158131187874 angle=-3.314153920448652"
dssText.Command = "Edit Vsource.Vsrc_at_6_1 pu=1.1735630125024648 angle=-17.152317213436135"
dssText.Command = "Edit Vsource.source pu=1.168693132879689 angle=31.877852217854276"

# ---------------- Adding more precision to Line Impedances ---------------------
dssText.Command = "Edit line.6_11_1_1  r1=12.5576136   x1=26.30000205 "
dssText.Command = "Edit line.6_12_1_1  r1=16.2469125   x1=33.8235723  "
dssText.Command = "Edit line.6_13_1_1  r1=8.745837975  x1=17.22422678 "
dssText.Command = "Edit line.9_10_1_1  r1=4.206700575  x1=11.17287675 "
dssText.Command = "Edit line.9_14_1_1  r1=1.679575     x1=35.75070608 "
dssText.Command = "Edit line.10_11_1_1 r1=10.8479385   x1=25.395174   "
dssText.Command = "Edit line.12_13_1_1 r1=29.20944915  x1=26.42997735 "
dssText.Command = "Edit line.13_14_1_1 r1=22.60070505  x1=46.01625525 "

dssText.Command = "Edit line.1_2_1_1 r1=10.2480525  x1=31.294053    c1=264.727722  "
dssText.Command = "Edit line.1_5_1_1 r1=28.5745698  x1=117.9675819  c1=246.6901618 "
dssText.Command = "Edit line.2_3_1_1 r1=24.8452785  x1=104.7101013  c1=219.6338215 "
dssText.Command = "Edit line.2_4_1_1 r1=30.7341594  x1=93.2522787   c1=187.5375746 "
dssText.Command = "Edit line.2_5_1_1 r1=30.1142772  x1=91.9625238   c1=169.7652726 "
dssText.Command = "Edit line.3_4_1_1 r1=35.4432645  x1=90.4528107   c1=173.478888  "
dssText.Command = "Edit line.4_5_1_1 r1=7.06065822  x1=22.2657687   c1=64.19249371 "

dssText.Command = "Edit Load.Load_at_8_1 kW=0.0 kvar=12900"

# --------- Simulation Settings ---------
dssText.Command = "set tolerance=0.00001"
dssText.Command = "batchedit load..* vmaxpu=1.1 vminpu=0.8"

if harmonics_cancelling_case:
    dssText.Command = "Edit Load.Load_at_302_1 conn=wye"
else:
    dssText.Command = "Edit Load.Load_at_302_1 conn=delta"

dssText.Command = "set tol=0.0001"
dssText.Command = "batchedit transformer..* ppm_antifloat=0"

# Adding high resistances to ground delta
dssText.Command = "New Reactor.Test R=1000000 X=0 bus1=302_bus302"
dssText.Command = "New Reactor.Test3 R=1000000 X=0 bus1=301_bus301"
dssText.Command = "New Reactor.Test2 R=1000000 X=0 bus1=8_bus8"

dssText.Command = rf"Solve mode=snap"

# ---------- Prepare DataFrame ----------
dfBuses = get_dfBuses(dssCircuit)

# ---------- Place Monitors to Record Voltage Distortion ----------
lisBuses = dfBuses.loc[~pd.isna(dfBuses['ref_paper_name'])].index.values.tolist()
place_monitors_vTHD(dssCircuit, dssBus, dssText, lisBuses)

# ---------- Place Monitors to Record Harmonic Currents ----------
place_monitors_cTHD(dssText, dssTransformer, dssLine, transformers=True, lines=True,
                    lisElementsFullName=['capacitor.capa_at9_1',
                                         'Reactor.Filter_at9_2ndHarm_L',
                                         'Reactor.Filter_at9_5thHarm_L',
                                         'Reactor.Filter_at9_7thHarm_L',
                                         'Reactor.Filter_at9_11thHarm_L',
                                         'Reactor.Filter_at3_11thHarm_1_L',
                                         'Reactor.Filter_at3_11thHarm_2_L'])

dssText.Command = "Solve mode = snap"  # Need to solve again to add meters

if not dssSolution.Converged:
    sys.exit("Power flow solution didn't converge for when solving base power flow.")

# ---------- Edits before going into harmonics mode ----------
dssText.Command = "redirect loads_exact_RX.dss"

dssText.Command = "batchedit Load..* enabled=no"
dssText.Command = "edit Load.load_at_8_1 enabled=yes"  # Re-enabling loads that have a spectrum
dssText.Command = "edit Load.load_at_301_1 enabled=yes"
dssText.Command = "edit Load.load_at_302_1 enabled=yes"
dssText.Command = "set neglectloady=True"  # Neglect Loads Yprim from Ysystem

dssText.Command = "solve mode=harmonics"

if not dssSolution.Converged:
    sys.exit("Harmonics mode didn't converge for when solving in harmonics mode.")

# Export Monitors
dssText.Command = f"set datapath='{pathlib.Path(model_path).parent.joinpath('monitor').resolve()}'"
dfBuses['THD_DSS'] = pd.Series(get_vTHD(dssCircuit, dssMonitor, dssText))
dfElem = get_cTHD(dssCircuit, dssMonitor, dssText)

# Format DataFrame with Results
# vTHD
dfBuses.set_index('ref_paper_name', inplace=True, drop=True)
dfBuses = dfBuses[dfBuses.index.notnull()]
dfBenchmark.sort_index(inplace=True)
dfBuses.sort_index(inplace=True)

# cTHD
dfBenchmark_cTHD.index = dfBenchmark_cTHD.index.str.lower().str.replace('.', '_')
dfBenchmark_cTHD.sort_index(inplace=True)
dfElem.sort_index(inplace=True)

# Compare THD Results
# vTHD
dfDiff = pd.DataFrame(index=dfBuses.index)
dfDiff['THD (% Error)'] = abs(dfBenchmark['THD (%)'] - dfBuses['THD_DSS']) / dfBenchmark['THD (%)'] * 100.0
dfDiff['LF Voltage (% Error)'] = abs(dfBenchmark['LF Voltage (pu)'] - dfBuses['Mag_DSS']) / dfBenchmark['LF Voltage (pu)'] * 100.0
dfDiff['LF Angle (% Error)'] = abs(abs(dfBenchmark['LF Angle (deg)'] - dfBuses['Angle_DSS']) / dfBenchmark['LF Angle (deg)'] * 100.0)
dfDiff['LF Voltage (Diff pu)'] = abs(dfBenchmark['LF Voltage (pu)'] - dfBuses['Mag_DSS'])
dfDiff['LF Angle (Diff)'] = abs(dfBenchmark['LF Angle (deg)'] - dfBuses['Angle_DSS'])
dfDiff['LF Angle (% Error)'].iloc[0] = 0.0

# cTHD
dfTHD_c = pd.DataFrame(index=dfBenchmark_cTHD.index)
dfTHD_c['cTHD Left DSS'] = dfElem['THD(%)_Left']
dfTHD_c['cTHD Left Paper'] = dfBenchmark_cTHD['THD(%)_Left']
dfTHD_c['cTHD Right DSS'] = dfElem['THD(%)_Right']
dfTHD_c['cTHD Right Paper'] = dfBenchmark_cTHD['THD(%)_Right']
dfTHD_c['cTHD Diff Left'] = dfBenchmark_cTHD['THD(%)_Left'] - dfElem['THD(%)_Left']
dfTHD_c['cTHD Diff Right'] = dfBenchmark_cTHD['THD(%)_Right'] - dfElem['THD(%)_Right']
dfTHD_c['cTHD (% Error) Left'] = abs(dfBenchmark_cTHD['THD(%)_Left'] - dfElem['THD(%)_Left']) / dfBenchmark_cTHD['THD(%)_Left'] * 100.0
dfTHD_c['cTHD (% Error) Right'] = abs(dfBenchmark_cTHD['THD(%)_Right'] - dfElem['THD(%)_Right']) / dfBenchmark_cTHD['THD(%)_Right'] * 100.0

dfDiff.index = dfDiff.index.astype(int)

# LF Prints
dfLF = pd.DataFrame(index=dfBenchmark.index)
dfLF['V (pu) Paper'] = dfBenchmark['LF Voltage (pu)']
dfLF['V (pu) DSS'] = dfBuses['Mag_DSS']
dfLF['Angle (deg) Paper'] = dfBenchmark['LF Angle (deg)']
dfLF['Angle (deg) DSS'] = dfBuses['Angle_DSS']
dfLF['V (pu) % Error'] = abs(dfLF['V (pu) Paper'] - dfLF['V (pu) DSS']) / dfLF['V (pu) Paper'] * 100.0
dfLF['Angle % Error'] = abs(dfLF['Angle (deg) Paper'] - dfLF['Angle (deg) DSS']) / dfLF['Angle (deg) Paper'] * 100.0
dfLF['Angle Diff'] = dfLF['Angle (deg) Paper'] - dfLF['Angle (deg) DSS']

# vTHD Prints
dfvTHD = pd.DataFrame(index=dfBenchmark.index)
dfvTHD['vTHD (%) Paper'] = dfBenchmark['THD (%)']
dfvTHD['vTHD (%) DSS'] = dfBuses['THD_DSS']
dfvTHD['vTHD Diff'] = dfvTHD['vTHD (%) Paper'] - dfvTHD['vTHD (%) DSS']
dfvTHD['vTHD % Error'] = abs(dfvTHD['vTHD (%) Paper'] - dfvTHD['vTHD (%) DSS']) / dfvTHD['vTHD (%) Paper'] * 100.0

if harmonics_cancelling_case is True:
    print(f"{'-'*10} Results for Case with Harmonics Cancellation {'-'*10}")

# LF Prints
print("\nLF:")
dfLF.index = dfLF.index.astype(int)
print(dfLF.sort_index().to_string())

# vTHD Prints
print("\nvTHD")
dfvTHD.index = dfvTHD.index.astype(int)
print(dfvTHD.sort_index().to_string())

# cTHD Prints
print("\ncTHD:")
print(dfTHD_c.sort_index().to_string())


The pyControl object runs under another application called the DSDpyServer. This application requires the following packages installed in your local python installation:

- pywin32 (if Windows)
- numpy

Do this before starting any simulation using pyControl objects (only the first time).

If working on MS Windows:

1. Copy the content of this folder ourside the OpenDSS installation folder. 
2. Paste the content in a folder with writing privileges (e.g. create a foolder within "Documents" or a public folder).
3. Run the executable copies with the other 2 .py scripts. This will register the DSSpyServer for OpenDSS in your system. Use the executable acoordingly to your OS.
4. Ready to go...

If working on Linux (C++)

1. Copy the content of this folder ourside the OpenDSS installation folder. 
2. Paste the content in a folder with writing privileges (e.g. create a foolder within "Documents" or a public folder).
3. Run the python script called Linux_install.py from a terminal within the folder where the DSSpyServer files are. To run this script type: Python3 Linux_install.py
4. Ready to go...
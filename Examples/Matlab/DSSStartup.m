%--------------------------------------------------------------------------
function [Start,Obj,Text] = DSSStartup(mydir)
    % Function for starting up the DSS
    % make sure we are in the proper directory
    cd(mydir);
    %
    %instantiate the DSS Object
    Obj = actxserver('OpenDSSEngine.DSS');
    %
    %Start the DSS.   Only needs to be executed the first time w/in a
    %Matlab session
    Start = Obj.Start(0);

    % Define the text interface
    Text = Obj.Text;    
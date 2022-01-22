(* ::Package:: *)

BeginPackage["NotebookAutoSave`"]; 


$NotebookAutoSaveOnSchedule::usage = "When this is set to False, auto save is suspended."; 


$NotebookAutoSaveScheduledTask::usage = "The schedule task used to perform autosave. Do not manipulate this object directly; instead, use the functions provided by this package."


InstallNotebookAutoSaveScheduledTask::usage = "Call this function to create and start the auto save task for the current session. Calling this more that once has no effect."


RemoveNotebookAutoSaveScheduledTask::usage "Call this function to stop and remove the auto save task for the current session. Calling this more than once has no effect."


Begin["`Private`"]; 


$NotebookAutoSaveOnSchedule = True;


InputCellSelectedQ[nb_NotebookObject] := Length@Cells[NotebookSelection@InputNotebook[],CellStyle->{"Input"}] == 1;


NotebookHasFileQ[nb_NotebookObject] :=
    Quiet[Check[!FailureQ[NotebookFileName[SelectedNotebook[]]], False
        ]]; 


NotebookModifiedQ[nb_NotebookObject] :=
    Lookup[Association @@ NotebookInformation[nb], "ModifiedInMemory"
        ]; 


SaveSelectedNotebook[] :=
    With[{nb = SelectedNotebook[], inb = InputNotebook[]},
        Module[{saved = False},
            If[nb == inb && NotebookModifiedQ[nb] && NotebookHasFileQ[nb] && !InputCellSelectedQ[nb],
                (FrontEndTokenExecute["Save"]; saved = True)
            ]; saved
        ]
    ]; 


InstallNotebookAutoSaveScheduledTask[] :=
    If[MissingQ[SelectFirst[ScheduledTasks[], #["TaskUUID"] == $NotebookAutoSaveScheduledTask[
        "TaskUUID"]&]],
        $NotebookAutoSaveScheduledTask =
            SessionSubmit[
                ScheduledTask[
                    If[$NotebookAutoSaveOnSchedule,
                        SaveSelectedNotebook[]
                    ]
                    ,
                    Quantity[10, "Seconds"]
                ]
            ]
    ]; 


RemoveNotebookAutoSaveScheduledTask[] :=
    If[!MissingQ[SelectFirst[ScheduledTasks[], #["TaskUUID"] == $NotebookAutoSaveScheduledTask[
        "TaskUUID"]&]],
        TaskRemove[$NotebookAutoSaveScheduledTask]
    ]; 


End[]; 


EndPackage[]; 

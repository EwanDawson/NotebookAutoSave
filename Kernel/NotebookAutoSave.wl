(* ::Package:: *)

BeginPackage["EwanDawson`NotebookAutoSave`"]; 


$NotebookAutoSaveOnSchedule::usage = "When this is set to False, auto save is suspended."; 


$NotebookAutoSaveScheduledTask::usage = "The schedule task used to perform autosave. Do not manipulate this object directly; instead, use the functions provided by this package."


InstallNotebookAutoSaveScheduledTask::usage = "Call this function to create and start the auto save task for the current session. Calling this more that once has no effect."


RemoveNotebookAutoSaveScheduledTask::usage = "Call this function to stop and remove the auto save task for the current session. Calling this more than once has no effect."


Begin["`Private`"]; 


$NotebookAutoSaveOnSchedule = True;


InputCellSelectedQ[nb_NotebookObject] := Length@Cells[NotebookSelection[nb],CellStyle->{"Input"}] == 1;


SystemNotebookQ[nb_NotebookObject] :=
    Enclose[StringStartsQ[Confirm[Quiet[NotebookFileName[nb]]], $InstallationDirectory
        ], False&];


NotebookHasFileQ[nb_NotebookObject] :=
    Quiet[Check[!FailureQ[NotebookFileName[nb]], False]];


NotebookModifiedQ[nb_NotebookObject] :=
    Lookup[Association @@ NotebookInformation[nb], "ModifiedInMemory"
        ];


SaveSelectedNotebook[] :=
    With[{nb = SelectedNotebook[], inb = InputNotebook[]},
        Module[{saved = False},
            If[nb == inb && !SystemNotebookQ[nb] && NotebookModifiedQ[
                nb] && NotebookHasFileQ[nb] && !InputCellSelectedQ[nb],
                (FrontEndTokenExecute["Save"]; saved = True)
            ];
            saved
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


(* Since this Paclet is set to load at startup, this file will be evaluted when the Kernel is initialized.
   This lets us install the auto-save scheduled task at startup. However, since it relies on symbols that
   are part of the FrontEnd, we must delay initialization until the FrontEnd session is initialized.
   Simply evaluating InstallNotebookAutoSaveScheduledTask[] will fail (and in fact block the Kernel initialization
   process, preventing Wolfram Desktop from being able to start.) *)
InitializationValue[$Initialization,"FrontEndSession"] = Hold[InstallNotebookAutoSaveScheduledTask[]];

program RegenixServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fMain, uCore, Configuration, dMain, uniqueinstanceraw
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  if ( not InstanceRunning('RegenixServerUniqueInstance') ) then
  begin
    Application.Initialize;
    Core.Initialize;

    Application.CreateForm(TfmMain, fmMain);
    Application.CreateForm(TdtMain, dtMain);

    Core.PostInitialize;
    Application.Run;
    Core.Finalize;
  end;
end.


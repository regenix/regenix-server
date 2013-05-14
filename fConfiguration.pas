unit fConfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, ButtonPanel, StdCtrls;

type

  { TfmConfiguration }

  TfmConfiguration = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbRunProgram: TCheckBox;
    cbStartServer: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    procedure cbRunProgramChange(Sender: TObject);
    procedure cbRunProgramMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmConfiguration: TfmConfiguration;

implementation


{$R *.lfm}

{ TfmConfiguration }

procedure TfmConfiguration.cbRunProgramMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  cbStartServer.Enabled := TCheckBox(Sender).Checked;
end;

procedure TfmConfiguration.FormShow(Sender: TObject);
begin

end;

procedure TfmConfiguration.cbRunProgramChange(Sender: TObject);
begin
  cbStartServer.Enabled := TCheckBox(Sender).Checked;
end;

end.


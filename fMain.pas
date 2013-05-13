unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, CheckLst, ButtonPanel, Buttons, UTF8Process;

type

  { TfmMain }
  TfmMain = class(TForm)
    Bevel1: TBevel;
    btnRestartServer: TBitBtn;
    btnStartServer: TBitBtn;
    btnExit: TBitBtn;
    btnStartServer2: TBitBtn;
    btnStopServer: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    labMongoDBVersion1: TLabel;
    labNginxVersion: TLabel;
    labPHPVersion: TLabel;
    labMongoDBVersion: TLabel;
    Shape1: TShape;
    StatusBar: TStatusBar;
    procedure btnExitClick(Sender: TObject);
    procedure btnRestartServerClick(Sender: TObject);
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses uCore;

procedure TfmMain.btnStartServerClick(Sender: TObject);
begin
  btnStartServer.Enabled := false;
  Core.StartServer;
  btnStopServer.Enabled := true;
  btnRestartServer.Enabled := true;
end;

procedure TfmMain.btnRestartServerClick(Sender: TObject);
begin
  btnRestartServer.Enabled := false;
  Core.RestartServer;
  btnRestartServer.Enabled := true;
end;

procedure TfmMain.btnExitClick(Sender: TObject);
begin
  Core.Exit;
end;

procedure TfmMain.btnStopServerClick(Sender: TObject);
begin
  btnStartServer.Enabled   := true;
  Core.StopServer;
  btnStopServer.Enabled    := false;
  btnRestartServer.Enabled := false;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  labNginxVersion.Caption   := Core.version.Nginx;
  labPHPVersion.Caption     := Core.version.PHP;
  labMongoDBVersion.Caption := Core.version.MongoDB;
end;


{$R *.lfm}

{ TfmMain }



end.


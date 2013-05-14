unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, CheckLst, ButtonPanel, Buttons, UTF8Process, LCLIntf;

type

  { TfmMain }
  TfmMain = class(TForm)
    Bevel1: TBevel;
    btnRestartServer: TBitBtn;
    btnStartServer: TBitBtn;
    btnExit: TBitBtn;
    btnConfiguration: TBitBtn;
    btnStopServer: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    IdleTimer: TIdleTimer;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    labLinkRockMongo: TLabel;
    labPHPInfo: TLabel;
    labMongoDBVersion1: TLabel;
    labNginxVersion: TLabel;
    labPHPInfo1: TLabel;
    labLocalhost: TLabel;
    labPHPVersion: TLabel;
    labMongoDBVersion: TLabel;
    Shape1: TShape;
    StatusBar: TStatusBar;
    procedure btnExitClick(Sender: TObject);
    procedure btnRestartServerClick(Sender: TObject);
    procedure btnConfigurationClick(Sender: TObject);
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure labLinkRockMongoClick(Sender: TObject);
    procedure labLocalhostClick(Sender: TObject);
    procedure labPHPInfo1Click(Sender: TObject);
    procedure labPHPInfoClick(Sender: TObject);
    procedure StatusBarClick(Sender: TObject);
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
end;

procedure TfmMain.btnRestartServerClick(Sender: TObject);
begin
  btnRestartServer.Enabled := false;
  Core.RestartServer;
end;

procedure TfmMain.btnConfigurationClick(Sender: TObject);
begin
  Core.ShowConfiguration;
end;

procedure TfmMain.btnExitClick(Sender: TObject);
begin
  Core.Exit;
end;

procedure TfmMain.btnStopServerClick(Sender: TObject);
begin
  btnStartServer.Enabled   := true;
  Core.StopServer;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  labNginxVersion.Caption   := Core.version.Nginx;
  labPHPVersion.Caption     := Core.version.PHP;
  labMongoDBVersion.Caption := Core.version.MongoDB;
  StatusBar.SimpleText := 'Document root: ' + Core.serverPath + 'nginx/regenix/';
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  if Core.IsCmdParam('-h') then
  begin
     Core.Hide;
  end;
end;

procedure TfmMain.IdleTimerTimer(Sender: TObject);
begin
  if Core.status = ssStarted then
  begin
    labLinkRockMongo.Visible := true;
    labPHPInfo.Visible := true;
    labLocalhost.Visible := true;

    btnStartServer.Enabled   := false;
    btnStopServer.Enabled    := true;
    btnRestartServer.Enabled := true;
  end else
  begin
    labLinkRockMongo.Visible := false;
    labPHPInfo.Visible := false;
    labLocalhost.Visible := false;

    btnStartServer.Enabled   := true;
    btnStopServer.Enabled    := false;
    btnRestartServer.Enabled := false;
  end;
end;

procedure TfmMain.labLinkRockMongoClick(Sender: TObject);
begin
  OpenURL('http://localhost/tools/rockmongo/');
end;

procedure TfmMain.labLocalhostClick(Sender: TObject);
begin
  OpenURL('http://localhost/');
end;

procedure TfmMain.labPHPInfo1Click(Sender: TObject);
begin
  OpenURL('http://regenix.ru/');
end;

procedure TfmMain.labPHPInfoClick(Sender: TObject);
begin
  OpenURL('http://localhost/tools/phpinfo/');
end;

procedure TfmMain.StatusBarClick(Sender: TObject);
begin
  OpenDocument(Core.serverPath + 'nginx/regenix/');
end;


{$R *.lfm}

{ TfmMain }



end.


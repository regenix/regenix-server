unit uCore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms, Configuration, Menus, Dialogs, LCLType,
  process, Graphics, IntfGraphics;

type
  { Core }

  Core = class(TObject)
  private
        constructor Create;
  protected
        class var process: TProcess;
        class var cgiProccess: TProcess;
        class var nginxProcess: TProcess;
        class var mongoProcess: TProcess;

        class var tray: TTrayIcon;
        class var phpVersion: String;

        procedure onCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure onMinimize(Sender: TObject);
        procedure onTrayDblClick(Sender: TObject);
        procedure onTrayClick(Sender: TObject);

        class procedure killProcess(p: TProcess);
  public
        class var status: String;
        class var serverPath: String;
        class var path: String;
        class var cgiPort: Integer;
        class var configuration: TConfiguration;
        class procedure Initialize;
        class procedure PostInitialize;
        class procedure Finalize;

        class procedure Restore;
        class procedure Hide;
        class procedure Exit;

        class procedure SetTrayMenu(Menu: TPopupMenu);
        class procedure SetPHPVersion(Version: String);

        class procedure StartServer;
        class procedure RestartServer;
        class procedure StopServer;

        procedure onMessage(Sender: TObject);
  end;

implementation

uses dMain;

{ Core }

constructor Core.Create;
begin
end;

procedure Core.onCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  Core.Hide;
end;

procedure Core.onMinimize(Sender: TObject);
begin
  Application.MainForm.Hide;
end;

procedure Core.onTrayDblClick(Sender: TObject);
begin
  Core.Restore;
end;

procedure Core.onTrayClick(Sender: TObject);
begin
  Core.Restore;
end;

class procedure Core.killProcess(p: TProcess);
begin
  if ( p.Running ) then
  begin
       process.CommandLine := 'taskkill /F /PID ' + IntToStr(p.ProcessID);
       process.Options     := process.Options + [poNoConsole, poWaitOnExit];
       process.Execute;
  end;
end;

class procedure Core.Initialize;
begin
  serverPath := StringReplace(ExtractFilePath(ExtractFileDir(ParamStr(0))), '\', '/', [rfReplaceAll]);
  path       := StringReplace(ExtractFilePath(ParamStr(0)), '\', '/', [rfReplaceAll]);

  SetCurrentDir(path);

  if not DirectoryExists('conf/') then
     MkDir('conf/');

  configuration := TConfiguration.Create('conf/application.conf');

  phpVersion := '5.4.15';
  cgiPort    := configuration.ReadInteger('main', 'cgi.port', 9001);

  tray := TTrayIcon.Create(Application);
  tray.Icon.LoadFromFile('favicon.ico');
  tray.Show;
  tray.ShowIcon := true;

  // tray.OnDblClick := onTrayDblClick;
  tray.OnClick    := onTrayClick;
  with TStringList.Create do
  begin
    Text := IntToStr(GetProcessID);
    SaveToFile(ExtractFileName(Application.ExeName) + '.pid');
    Free;
  end;

  process := TProcess.Create(nil);

  cgiProccess := TProcess.Create(nil);
  cgiProccess.Options := cgiProccess.Options + [poNoConsole];
  cgiProccess.CommandLine := '"' + serverPath + 'usr/php/php-cgi.exe" -b 127.0.0.1:' + IntToStr(cgiPort);

  nginxProcess := TProcess.Create(nil);
  nginxProcess.Options := nginxProcess.Options + [poNoConsole];
  nginxProcess.CommandLine := '"' + serverPath + 'nginx/nginx.exe" -p "' + serverPath + 'nginx/"';

  mongoProcess := TProcess.Create(nil);
  mongoProcess.Options := mongoProcess.Options + [poNoConsole, poUsePipes];
  mongoProcess.CommandLine := '"' + serverPath + 'usr/mongodb/bin/mongod.exe"' +
                           ' --logpath "'+ serverPath +'logs/mongodb.log"' +
                           ' --dbpath "'+ serverPath +'/data/mongodb/" --directoryperdb --journal';
end;

class procedure Core.PostInitialize;
begin
     Application.MainForm.OnCloseQuery := onCloseQuery;
     Application.OnMinimize := onMinimize;

     tray.PopUpMenu.AutoPopup := true;
     tray.Icons := dtMain.ImageList;
end;

class procedure Core.Finalize;
begin
  tray.Free;
  configuration.WriteInteger('main', 'cgi.port', cgiPort);
  configuration.Free;

  DeleteFile(ExtractFileName(Application.ExeName) + '.pid');

  Core.StopServer;
end;

class procedure Core.Restore;
begin
    Application.MainForm.Show;
    Application.Restore;
end;

class procedure Core.Hide;
begin
    Application.Minimize;
    Application.MainForm.Hide;
end;

class procedure Core.Exit;
begin
  if ( MessageDlg('The server will be stopped! Are you shure?', mtConfirmation, mbYesNo, 0) = IDYES ) then
  begin
       Application.MainForm.OnCloseQuery := nil;
       Application.MainForm.Close;
  end;
end;

class procedure Core.SetTrayMenu(Menu: TPopupMenu);
begin
  tray.PopUpMenu := Menu;
end;

class procedure Core.SetPHPVersion(Version: String);
begin
  phpVersion := Version;
end;

class procedure Core.StartServer;
begin
  status := 'started';
  cgiProccess.Execute;
  nginxProcess.Execute;
  mongoProcess.Execute;
end;

class procedure Core.RestartServer;
begin
  StopServer;
  StartServer;
end;

class procedure Core.StopServer;
begin
  killProcess(cgiProccess);
  killProcess(nginxProcess);
  killProcess(mongoProcess);
  status := 'stopped';
end;

procedure Core.onMessage(Sender: TObject);
begin
  Core.Restore;
end;

end.


{%RunCommand $MakeExe($(EdFile)) -start -h}
unit uCore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms, Configuration, Menus, Dialogs, LCLType,
  process, Graphics, IntfGraphics, registry;

type
  { Core }
  TServerStatus = (ssStarted, ssStopped);
  TVersions = class;

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
  public
        class var version: TVersions;
        class var status: TServerStatus;
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

        class procedure RegisterOnStartUp(ServerStart: Boolean);
        class procedure UnregisterOnStartUp();
        class function isRegisteredOnStartUp: Boolean;
        class function isStartOnStartUp: Boolean;

        class function IsCmdParam(Param: String): Boolean;

        class procedure ShowConfiguration;

        procedure onMessage(Sender: TObject);
  end;

  { CoreUtils }

  CoreUtils = class(TObject)
  protected
        class var lastProccess: TProcess;
  public
        class function Execute(Command: String; Parameters: Array of String; Options: TProcessOptions = [poNoConsole, poWaitOnExit]): String;
        class procedure KillProcess(ProcessID: Integer); overload;
        class procedure KillProcess(Process: TProcess); overload;
        class function GetLastProcessID: Integer;
  end;

  { TVersions }
  TVersions = class(TObject)
  private
    FMongoDB: String;
    FNginx: String;
    FPHP: String;
  protected

  public
    constructor Create;
    destructor Destroy;

    property PHP: String read FPHP;
    property MongoDB: String read FMongoDB;
    property Nginx: String read FNginx;

    procedure UpdateVersions;
  end;

implementation

uses dMain, fConfiguration;

{ CoreUtils }

class function CoreUtils.Execute(Command: String; Parameters: Array of String; Options: TProcessOptions = [poNoConsole, poWaitOnExit]): String;
  var
     output: TStrings;
     process: TProcess;
     i: Integer;
begin
  if Assigned(lastProccess) then
     FreeAndNil(lastProccess);

  Result  := '';
  output  := TStringList.Create;
  process := TProcess.Create(nil);
  lastProccess := process;
  try
    process.Options    := process.Options + Options + [poUsePipes];
    process.Executable := Command;

    for i := 0 to High(Parameters) do
        process.Parameters.Add(Parameters[i]);

    process.Execute;

    if process.Output <> nil then
    begin
         output.LoadFromStream(process.Output);
         Result := Trim(output.Text);
    end;

    if Result = '' then
    begin
         if process.Stderr <> nil then
         begin
              output.LoadFromStream(process.Stderr);
              Result := Trim(output.Text);
         end;
    end;
  finally
    output.Free;
  end;
end;

class procedure CoreUtils.KillProcess(ProcessID: Integer);
begin
  if ProcessID <> 0 then
     Execute('taskkill',  ['/F', '/PID', IntToStr(ProcessID)]);
end;

class procedure CoreUtils.KillProcess(Process: TProcess);
begin
  if (Process <> nil) and (Process.Active) then
     KillProcess(Process.ProcessID);
end;

class function CoreUtils.GetLastProcessID: Integer;
begin
  if lastProccess <> nil then
     Result := lastProccess.ProcessID
  else
     Result := 0;
end;

{ TVersions }

constructor TVersions.Create;
begin
   UpdateVersions;
end;

destructor TVersions.Destroy;
begin

end;

procedure TVersions.UpdateVersions;
begin
   FPHP     := CoreUtils.Execute(Core.serverPath + 'usr/php/php.exe', ['--version']);
   FMongoDB := CoreUtils.Execute(Core.serverPath + 'usr/mongodb/bin/mongod.exe', ['--version']);
   FNginx   := CoreUtils.Execute(Core.serverPath + 'nginx/nginx.exe', ['-v']);
end;


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

class procedure Core.Initialize;
begin
  status     := ssStopped;
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

  version := TVersions.Create;
end;

class procedure Core.PostInitialize;
begin
     Application.MainForm.OnCloseQuery := onCloseQuery;
     Application.OnMinimize := onMinimize;

     tray.PopUpMenu.AutoPopup := true;
     tray.Icons := dtMain.ImageList;

     if IsCmdParam('-start') then
        Core.StartServer;
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
  Core.StopServer;

  status := ssStarted;
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
  CoreUtils.Execute(serverPath + 'nginx/nginx.exe', ['-s', 'quit', '-p', serverPath + 'nginx/']);
  CoreUtils.KillProcess(cgiProccess);
  CoreUtils.KillProcess(mongoProcess);

  status := ssStopped;
end;

const
  ApplicationTitle = 'RegenixServer';
  StartupSection   = 'Software\Microsoft\Windows\CurrentVersion\RunOnce' + #0;
  StartupRoot      = HKEY_CURRENT_USER;

class procedure Core.RegisterOnStartUp(ServerStart: Boolean);
 var
    cmd: String;
begin
  with TRegIniFile.Create('') do
   try
     RootKey := StartupRoot;
     cmd     := '"' + Application.ExeName + '" -h';
     if ServerStart then
       cmd := cmd + ' -start';

     WriteString(StartupSection, ApplicationTitle, cmd) ;
   finally
     Free;
   end;
end;

class procedure Core.UnregisterOnStartUp;
begin
  with TRegIniFile.Create('') do
   try
     RootKey := StartupRoot;
     DeleteKey(StartupSection, ApplicationTitle);
   finally
     Free;
   end;
end;

class function Core.isRegisteredOnStartUp: Boolean;
  var
  cmd: String;
begin
  Result := false;
  with TRegIniFile.Create('') do
   try
     RootKey := StartupRoot;
     cmd := ReadString(StartupSection, ApplicationTitle, '');
     Result := cmd <> '';
   finally
     Free;
   end;
end;

class function Core.isStartOnStartUp: Boolean;
var
   cmd: String;
begin
Result := false;
with TRegIniFile.Create('') do
 try
   RootKey := StartupRoot;
   cmd := ReadString(StartupSection, ApplicationTitle, '');
   Result := Pos(' -start', cmd) > 0;
 finally
   Free;
 end;
end;

class function Core.IsCmdParam(Param: String): Boolean;
  var
     i: integer;
begin
Result := false;
  for i := 1 to Paramcount do
  begin
    if ParamStr(i) = Param then
    begin
       Result := true;
       break;
    end;
  end;
end;

class procedure Core.ShowConfiguration;
begin
  fmConfiguration.cbRunProgram.Checked  := isRegisteredOnStartUp;
  fmConfiguration.cbStartServer.Checked := isStartOnStartUp;

  if fmConfiguration.ShowModal = IDOK then
  begin
       if fmConfiguration.cbRunProgram.Checked then
       begin
         RegisterOnStartUp(fmConfiguration.cbStartServer.Checked);
       end else
       begin
         UnregisterOnStartUp();
       end;
  end;
end;

procedure Core.onMessage(Sender: TObject);
begin
  Core.Restore;
end;

end.


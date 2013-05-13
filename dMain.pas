unit dMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Menus, Controls, ExtCtrls, uCore, simpleipc,
  SimpleIPCWrapper;

type

  { TdtMain }

  TdtMain = class(TDataModule)
    ImageList: TImageList;
    itExit: TMenuItem;
    itAbout: TMenuItem;
    MenuItem1: TMenuItem;
    itStartServer: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    itOpenControl: TMenuItem;
    itStopServer: TMenuItem;
    itRestartServer: TMenuItem;
    trayMenu: TPopupMenu;
    procedure DataModuleCreate(Sender: TObject);
    procedure itExitClick(Sender: TObject);
    procedure itOpenControlClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dtMain: TdtMain;

implementation

{$R *.lfm}

{ TdtMain }

procedure TdtMain.DataModuleCreate(Sender: TObject);
begin
  Core.SetTrayMenu(trayMenu);
end;

procedure TdtMain.itExitClick(Sender: TObject);
begin
  Core.Exit;
end;

procedure TdtMain.itOpenControlClick(Sender: TObject);
begin
  Core.Restore;
end;

end.


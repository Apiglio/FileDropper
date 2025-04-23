unit file_dropper_main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    StdCtrls, ExtCtrls, file_dropper;

type

    TDetailedButton = class(TPanel)
    private
        ButtonIcon:TImage;
        ButtonTitle:TLabel;
    protected
        procedure DoPanelClick(Sender: TObject);
        procedure DoPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    public
        ButtonId:Integer;
    public
        procedure LoadIcon(icon:Graphics.TBitmap);
        procedure SetTitle(Title:string);
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
    end;
    TFileDropperPhase  = (fdpOveriew, fdpOpenner, fpdArchiver, fpdFormatter);


    { TForm_FileDropper }

    TForm_FileDropper = class(TForm)
      Memo_Formatter_Tmp: TMemo;
      Panel_FormatterDetailed: TPanel;
      Panel_ArchiverDetailed: TPanel;
      Panel_Formatter: TPanel;
      Panel_Archiver: TPanel;
        Panel_OpennerDetailed: TPanel;
        Panel_Openner: TPanel;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
        procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
        procedure Panel_ArchiverClick(Sender: TObject);
        procedure Panel_ArchiverMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure Panel_FormatterClick(Sender: TObject);
        procedure Panel_FormatterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure Panel_OpennerDetailedResize(Sender: TObject);
        procedure Panel_OpennerClick(Sender: TObject);
        procedure Panel_OpennerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    private
        FDetailedButtons:TList;
        procedure DetailedButtonClick(Sender: TObject);
        procedure DetailedMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    protected
        FPhase: TFileDropperPhase;
    public
        procedure Phase_Overview;
        procedure Phase_Openner;
        procedure Phase_Archiver;
        procedure Phase_Formatter;
    end;

var
    Form_FileDropper: TForm_FileDropper;
    FileDropper:TFileDropper;
    CurrentFileName:string;
    ReleaseAction:boolean;

implementation
uses Windows;

{$R *.lfm}


{ TDetailedButton }

procedure TDetailedButton.DoPanelClick(Sender: TObject);
begin
    OnClick(Self);
end;

procedure TDetailedButton.DoPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    OnMouseUp(Self,Button,Shift,X,Y);
end;

procedure TDetailedButton.LoadIcon(icon:Graphics.TBitmap);
begin
    ButtonIcon.Picture.Bitmap:=icon;
end;

procedure TDetailedButton.SetTitle(Title:string);
begin
    ButtonTitle.Caption:=Title;
end;

constructor TDetailedButton.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    ButtonIcon:=TImage.Create(Self);
    ButtonIcon.Parent:=Self;
    ButtonIcon.Anchors:=[akTop,akLeft];
    ButtonIcon.AnchorSideTop.Control:=Self;
    ButtonIcon.AnchorSideTop.Side:=asrCenter;
    ButtonIcon.AnchorSideLeft.Control:=Self;
    ButtonIcon.AnchorSideLeft.Side:=asrCenter;
    ButtonIcon.AutoSize:=true;
    ButtonIcon.OnClick:=@DoPanelClick;
    ButtonIcon.OnMouseUp:=@DoPanelMouseUp;

    ButtonTitle:=TLabel.Create(Self);
    ButtonTitle.Parent:=Self;
    ButtonTitle.Anchors:=[akTop,akLeft];
    ButtonTitle.AnchorSideTop.Control:=ButtonIcon;
    ButtonTitle.AnchorSideTop.Side:=asrBottom;
    ButtonTitle.AnchorSideLeft.Control:=Self;
    ButtonTitle.AnchorSideLeft.Side:=asrCenter;
    ButtonTitle.AutoSize:=true;
    ButtonTitle.OnClick:=@DoPanelClick;
    ButtonTitle.OnMouseUp:=@DoPanelMouseUp;

    Self.Color:=clWhite;

end;

destructor TDetailedButton.Destroy;
begin
    inherited Destroy;
end;


{ TForm_FileDropper }

procedure TForm_FileDropper.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    FDetailedButtons.Free;

    FileDropper.SaveProperties;
    FileDropper.Free;
end;

procedure TForm_FileDropper.FormCreate(Sender: TObject);
var idx,h4,w4:integer;
    newDetailedButton:TDetailedButton;
    tmpOpener:TFileDropperOpener;
begin
    FileDropper:=TFileDropper.Create;
    FileDropper.LoadProperties;
    //强行补到16个
    while FileDropper.GetOpenerCount<16 do begin
        tmpOpener:=TFileDropperOpener.Create;
        tmpOpener.DisplayName:='';
        //tmpOpener.DisplayIcon:='';
        tmpOpener.ExecuteFile:='';
        FileDropper.AddOpenner(tmpOpener);
    end;

    h4:=Panel_OpennerDetailed.Height div 4;
    w4:=Panel_OpennerDetailed.Width div 4;
    FDetailedButtons:=TList.Create;
    for idx:=0 to 15 do begin
        newDetailedButton:=TDetailedButton.Create(Self);
        newDetailedButton.Parent:=Panel_OpennerDetailed;
        newDetailedButton.Top:=(idx div 4) * h4;
        newDetailedButton.Left:=(idx mod 4) * w4;
        newDetailedButton.Width:=w4;
        newDetailedButton.Height:=h4;
        newDetailedButton.ButtonId:=idx;
        newDetailedButton.OnClick:=@DetailedButtonClick;
        newDetailedButton.OnMouseUp:=@DetailedMouseUp;
        FDetailedButtons.Add(newDetailedButton);
        tmpOpener:=FileDropper.GetOpenerById(idx);
        if tmpOpener<>nil then begin
            newDetailedButton.SetTitle(tmpOpener.DisplayName);
            tmpOpener.ReloadIcon;
            newDetailedButton.LoadIcon(tmpOpener.DisplayIcon);
        end;
    end;

    ReleaseAction:=false;
    Phase_Overview;
end;

procedure TForm_FileDropper.FormDeactivate(Sender: TObject);
begin
    Phase_Overview;
end;

procedure TForm_FileDropper.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
    if Length(FileNames)<>1 then exit;
    CurrentFileName:=FileNames[0];
    ReleaseAction:=true;

end;

procedure TForm_FileDropper.Panel_ArchiverClick(Sender: TObject);
begin
    if FPhase=fpdArchiver then begin
        Phase_Overview;
        ReleaseAction:=false;
    end else begin
        Phase_Archiver;
        CurrentFileName:='';
    end;
end;

procedure TForm_FileDropper.Panel_ArchiverMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
    if ReleaseAction then Phase_Archiver;
end;

procedure TForm_FileDropper.Panel_FormatterClick(Sender: TObject);
begin
    if FPhase=fpdFormatter then begin
        Phase_Overview;
        ReleaseAction:=false;
    end else begin
        Phase_Formatter;
        CurrentFileName:='';
    end;
end;

procedure TForm_FileDropper.Panel_FormatterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
    if ReleaseAction then Phase_Formatter;
end;

procedure TForm_FileDropper.Panel_OpennerDetailedResize(Sender: TObject);
var idx,h4,w4:integer;
begin
    h4:=Panel_OpennerDetailed.Height div 4;
    w4:=Panel_OpennerDetailed.Width div 4;
    for idx:=0 to 15 do begin
        TDetailedButton(FDetailedButtons[idx]).Top:=(idx div 4) * h4;
        TDetailedButton(FDetailedButtons[idx]).Left:=(idx mod 4) * w4;
        TDetailedButton(FDetailedButtons[idx]).Width:=w4;
        TDetailedButton(FDetailedButtons[idx]).Height:=h4;
    end;
end;

procedure TForm_FileDropper.Panel_OpennerClick(Sender: TObject);
begin
    if FPhase=fdpOpenner then begin
        Phase_Overview;
        ReleaseAction:=false;
    end else begin
        Phase_Openner;
        CurrentFileName:='';
    end;
end;

procedure TForm_FileDropper.Panel_OpennerMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
begin
    if ReleaseAction then Phase_Openner;
end;

procedure TForm_FileDropper.DetailedButtonClick(Sender: TObject);
begin
    if not ReleaseAction then exit;
    if CurrentFileName<>'' then FileDropper.Open(CurrentFileName,(Sender as TDetailedButton).ButtonId);
    ReleaseAction:=false;
    Phase_Overview;
end;

procedure TForm_FileDropper.DetailedMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var buttonId:Integer;
    tmpOpener:TFileDropperOpener;
    old_execfile, new_execfile, old_dispname, new_dispname, old_params, new_params:string;
begin
    if (Button=mbRight) and (Shift = []) then begin
        buttonId:=TDetailedButton(Sender as TDetailedButton).ButtonId;
        tmpOpener:=FileDropper.GetOpenerById(buttonId);
        old_execfile:=tmpOpener.ExecuteFile;
        old_params:=  tmpOpener.RunParameters;
        old_dispname:=tmpOpener.DisplayName;
        new_execfile:=InputBox('修改打开方式','执行文件路径：',old_execfile);
        if (new_execfile<>'') and (new_execfile<>old_execfile) then tmpOpener.ExecuteFile:=new_execfile;
        new_params:=  InputBox('修改打开方式','程序执行参数：',old_params);
        if (new_params<>'')   and (new_params<>old_params) then tmpOpener.RunParameters:=new_params;
        new_dispname:=InputBox('修改打开方式','按键提示文字：',old_dispname);
        if (new_dispname<>'') and (new_dispname<>old_dispname) then tmpOpener.DisplayName:=new_dispname;
        (Sender as TDetailedButton).SetTitle(new_dispname);
        tmpOpener.ReloadIcon;
        (Sender as TDetailedButton).LoadIcon(tmpOpener.DisplayIcon);
    end;
end;

procedure TForm_FileDropper.Phase_Overview;
begin
    Panel_Openner.Height:=80;
    Panel_Archiver.Height:=80;
    //Panel_Formatter.Height:=80; //因为有anchor最后一个不需要设置高度
    Width:=120;
    Height:=240; //80*3
    FPhase:=fdpOveriew;
end;

procedure TForm_FileDropper.Phase_Openner;
begin
    Panel_Openner.Height:=320; //480-2*80;
    Panel_Archiver.Height:=80;
    //Panel_Formatter.Height:=80; //因为有anchor最后一个不需要设置高度
    Width:=640;
    Height:=480;
    FPhase:=fdpOpenner;
end;

procedure TForm_FileDropper.Phase_Archiver;
begin
    Panel_Openner.Height:=80;
    Panel_Archiver.Height:=320; //480-2*80;
    //Panel_Formatter.Height:=80; //因为有anchor最后一个不需要设置高度
    Width:=640;
    Height:=480;
    FPhase:=fpdArchiver;
end;

procedure TForm_FileDropper.Phase_Formatter;
begin
    Panel_Openner.Height:=80;
    Panel_Archiver.Height:=80;
    //Panel_Formatter.Height:=320; //480-2*80; //因为有anchor最后一个不需要设置高度
    Width:=640;
    Height:=480;
    FPhase:=fpdFormatter;
end;

end.


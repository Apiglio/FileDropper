unit file_dropper_main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    StdCtrls, ExtCtrls, file_dropper, comobj, windows, ActiveX, LazUTF8, Clipbrd;

type

    TDetailedButton = class(TPanel)
    private
        ButtonIcon:TImage;
        ButtonTitle:TLabel;
        FHover:boolean;
    protected
        procedure DoPanelClick(Sender: TObject);
        procedure DoPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure DoResize(Sender: TObject);
        procedure DoMouseEnter(Sender: TObject);
        procedure DoMouseLeave(Sender: TObject);
        procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
        procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
        procedure DoPaint(Sender: TObject);
    public
        ButtonId:Integer;
        Actioner:TObject; // Openner, Archiver or Formatter
    public
        procedure LoadIcon(icon:Graphics.TBitmap);
        procedure SetTitle(Title:string);
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
    end;
    TFileDropperPhase  = (fdpOveriew, fdpOpener, fpdArchiver, fpdFormatter);


    { TForm_FileDropper }

    TForm_FileDropper = class(TForm, IDropTarget)
        Memo_FileInfo: TMemo;
        Memo_Formatter_Example: TMemo;
        Panel_Readonly: TPanel;
        Panel_FormatterDetailed: TPanel;
        Panel_ArchiverDetailed: TPanel;
        Panel_Formatter: TPanel;
        Panel_Archiver: TPanel;
        Panel_OpenerDetailed: TPanel;
        Panel_Opener: TPanel;
        Panel_Hidden: TPanel;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
        procedure Panel_ArchiverClick(Sender: TObject);
        procedure Panel_ArchiverMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure Panel_FormatterClick(Sender: TObject);
        procedure Panel_FormatterDetailedResize(Sender: TObject);
        procedure Panel_FormatterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure Panel_OpenerDetailedResize(Sender: TObject);
        procedure Panel_OpenerClick(Sender: TObject);
        procedure Panel_OpenerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    private
        FDetailedButtons:TList;
        procedure DetailedButtonClick(Sender: TObject);
        procedure DetailedMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    private
        function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
        function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
        function DragLeave: HResult;StdCall;
        function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD):HResult;StdCall;
    protected
        FPhase: TFileDropperPhase;
        FCurrentFiles: array of string;
    public
        procedure Phase_Overview;
        procedure Phase_Openner;
        procedure Phase_Archiver;
        procedure Phase_Formatter;
    end;

var
    Form_FileDropper: TForm_FileDropper;
    FileDropper:TFileDropper;
    ReleaseAction:boolean;

implementation

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

procedure TDetailedButton.DoResize(Sender: TObject);
begin
    if Self.Height<32 then begin
        ButtonIcon.Visible:=false;
    end else begin
        ButtonIcon.Visible:=true;
    end;
end;

procedure TDetailedButton.DoMouseEnter(Sender: TObject);
begin
    FHover:=true;
    Paint;
end;

procedure TDetailedButton.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
    OnDragDrop(Sender, Source, X, Y)
end;

procedure TDetailedButton.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
    DoDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TDetailedButton.DoMouseLeave(Sender: TObject);
begin
    FHover:=false;
    Paint;
end;

procedure TDetailedButton.DoPaint(Sender: TObject);
begin
    Canvas.Brush.Style:=bsClear;
    Canvas.Pen.Width:=5;
    if FHover then Canvas.Pen.Color:=$cfcfcf else Canvas.Pen.Color:=clWhite;;
    Canvas.Rectangle(0,0,Self.Width-1,Height-1);
    Canvas.Pen.Width:=1;
    Canvas.Pen.Color:=clBlack;
    Canvas.Rectangle(-1,-1,Self.Width,Height);

end;

procedure TDetailedButton.LoadIcon(icon:Graphics.TBitmap);
begin
    ButtonIcon.Picture.Bitmap:=icon;
    if icon=nil then ButtonIcon.Picture.Bitmap.SetSize(1,1);
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
    ButtonIcon.OnMouseEnter:=@DoMouseEnter;
    ButtonIcon.OnMouseLeave:=@DoMouseLeave;

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
    ButtonTitle.OnMouseEnter:=@DoMouseEnter;
    ButtonTitle.OnMouseLeave:=@DoMouseLeave;

    Self.OnResize:=@DoResize;
    Self.OnPaint:=@DoPaint;
    Self.OnMouseEnter:=@DoMouseEnter;
    Self.OnMouseLeave:=@DoMouseLeave;
    Self.Color:=clWhite;
    Self.FHover:=false;


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


    RevokeDragDrop(Self.Handle);
    OleUninitialize;

end;

procedure TForm_FileDropper.FormCreate(Sender: TObject);
var idx:integer;
    newDetailedButton:TDetailedButton;
    tmpOpener:TFileDropperOpener;
    tmpFormatter:TFileDropperFormatter;
begin

    OleInitialize(nil);
    RegisterDragDrop(Self.Handle, Self as IDropTarget);
    DragAcceptFiles(Self.Handle, true);

    FileDropper:=TFileDropper.Create;
    FileDropper.LoadProperties;
    FDetailedButtons:=TList.Create;

    // 强行补到16个
    // Opener
    while FileDropper.GetOpenerCount<16 do begin
        tmpOpener:=TFileDropperOpener.Create;
        tmpOpener.DisplayName:='';
        tmpOpener.ExecuteFile:='';
        FileDropper.AddOpenner(tmpOpener);
    end;
    for idx:=0 to 15 do begin
        newDetailedButton:=TDetailedButton.Create(Self);
        newDetailedButton.Parent:=Panel_OpenerDetailed;
        newDetailedButton.ButtonId:=idx;
        newDetailedButton.OnClick:=@DetailedButtonClick;
        newDetailedButton.OnMouseUp:=@DetailedMouseUp;
        FDetailedButtons.Add(newDetailedButton);
        newDetailedButton.Actioner:=FileDropper.GetOpenerById(idx);
        tmpOpener:=newDetailedButton.Actioner as TFileDropperOpener;
        if tmpOpener<>nil then begin
            newDetailedButton.SetTitle(tmpOpener.DisplayName);
            tmpOpener.ReloadIcon;
            newDetailedButton.LoadIcon(tmpOpener.DisplayIcon);
        end;
    end;

    // 强行补到16个
    // Formatter
    while FileDropper.GetFormatterCount<16 do begin
        tmpFormatter:=TFileDropperFormatter.Create;
        tmpFormatter.DisplayName:='';
        tmpFormatter.OriginText:='';
        tmpFormatter.ReplaceText:='';
        FileDropper.AddFormatter(tmpFormatter);
    end;
    for idx:=0 to 15 do begin
        newDetailedButton:=TDetailedButton.Create(Self);
        newDetailedButton.Parent:=Panel_FormatterDetailed;
        newDetailedButton.ButtonId:=idx;
        newDetailedButton.OnClick:=@DetailedButtonClick;
        newDetailedButton.OnMouseUp:=@DetailedMouseUp;
        FDetailedButtons.Add(newDetailedButton);
        newDetailedButton.Actioner:=FileDropper.GetFormatterById(idx);
        tmpFormatter:=newDetailedButton.Actioner as TFileDropperFormatter;
        if tmpFormatter<>nil then begin
            newDetailedButton.SetTitle(tmpFormatter.DisplayName);
            newDetailedButton.LoadIcon(nil);
        end;
    end;

    ReleaseAction:=false;
    Phase_Overview;
end;

procedure TForm_FileDropper.FormDeactivate(Sender: TObject);
begin
    Phase_Overview;
end;

procedure TForm_FileDropper.Panel_ArchiverClick(Sender: TObject);
begin
    if FPhase=fpdArchiver then begin
        Phase_Overview;
        ReleaseAction:=false;
    end else begin
        Phase_Archiver;
        SetLength(FCurrentFiles,0);
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
        SetLength(FCurrentFiles,0)
    end;
end;

procedure TForm_FileDropper.Panel_FormatterDetailedResize(Sender: TObject);
var idx,h4,w4:integer;
begin
    // Formatter 临时采用16-31的偏移量
    h4:=(Panel_FormatterDetailed.Height - Memo_Formatter_Example.Height - 2) div 4;
    w4:=Panel_FormatterDetailed.Width div 4;
    for idx:=0 to 15 do begin
        TDetailedButton(FDetailedButtons[idx+16]).Top:=Memo_Formatter_Example.Height + (idx div 4) * h4 + 3;
        TDetailedButton(FDetailedButtons[idx+16]).Left:=(idx mod 4) * w4;
        TDetailedButton(FDetailedButtons[idx+16]).Width:=w4;
        TDetailedButton(FDetailedButtons[idx+16]).Height:=h4;
    end;
end;

procedure TForm_FileDropper.Panel_FormatterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
    if ReleaseAction then Phase_Formatter;
end;

procedure TForm_FileDropper.Panel_OpenerDetailedResize(Sender: TObject);
var idx,h4,w4:integer;
begin
    // Opener
    h4:=Panel_OpenerDetailed.Height div 4;
    w4:=Panel_OpenerDetailed.Width div 4;
    for idx:=0 to 15 do begin
        TDetailedButton(FDetailedButtons[idx]).Top:=(idx div 4) * h4;
        TDetailedButton(FDetailedButtons[idx]).Left:=(idx mod 4) * w4;
        TDetailedButton(FDetailedButtons[idx]).Width:=w4;
        TDetailedButton(FDetailedButtons[idx]).Height:=h4;
    end;
end;

procedure TForm_FileDropper.Panel_OpenerClick(Sender: TObject);
begin
    if FPhase=fdpOpener then begin
        Phase_Overview;
        ReleaseAction:=false;
    end else begin
        Phase_Openner;
        SetLength(FCurrentFiles,0);
    end;
end;

procedure TForm_FileDropper.Panel_OpenerMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
begin
    if ReleaseAction then Phase_Openner;
end;

procedure TForm_FileDropper.DetailedButtonClick(Sender: TObject);
var buttonId:Integer;
    tmpButton:TDetailedButton;
begin
    tmpButton:=Sender as TDetailedButton;
    buttonId:=tmpButton.ButtonId;
    if tmpButton.Actioner=nil then exit;
    if tmpButton.Actioner is TFileDropperOpener then begin
        if Length(FCurrentFiles)>0 then begin
            if not ReleaseAction then exit;
            FileDropper.OpenWinCPs(FCurrentFiles,(Sender as TDetailedButton).ButtonId);
            ReleaseAction:=false;
            Phase_Overview;
        end;
    end else if tmpButton.Actioner is TFileDropperFormatter then begin
        FileDropper.Format(Memo_Formatter_Example.Lines,(Sender as TDetailedButton).ButtonId);
        Clipboard.AsText:=Memo_Formatter_Example.Text;
    end else begin
        //无效Actioner
    end;
end;

procedure TForm_FileDropper.DetailedMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var buttonId:Integer;
    tmpButton:TDetailedButton;
    tmpOpener:TFileDropperOpener;
    tmpFormmater:TFileDropperFormatter;
    old_s1, new_s1, old_s2, new_s2, old_s3, new_s3:string;
begin
    if (Button=mbRight) and (Shift = []) then begin
        tmpButton:=Sender as TDetailedButton;
        buttonId:=tmpButton.ButtonId;
        if tmpButton.Actioner=nil then exit;

        if tmpButton.Actioner is TFileDropperOpener then begin
            tmpOpener:=FileDropper.GetOpenerById(buttonId);
            old_s1:= tmpOpener.ExecuteFile;
            old_s3:= tmpOpener.RunParameters;
            old_s2:= tmpOpener.DisplayName;
            new_s1:= InputBox('修改打开方式','执行文件路径：',old_s1);
            if (new_s1<>'') and (new_s1<>old_s1) then tmpOpener.ExecuteFile:=new_s1;
            new_s3:= InputBox('修改打开方式','程序执行参数：',old_s3);
            if (new_s3<>'')   and (new_s3<>old_s3) then tmpOpener.RunParameters:=new_s3;
            new_s2:= InputBox('修改打开方式','按键提示文字：',old_s2);
            if (new_s2<>'') and (new_s2<>old_s2) then tmpOpener.DisplayName:=new_s2;
            (Sender as TDetailedButton).SetTitle(new_s2);
            tmpOpener.ReloadIcon;
            (Sender as TDetailedButton).LoadIcon(tmpOpener.DisplayIcon);
        end else if tmpButton.Actioner is TFileDropperFormatter then begin
            tmpFormmater:=FileDropper.GetFormatterById(buttonId);
            old_s2:= tmpFormmater.OriginText;
            old_s3:= tmpFormmater.ReplaceText;
            old_s1:= tmpFormmater.DisplayName;
            new_s2:= InputBox('修改替换设置','查找字符串：',old_s2);
            if (new_s2<>'') and (new_s2<>old_s2) then tmpFormmater.OriginText:=new_s2;
            new_s3:= InputBox('修改替换设置','替换字符串：',old_s3);
            if (new_s3<>'') and (new_s3<>old_s3) then tmpFormmater.ReplaceText:=new_s3;
            new_s1:= InputBox('修改替换设置','按键提示文字：',old_s1);
            if (new_s1<>'') and (new_s1<>old_s1) then begin
                tmpFormmater.DisplayName:=new_s1;
                (Sender as TDetailedButton).SetTitle(new_s1);
            end;
        end else begin
            //无效Actioner
        end;
    end;
end;

function TForm_FileDropper.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; StdCall;
begin
    result:=S_OK;
end;

function TForm_FileDropper.DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; StdCall;
begin
    dwEffect:=DROPEFFECT_COPY;
    result:=S_OK;
end;

function TForm_FileDropper.DragLeave: HResult; StdCall;
begin
    result:=S_OK;
end;

function TForm_FileDropper.Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; StdCall;
var EnumFormatEtc: IEnumFormatEtc;
    FormatEtc: TFormatEtc;
    Fetched: LongInt;
    Medium: TStgMedium;
    tmpDrop: HDROP;
    FilePath:array[0..MAX_PATH] of char;
    idx, FileCount:integer;
    unicode_context:utf8String;
begin
    Result := S_OK;
    if not dataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc) = S_OK then exit;
    while EnumFormatEtc.Next(1, FormatEtc, @Fetched) = S_OK do begin
        case FormatEtc.cfFormat of
            CF_HDROP: // InShellDragLoop
            begin
                SetLength(FCurrentFiles,0);
                if dataObj.GetData(FormatEtc, Medium) = S_OK then
                try
                    tmpDrop := HDROP(Medium.hGlobal);
                    FileCount := DragQueryFile(tmpDrop, $FFFFFFFF, nil, 0);
                    SetLength(FCurrentFiles, FileCount);
                    for idx := 0 to FileCount - 1 do begin
                        DragQueryFile(tmpDrop, idx, @FilePath[0], MAX_PATH);
                        FCurrentFiles[idx] := FilePath;
                    end;
                    ReleaseAction:=true;
                finally
                    ReleaseStgMedium(Medium);
                end;
            end;
            CF_UNICODETEXT:
            begin
                if dataObj.GetData(FormatEtc, Medium) = S_OK then
                try
                    unicode_context:=PUnicodeChar(GlobalLock(Medium.hGlobal));
                    Memo_Formatter_Example.Lines.Clear;
                    Memo_Formatter_Example.Lines.add(unicode_context);
                    GlobalUnlock(Medium.hGlobal);
                    ReleaseAction:=true;
                finally
                    ReleaseStgMedium(Medium);
                end;
                Application.ProcessMessages;
                Clipboard.AsText:=Memo_Formatter_Example.Text;
            end;
        end;
    end;
end;

procedure TForm_FileDropper.Phase_Overview;
begin
    Panel_Opener.Height:=80;
    Panel_Archiver.Height:=80;
    //Panel_Formatter.Height:=80; //因为有anchor最后一个不需要设置高度
    Width:=120;
    Height:=240; //80*3
    FPhase:=fdpOveriew;
end;

procedure TForm_FileDropper.Phase_Openner;
begin
    Panel_Opener.Height:=320; //480-2*80;
    Panel_Archiver.Height:=80;
    //Panel_Formatter.Height:=80; //因为有anchor最后一个不需要设置高度
    Width:=640;
    Height:=480;
    FPhase:=fdpOpener;
end;

procedure TForm_FileDropper.Phase_Archiver;
begin
    Panel_Opener.Height:=80;
    Panel_Archiver.Height:=320; //480-2*80;
    //Panel_Formatter.Height:=80; //因为有anchor最后一个不需要设置高度
    Width:=640;
    Height:=480;
    FPhase:=fpdArchiver;
end;

procedure TForm_FileDropper.Phase_Formatter;
begin
    Panel_Opener.Height:=80;
    Panel_Archiver.Height:=80;
    //Panel_Formatter.Height:=320; //480-2*80; //因为有anchor最后一个不需要设置高度
    Width:=640;
    Height:=480;
    FPhase:=fpdFormatter;
end;

end.


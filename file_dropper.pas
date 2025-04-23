unit file_dropper;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Windows, Graphics;

type

    TFileDropperOpener = class
    public
        DisplayName : String;
        DisplayIcon : TBitMap; //不保存在注册表中，直接从可执行文件中加载图标
        ExecuteFile : String;
        RunParameters: String;
    protected
        procedure SetJSON(json_str:string);
        function GetJSON:string;
    public
        property AsJSON:string read GetJSON write SetJSON;
    public
        procedure ReloadIcon;
        constructor Create;
        destructor Destroy; override;
    end;

    TFileDropper = class
    private
        FFileOpeners : TList;
    public
        procedure AddOpenner(openner:TFileDropperOpener);
        procedure Open(Filename:String;index:Integer);
        function GetOpenerById(index:Integer):TFileDropperOpener;
        function GetOpenerCount:Integer;
    public
        procedure LoadProperties;
        procedure SaveProperties;
    public
        constructor Create;
        destructor Destroy; override;
    end;

implementation
uses registry, fpjson, LazUTF8, Dialogs;

{ TFileDropperOpener }

procedure TFileDropperOpener.SetJSON(json_str:string);
var json:TJSONData;
begin
    json:=fpjson.GetJSON(json_str);
    try
        if not (json is TJSONObject) then exit;
        with TJSONObject(json) do begin
            if Find('dispname')<>nil then Self.DisplayName:=Strings['dispname'];
            if Find('execfile')<>nil then Self.ExecuteFile:=Strings['execfile'];
            if Find('runparams')<>nil then Self.RunParameters:=Strings['runparams'];
        end;
    finally
        json.Free;
    end;
end;

function TFileDropperOpener.GetJSON:string;
begin
    with TJSONObject.Create do
    try
        Add('dispname',Self.DisplayName);
        Add('execfile',Self.ExecuteFile);
        Add('runparams',Self.RunParameters);
        result:=AsJSON;
    finally
        Free;
    end;
end;

procedure TFileDropperOpener.ReloadIcon;
var Icon: TIcon;
    IconHandle: HICON;
begin
    Icon := TIcon.Create;
    try
        IconHandle := ExtractIcon(HInstance, PChar(ExecuteFile), 0);
        if IconHandle = 0 then exit;//无法从文件中提取图标，直接退出
        try
            Icon.Handle := IconHandle;
            DisplayIcon.Width := Icon.Width;
            DisplayIcon.Height := Icon.Height;
            DisplayIcon.Canvas.Brush.Color := clNone;
            DisplayIcon.Canvas.FillRect(Classes.Rect(0, 0, DisplayIcon.Width, DisplayIcon.Height));
            DisplayIcon.Canvas.Draw(0, 0, Icon);
        finally
            DestroyIcon(IconHandle);
        end;
    finally
        Icon.Free;
    end;
end;

constructor TFileDropperOpener.Create;
begin
    inherited Create;
    DisplayIcon:=TBitmap.Create;
    DisplayIcon.PixelFormat:=pf32bit;
    DisplayIcon.SetSize(0,0);
end;

destructor TFileDropperOpener.Destroy;
begin
    DisplayIcon.Free;
    inherited Destroy;
end;



{ TFileDropper }

procedure TFileDropper.AddOpenner(openner:TFileDropperOpener);
begin
    FFileOpeners.Add(openner);
end;

procedure TFileDropper.Open(Filename:String;index:Integer);
begin
    if index>=FFileOpeners.Count then exit;
    with TFileDropperOpener(Self.FFileOpeners.Items[index]) do begin
        if ExecuteFile='' then exit;
        ShellExecute(0,'open',pchar(Utf8ToWinCP(ExecuteFile)),pchar(Utf8ToWinCP(RunParameters+' "'+Filename+'"')),'',SW_NORMAL);
    end;
end;

function TFileDropper.GetOpenerById(index:Integer):TFileDropperOpener;
begin
    result:=nil;
    if index>=FFileOpeners.Count then exit;
    result:=TFileDropperOpener(FFileOpeners.Items[index]);
end;

function TFileDropper.GetOpenerCount:Integer;
begin
    result:=FFileOpeners.Count;
end;

procedure TFileDropper.LoadProperties;
var Reg:TRegistry;
    Idx:Integer;
    Key,Value:string;
    NewOpenner:TFileDropperOpener;
begin
    Reg:=TRegistry.Create;
    try
        Reg.RootKey:=HKEY_CURRENT_USER;
        Idx:=0;
        if Reg.OpenKey('Software\ApiglioToolBox\File_Dropper\Openner',false) then begin
            while true do begin
                Key:=IntToStr(Idx);
                if not Reg.ValueExists(Key) then break;
                Value:=Reg.ReadString(Key);
                NewOpenner:=TFileDropperOpener.Create;
                NewOpenner.AsJSON:=WinCPToUTF8(Value);
                AddOpenner(NewOpenner);
                Inc(Idx);
            end;
            Reg.CloseKey;
        end;

    finally
        Reg.Free;
    end;

end;

procedure TFileDropper.SaveProperties;
var Reg:TRegistry;
    idx:integer;
begin
    Reg:=TRegistry.Create;
    try
        Reg.RootKey:=HKEY_CURRENT_USER;
        Reg.DeleteKey('Software\ApiglioToolBox\File_Dropper\Openner');
        Reg.OpenKey('Software\ApiglioToolBox\File_Dropper\Openner',true);
        for idx:=FFileOpeners.Count-1 downto 0 do begin
            Reg.WriteString(
                IntToStr(idx),
                UTF8ToWinCP(TFileDropperOpener(FFileOpeners.Items[idx]).AsJSON)
            );
        end;
        Reg.CloseKey;
    finally
        Reg.Free;
    end;
end;

constructor TFileDropper.Create;
begin
    FFileOpeners:=TList.Create;
end;

destructor TFileDropper.Destroy;
var idx:integer;
begin
    for idx:= FFileOpeners.Count - 1 downto 0 do
        TFileDropperOpener(FFileOpeners.Items[idx]).Free;
    FFileOpeners.Free;
end;


end.


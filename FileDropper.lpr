program FileDropper;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, file_dropper_main, file_dropper
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
    DefaultTextLineBreakStyle := tlbsCR;  // 全局强制使用 CR, 临时的兼容word bug的处理
    Application.CreateForm(TForm_FileDropper, Form_FileDropper);
    Application.Run;
end.


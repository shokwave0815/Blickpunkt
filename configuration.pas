unit configuration;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, IniFiles, Forms, main, Dialogs;

type
    TConfiguration = class(TObject)
    private
    public
        procedure LoadForm(AForm: TMainForm);
        procedure SaveForm(AForm: TMainForm);
        procedure SavePicturesList(PicList: TStrings);
        function LoadPicturesList(): TStrings;
    end;

implementation

procedure TConfiguration.LoadForm(AForm: TMainForm);
var
    INI: TIniFile;
    aleft, atop: Integer;
begin
    try
        INI := TINIFile.Create(GetAppConfigDir(False) + 'config.ini');
        AForm.Top := AForm.Scale96ToScreen(INI.ReadInteger('Window', 'Top', 100));
        AForm.Left := AForm.Scale96ToScreen(INI.ReadInteger('Window', 'Left', 100));
        AForm.Width := AForm.Scale96ToForm(INI.ReadInteger('Window', 'Width', 800));
        AForm.Height := AForm.Scale96ToForm(INI.ReadInteger('Window', 'Height', 500));
        atop :=AForm.Scale96ToForm(INI.ReadInteger('SplitterH', 'Top', 150));
        AForm.ListSplitter.SetSplitterPosition(atop);
        aleft := AForm.Scale96ToForm(INI.ReadInteger('Splitter', 'Left', 300));
        AForm.PreviewSplitter.Left:= aleft;
        AForm.PreviewSplitter.SetSplitterPosition(aleft);

    finally
        FreeAndNil(INI);
    end;
end;

procedure TConfiguration.SaveForm(AForm: TMainForm);
var
    INIFile: TINIFile;
begin
    try
        INIFile := TINIFile.Create(GetAppConfigDir(False) + 'config.ini');
        INIFile.WriteInteger('Window', 'Top', AForm.ScaleScreenTo96(AForm.Top));
        INIFile.WriteInteger('Window', 'Left', AForm.ScaleScreenTo96(AForm.Left));
        INIFile.WriteInteger('Window', 'Width', AForm.ScaleFormTo96(AForm.Width));
        INIFile.WriteInteger('Window', 'Height', AForm.ScaleFormTo96(AForm.Height));
        INIFile.WriteInteger('Splitter', 'Left', AForm.ScaleFormTo96(AForm.PreviewSplitter.GetSplitterPosition));
        INIFile.WriteInteger('SplitterH', 'Top', AForm.ScaleFormTo96(AForm.ListSplitter.GetSplitterPosition));

    finally
        FreeAndNil(INIFile);
    end;
end;

procedure TConfiguration.SavePicturesList(PicList: TStrings);
var
    INIFile: TINIFile;
    i: integer;
begin
    ForceDirectories(GetAppConfigDir(False));
    PicList.SaveToFile(GetAppConfigDir(False) + 'piclist.txt');
    {
    try
        INIFile := TINIFile.Create(GetAppConfigDir(False) + 'config.ini');
        INIFile.EraseSection('Last');
        for i := 0 to PicList.Count - 1 do
        begin
            INIFile.WriteString('Last', IntToStr(i), PicList.Strings[i]);
        end;
    finally
        FreeAndNil(INIFile);
    end;
    }
end;

function TConfiguration.LoadPicturesList(): TStrings;
var
    INIFile: TINIFile;
    PicList: TStrings;
    i: integer;
begin
    Result := TStringList.Create();
    if FileExists(GetAppConfigDir(False) + 'piclist.txt') then
    begin
        Result.LoadFromFile(GetAppConfigDir(False) + 'piclist.txt');
    end else
    begin
        try
            INIFile := TINIFile.Create(GetAppConfigDir(False) + 'config.ini');
            PicList := TStringList.Create();
            INIFile.ReadSection('Last', PicList);

            for i := 0 to PicList.Count - 1 do
            begin
                Result.Append(INIFile.ReadString('Last', IntToStr(i), ''));
            end;
        finally
            FreeAndNil(PicList);
            FreeAndNil(INIFile);
        end;
    end;

end;

end.

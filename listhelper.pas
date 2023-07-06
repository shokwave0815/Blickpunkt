unit ListHelper;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, FileHelper, StdCtrls, FileUtil;

type
    TListHelper = class(TObject)
    private
        FileHelper: TFileHelper;
        procedure SearchNewPictures(PicList, Other: TStrings);
        procedure RemoveNonexistingPictures(PicList, Other: TStrings);
        function RemoveFilePaths(AList: TStrings): TStrings;
        function IsBigAndWide(FullFileName: string): boolean;
    public
        procedure FillListWithBigWidePics(FileList, TargetList: TStrings);
        procedure UpdateList(PicList, Other: TStrings);
        function HasSelectedItem(ListBox: TListBox): boolean;
        function RescanPicturesFolder(Folder: string): TStrings;
        constructor Create();
        destructor Destroy(); override;
    end;

implementation

procedure TListHelper.UpdateList(PicList, Other: TStrings);
begin
    if ((PicList <> nil) and (other <> nil)) then
    begin
        SearchNewPictures(PicList, Other);
        RemoveNonexistingPictures(PicList, Other);
    end;
end;

procedure TListHelper.SearchNewPictures(PicList, Other: TStrings);
var
    i: integer;
begin
    for i := 0 to Other.Count - 1 do
    begin
        if (PicList.IndexOf(ExtractFileName(Other.Strings[i])) < 0) then
        begin
            PicList.Insert(0, ExtractFileName(Other.Strings[i]));
        end;
    end;
end;

procedure TListHelper.RemoveNonexistingPictures(PicList, Other: TStrings);
var
    i: integer;
begin
    for i := PicList.Count - 1 downto 0 do
    begin
        if (Other.IndexOf(PicList.Strings[i]) < 0) then
        begin
            PicList.Delete(i);
        end;
    end;
end;

procedure TListHelper.FillListWithBigWidePics(FileList, TargetList: TStrings);
var
    i: integer;
    FullFileName: string;
begin
    if ((FileList <> nil) and (TargetList <> nil)) then
    begin
        TargetList.Clear;
        for i := 0 to FileList.Count - 1 do
        begin
            FullFileName := FileList[i];
            if (IsBigAndWide(FullFileName)) then
            begin
                TargetList.Append(ExtractFileName(FullFileName));
            end;
        end;
    end;
end;

function TListHelper.IsBigAndWide(FullFileName: string): boolean;
begin
    Result := ((FileHelper.GetFileSize(FullFileName) > 120000) and FileHelper.IsWide(FullFileName));
end;

function TListHelper.HasSelectedItem(ListBox: TListBox): boolean;
begin
    Result := False;
    if (ListBox <> nil) then
    begin
        Result := ListBox.ItemIndex > -1;
    end;
end;

function TListHelper.RescanPicturesFolder(Folder: string): TStrings;
begin
    Result := nil;
    if (DirectoryExists(Folder)) then
    begin
        Result := RemoveFilePaths(FindAllFiles(Folder, '*.*', False));
    end;
end;

constructor TListHelper.Create;
begin
    inherited;
    FileHelper := TFileHelper.Create();
end;

destructor TListHelper.Destroy;
begin
    FreeAndNil(FileHelper);
    inherited;
end;

function TListHelper.RemoveFilePaths(AList: TStrings): TStrings;
var
    i: integer;
begin
    Result := nil;
    if (AList <> nil) then
    begin
        Result := TStringList.Create();
        for i := 0 to AList.Count - 1 do
        begin
            Result.Add(ExtractFileName(AList.Strings[i]));
        end;
        FreeAndNil(AList);
    end;
end;

end.

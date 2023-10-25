unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls,
    FileUtil, Types, FileHelper, ListHelper, LCLType;

const
    MY_VERSION = 'Blickpunkt V1.3b ©2019-2023 by I.Steiniger';
    ASSETS_PATH = '\AppData\Local\Packages\Microsoft.Windows.ContentDeliveryManager_cw5n1h2txyewy\LocalState\Assets\';

type
    TMainForm = class(TForm)
        GrBo_SavedPictures: TGroupBox;
        GrBo_NewPictures: TGroupBox;
        NewPicturesSave: TMenuItem;
        SavedPicturesUpdate: TMenuItem;
        Separator1: TMenuItem;
        Preview: TImage;
        NewPictures: TListBox;
        SavedPictures: TListBox;
        NewPicturesSaveAs: TMenuItem;
        NewPicturesSetWallpaper: TMenuItem;
        SavedPicturesSaveAs: TMenuItem;
        SavedPicturesSetWallpaper: TMenuItem;
        SavedPicturesDelete: TMenuItem;
        PanelLeft: TPanel;
        PopMen_NewPictures: TPopupMenu;
        PopMen_SavedPictures: TPopupMenu;
        SaveDialog: TSaveDialog;
        PreviewSplitter: TSplitter;
        ListSplitter: TSplitter;
        StatusBar: TStatusBar;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure NewPicturesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure NewPicturesSaveClick(Sender: TObject);
        procedure SavedPicturesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure SavedPicturesUpdateClick(Sender: TObject);
        procedure NewPicturesClick(Sender: TObject);
        procedure NewPicturesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
        procedure NewPicturesDblClick(Sender: TObject);
        procedure SavedPicturesClick(Sender: TObject);
        procedure SavedPicturesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
        procedure SavedPicturesDblClick(Sender: TObject);
        procedure SavedPicturesDeleteClick(Sender: TObject);
        procedure SavedPicturesSaveAsClick(Sender: TObject);
        procedure NewPicturesSaveAsClick(Sender: TObject);
    private
        NewPicturesPath: string;
        SavedPicturesPath: string;
        IsProgramStart: boolean;
        ListHelper: TListHelper;
        FileHelper: TFileHelper;
        procedure FillNewPicturesList();
        procedure LoadConfig();
        procedure LoadSavedPicturesList();
        procedure PrepareFolderPaths();
        procedure DeleteSavedPicture(const PictureName: string);
        procedure RemoveFromSavedPicturesList(const PictureName: string);
        procedure SaveConfig();
        procedure SaveSavedPicturesList();
        procedure SaveNewPicture(PictureName: string);
        procedure SavePictureToDefaultPath(PictureName: string);
        procedure SetPictureOnTopOfSavedPicturesList(PictureName: string);
        procedure SavePictureAs(ListBox: TListBox);
        procedure RefreshMainFormCaption();
        procedure RefreshSavedPicturesCaption();
        procedure SetIsNewText(PictureName: string);
        function PictureIsAlreadySaved(PictureName: string): boolean;
    public
    end;

var
    MainForm: TMainForm;

implementation

{$R *.lfm}

uses configuration;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    IsProgramStart := True;
    ListHelper := TListHelper.Create();
    FileHelper := TFileHelper.Create();
    RefreshMainFormCaption();
    PrepareFolderPaths();
    LoadConfig();
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    FreeAndNil(ListHelper);
    FreeAndNil(FileHelper);
    SaveSavedPicturesList();
    SaveConfig();
    CloseAction := caFree;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
    if (IsProgramStart) then
    begin
        FillNewPicturesList();
        LoadSavedPicturesList();
        RefreshSavedPicturesCaption();
        SaveDialog.InitialDir := FileHelper.GetMyPicturesFolder();

        if (NewPictures.Count > 0) then
        begin
          NewPictures.Selected[0] := True;
          NewPictures.ItemIndex := 0;
          NewPicturesClick(Sender);
        end;

        IsProgramStart := False;
    end;
end;

procedure TMainForm.NewPicturesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
    if (key = VK_RETURN) then
    begin
        NewPicturesDblClick(Sender);
    end;
end;

procedure TMainForm.NewPicturesSaveClick(Sender: TObject);
var
    PictureName: string;
begin
    if (ListHelper.HasSelectedItem(NewPictures)) then
    begin
        PictureName := NewPictures.Items.Strings[NewPictures.ItemIndex];
        SaveNewPicture(PictureName);
    end;
end;

procedure TMainForm.SavedPicturesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
    if (key = VK_RETURN) then
    begin
        SavedPicturesDblClick(Sender);
    end;
end;

procedure TMainForm.SavedPicturesUpdateClick(Sender: TObject);
var
    FileList: TStrings;
begin
    FileList := ListHelper.RescanPicturesFolder(SavedPicturesPath);
    if (FileList <> nil) then
    begin
        ListHelper.UpdateList(SavedPictures.Items, FileList);
        FreeAndNil(FileList);
        RefreshSavedPicturesCaption();
    end;
end;

procedure TMainForm.NewPicturesClick(Sender: TObject);
var
    PictureName: string;
begin
    SavedPictures.ClearSelection();
    if (ListHelper.HasSelectedItem(NewPictures)) then
    begin
        PictureName := NewPictures.Items.Strings[NewPictures.ItemIndex];
        if (FileExists(NewPicturesPath + PictureName)) then
        begin
            Preview.Picture.LoadFromFile(NewPicturesPath + PictureName);
        end;

        SetIsNewText(PictureName);
    end;
end;

procedure TMainForm.NewPicturesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
var
    Enable: boolean;
begin
    Enable := ListHelper.HasSelectedItem(NewPictures);
    NewPicturesSaveAs.Enabled := Enable;
    NewPicturesSetWallpaper.Enabled := Enable;
end;

procedure TMainForm.NewPicturesDblClick(Sender: TObject);
var
    PictureName: string;
begin
    if (ListHelper.HasSelectedItem(NewPictures)) then
    begin
        PictureName := NewPictures.Items[NewPictures.ItemIndex];
        SaveNewPicture(PictureName);
        FileHelper.SetWallpaper(SavedPicturesPath + PictureName);
    end;
end;

procedure TMainForm.SavedPicturesClick(Sender: TObject);
var
    PictureName: string;
begin
    NewPictures.ClearSelection();
    if (ListHelper.HasSelectedItem(SavedPictures)) then
    begin
        PictureName := SavedPictures.Items[SavedPictures.ItemIndex];
        if (FileExists(SavedPicturesPath + PictureName)) then
        begin
            Preview.Picture.LoadFromFile(SavedPicturesPath + PictureName);
        end;
    end;
    StatusBar.SimpleText := '';
end;

procedure TMainForm.SavedPicturesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
var
    Enable: boolean;
begin
    Enable := ListHelper.HasSelectedItem(SavedPictures);
    SavedPicturesSaveAs.Enabled := Enable;
    SavedPicturesSetWallpaper.Enabled := Enable;
    SavedPicturesDelete.Enabled := Enable;
end;

procedure TMainForm.SavedPicturesDblClick(Sender: TObject);
var
    PictureName: string;
begin
    if (ListHelper.HasSelectedItem(SavedPictures)) then
    begin
        PictureName := SavedPictures.Items.Strings[SavedPictures.ItemIndex];

        RemoveFromSavedPicturesList(PictureName);
        SavedPictures.Items.Insert(0, PictureName);

        FileHelper.SetWallpaper(SavedPicturesPath + PictureName);
    end;
end;

procedure TMainForm.SavedPicturesDeleteClick(Sender: TObject);
var
    PictureName: string;
begin
    if (ListHelper.HasSelectedItem(SavedPictures)) then
    begin
        PictureName := SavedPictures.Items[SavedPictures.ItemIndex];
        DeleteSavedPicture(PictureName);
    end;
end;

procedure TMainForm.SavedPicturesSaveAsClick(Sender: TObject);
begin
    SavePictureAs(SavedPictures);
end;

procedure TMainForm.NewPicturesSaveAsClick(Sender: TObject);
begin
    SavePictureAs(NewPictures);
end;

procedure TMainForm.LoadConfig();
var
    Configuration: TConfiguration;
begin
    Configuration := TConfiguration.Create();
    Configuration.LoadForm(Self);
    FreeAndNil(Configuration);
end;

procedure TMainForm.LoadSavedPicturesList();
var
    PicList: TStrings;
    Configuration: TConfiguration;
begin
    Configuration := TConfiguration.Create();

    PicList := Configuration.LoadPicturesList();
    SavedPictures.Items.AddStrings(PicList);

    FreeAndNil(PicList);
    FreeAndNil(Configuration);
end;

procedure TMainForm.PrepareFolderPaths();
var
    ProfileDir: string;
begin
    ProfileDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('USERPROFILE'));
    NewPicturesPath := IncludeTrailingPathDelimiter(ProfileDir + ASSETS_PATH);
    SavedPicturesPath := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'Pictures' + PathDelim;
end;

procedure TMainForm.FillNewPicturesList();
var
    NewPicturesList: TStrings;
begin
    NewPicturesList := FindAllFiles(NewPicturesPath, '*.*', False);
    ListHelper.FillListWithBigWidePics(NewPicturesList, NewPictures.Items);
    FreeAndNil(NewPicturesList);
end;

procedure TMainForm.DeleteSavedPicture(const PictureName: string);
var
    Message: string;
begin
    Message := 'Wollen Sie das Bild "' + PictureName + '" wirklich löschen?';
    if (MessageDlg('Bild löschen', Message, mtWarning, [mbYes, mbNo], 0) = mrYes) then
    begin
        if (FileExists(SavedPicturesPath + PictureName)) then
        begin
            DeleteFile(SavedPicturesPath + PictureName);
        end;

        RemoveFromSavedPicturesList(PictureName);
        RefreshSavedPicturesCaption();
    end;
end;

procedure TMainForm.RemoveFromSavedPicturesList(const PictureName: string);
begin
    SavedPictures.Items.Delete(SavedPictures.Items.IndexOf(PictureName));
end;

procedure TMainForm.SaveConfig();
var
    Configuration: TConfiguration;
begin
    Configuration := TConfiguration.Create();
    Configuration.SaveForm(Self);
    FreeAndNil(Configuration);
end;

procedure TMainForm.SaveSavedPicturesList();
var
    Configuration: TConfiguration;
begin
    Configuration := TConfiguration.Create();
    Configuration.SavePicturesList(SavedPictures.Items);
    FreeAndNil(Configuration);
end;

procedure TMainForm.SaveNewPicture(PictureName: string);
begin
    if (PictureIsAlreadySaved(PictureName)) then
    begin
        RemoveFromSavedPicturesList(PictureName);
    end else
    begin
        SavePictureToDefaultPath(PictureName);
    end;

    SetPictureOnTopOfSavedPicturesList(PictureName);
    RefreshSavedPicturesCaption();
    SetIsNewText(PictureName);
end;

procedure TMainForm.SetPictureOnTopOfSavedPicturesList(PictureName: string);
begin
    SavedPictures.Items.Insert(0, PictureName);
end;

procedure TMainForm.SavePictureToDefaultPath(PictureName: string);
var
    Source, Target: string;
begin
    if (not PictureName.IsEmpty) then
    begin
        Source := NewPicturesPath + PictureName;
        Target := SavedPicturesPath + PictureName;
        if (ForceDirectories(SavedPicturesPath)) then
        begin
            CopyFile(Source, Target, []);
        end else
        begin
            MessageDlg('Blickpunkt', 'Fehler: Der Ordner "' + SavedPicturesPath +
                '" existiert nicht und konnte auch nicht angelegt werden.' + 'Bitte prüfen sie den Pfad.',
                mtError, [mbOK], 0);
        end;
    end;
end;

procedure TMainForm.SavePictureAs(ListBox: TListBox);
var
    PictureName, Source, Target: string;
begin
    if ((ListBox <> nil) and ListHelper.HasSelectedItem(ListBox)) then
    begin
        PictureName := ListBox.Items[ListBox.ItemIndex];
        SaveDialog.FileName := '';
        if (SaveDialog.Execute) then
        begin
            Source := NewPicturesPath + PictureName;
            Target := SaveDialog.FileName;
            CopyFile(Source, Target, []);
        end;
    end;
end;

procedure TMainForm.RefreshMainFormCaption();
begin
    Caption := MY_VERSION;
end;

procedure TMainForm.RefreshSavedPicturesCaption();
var
    NumberOfPics: integer;
begin
    NumberOfPics := SavedPictures.Items.Count;
    GrBo_SavedPictures.Caption := 'gespeicherte Bilder(' + IntToStr(NumberOfPics) + ')';
end;

procedure TMainForm.SetIsNewText(PictureName: string);
begin
    if (PictureIsAlreadySaved(PictureName)) then
    begin
        StatusBar.SimpleText := 'Dieses Bild ist bereits vorhanden.';
    end else
    begin
        StatusBar.SimpleText := 'Dieses Bild ist neu.';
    end;
end;

function TMainForm.PictureIsAlreadySaved(PictureName: string): boolean;
begin
    Result := SavedPictures.Items.IndexOf(PictureName) > -1;
end;

end.

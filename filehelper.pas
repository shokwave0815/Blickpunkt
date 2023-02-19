unit FileHelper;

{$mode ObjFPC}{$H+}

interface

uses
	   Registry, Windows, Classes, SysUtils, Graphics;

type
	   TFileHelper = class(TObject)
	   private
					   procedure WriteRegistryEntry(Filename: string);
					   procedure CallSetWallpaperWindowsAPI(Filename: string);
	   public
					   procedure SetWallpaper(Filename: string);
					   function GetMyPicturesFolder(): string;
					   function IsWide(Filename: string): boolean;
					   function GetFileSize(Filename: string): int64;
	   end;

implementation

procedure TFileHelper.SetWallpaper(Filename: string);
begin
	   WriteRegistryEntry(Filename);
	   CallSetWallpaperWindowsAPI(Filename);
end;

procedure TFileHelper.WriteRegistryEntry(Filename: string);
var
	   Registry: TRegistry;
begin
	   Registry := TRegistry.Create();
	   Registry.OpenKey('\Control Panel\desktop', False);
	   Registry.WriteString('Wallpaper', Filename);

{                 Tile   Center  Stretch  Fit  Fill
  WallpaperStyle   0       0       2      6     10
  TileWallpaper    1       0       0      0      0
}
	   Registry.WriteString('WallpaperStyle', '0');
	   Registry.WriteString('TileWallpaper', '0');

	   FreeAndNil(Registry);
end;

procedure TFileHelper.CallSetWallpaperWindowsAPI(Filename: string);
begin
	   SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Filename), SPIF_SENDCHANGE or SPIF_UPDATEINIFILE);
end;

function TFileHelper.IsWide(Filename: string): boolean;
var
	   Picture: TPicture;
begin
	   Picture := TPicture.Create();
	   Picture.LoadFromFile(Filename);
	   Result := Picture.Width > Picture.Height;
	   FreeAndNil(Picture);
end;

function TFileHelper.GetFileSize(Filename: string): int64;
var
	   ByteFile: file of byte;
begin
	   try
					   AssignFile(ByteFile, Filename);
					   Reset(ByteFile);
					   Result := FileSize(ByteFile);
	   finally
					   CloseFile(ByteFile);
	   end;
end;

function TFileHelper.GetMyPicturesFolder(): string;
var
	   Registry: TRegistry;
const
	   RegistryKey = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
begin
	   Registry := TRegistry.Create();
	   Registry.OpenKey(RegistryKey, False);
	   Result := Registry.ReadString('My Pictures');
	   FreeAndNil(Registry);
end;

end.
